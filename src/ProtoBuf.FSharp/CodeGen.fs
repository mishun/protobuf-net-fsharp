module internal ProtoBuf.FSharp.CodeGen

open System
open FSharp.Reflection
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open MethodHelpers

let private emitZeroValueOntoEvaluationStack (gen: ILGenerator) (getterType: MethodType) =
    match getterType with
    | MethodType.MethodInfo mi  ->
        gen.EmitCall(OpCodes.Call, mi, null) 
    | MethodType.PropertyInfo pi -> 
        gen.EmitCall(OpCodes.Call, pi.GetMethod, null) 
    | MethodType.FieldInfo fi ->
        gen.Emit(OpCodes.Ldsfld, fi)
    | MethodType.NewArray t -> 
        gen.Emit(OpCodes.Ldc_I4_0) // Push length onto the stack.
        gen.Emit(OpCodes.Newarr, t) // Initialise array with length.

let private emitStackTopZeroCheck (gen : ILGenerator) (topType : Type) =
    ZeroValues.getZeroValueMethodInfoOpt topType |> Option.iter (fun getValue ->
        let skip = gen.DefineLabel()
        gen.Emit(OpCodes.Dup)
        gen.Emit(OpCodes.Brtrue, skip)
        gen.Emit(OpCodes.Pop)
        emitZeroValueOntoEvaluationStack gen getValue
        gen.MarkLabel(skip)
    )

let private emitFieldAssignments (gen: ILGenerator) (zeroValuesPerField: ZeroValues.FieldWithZeroValueSetMethod[]) =
    for zeroValueField in zeroValuesPerField do
        if zeroValueField.FieldInfo.IsStatic then
            emitZeroValueOntoEvaluationStack gen zeroValueField.ZeroValueMethod
            gen.Emit(OpCodes.Stsfld, zeroValueField.FieldInfo) // Assign to field
        else
            gen.Emit(OpCodes.Dup)
            emitZeroValueOntoEvaluationStack gen zeroValueField.ZeroValueMethod
            gen.Emit(OpCodes.Stfld, zeroValueField.FieldInfo)

let private emitRecordDefault (gen: ILGenerator) (recordType: Type) =
    for pi in FSharpType.GetRecordFields(recordType, true) do
        let propertyType = pi.PropertyType

        match ZeroValues.getZeroValueMethodInfoOpt propertyType with
        | Some getValueMethodInfo ->
            emitZeroValueOntoEvaluationStack gen getValueMethodInfo
        | _ when propertyType.IsValueType ->
            let cell = gen.DeclareLocal(propertyType)
            gen.Emit(OpCodes.Ldloca_S, cell)
            gen.Emit(OpCodes.Initobj, propertyType)
            gen.Emit(OpCodes.Ldloc, cell)
        | _ ->
            gen.Emit(OpCodes.Ldnull)

    let ctr = FSharpValue.PreComputeRecordConstructorInfo(recordType, true)
    gen.Emit(OpCodes.Newobj, ctr)

/// Emits a factory to create the object making sure all values are default assigned as expected for F# consumption (e.g. no nulls where not possible to define for common cases)
let private emitFactory (resultType : Type) (zeroValuesPerField: ZeroValues.FieldWithZeroValueSetMethod array) =
    let factoryMethod = DynamicMethod("factory_" + resultType.FullName, resultType, [| |], true)
    let gen = factoryMethod.GetILGenerator()

    match resultType.GetConstructor Array.empty with
    | null when FSharpType.IsRecord (resultType, true) -> // Is an F# record with a F# constructor.
        emitRecordDefault gen resultType
    | null -> // Is a type that isn't a record with no parameterless constructor. NOTE: This is significantly slower for deserialisation than alternative pathways.
        gen.Emit(OpCodes.Ldtoken, resultType)
        gen.EmitCall(OpCodes.Call, MethodHelpers.getMethodInfo <@ Runtime.Serialization.FormatterServices.GetUninitializedObject @> [||], null)
        emitFieldAssignments gen zeroValuesPerField
    | ctr -> // Has a parameterless constructor
        gen.Emit(OpCodes.Newobj, ctr)
        emitFieldAssignments gen zeroValuesPerField

    gen.Emit(OpCodes.Ret)
    factoryMethod :> MethodInfo


let surrogateTypeDeclaration (surrogateModule: ModuleBuilder) (targetType: Type) (useValueTypeSurrogate: bool) =
    let surrogateType =
        let name = sprintf "ProtoBuf.FSharp.Surrogates.Generated.%s" targetType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if useValueTypeSurrogate
        then surrogateModule.DefineType(name, attr, typeof<ValueType>)
        else surrogateModule.DefineType(name, attr)

    let constructor =
        if surrogateType.IsValueType
        then ValueNone
        else
            let ctr = surrogateType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
            let gen = ctr.GetILGenerator()
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
            gen.Emit(OpCodes.Ret)
            ValueSome ctr

    struct (surrogateType, constructor)

/// Emits a record surrogate. Intended to be used to support value type records ONLY since Protobuf-net at time of writing does not support custom ValueTypes/Structs.
let private emitRecordSurrogate (surrogateModule: ModuleBuilder) (recordType: Type) (useValueTypeSurrogate: bool) =
    let struct (surrogateType, constructor) = surrogateTypeDeclaration surrogateModule recordType useValueTypeSurrogate

    let surrogateFields = [|
        for fi in FSharpType.GetRecordFields(recordType, true) ->
            struct (fi, surrogateType.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public))
    |]

    let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static

    // Define op_Implicit methods that Protobuf calls to create recordType from surrogate.
    let conv = surrogateType.DefineMethod("op_Implicit", attr, recordType, [| surrogateType |])
    let gen = conv.GetILGenerator()
    for (recordField, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateField)
        emitStackTopZeroCheck gen recordField.PropertyType
    gen.Emit(OpCodes.Newobj, FSharpValue.PreComputeRecordConstructorInfo(recordType, true))
    gen.Emit(OpCodes.Ret)

    // Define op_Implicit methods that Protobuf calls to create surrogate from recordType.
    let conv = surrogateType.DefineMethod("op_Implicit", attr, surrogateType, [| recordType |])
    let gen = conv.GetILGenerator()

    let cell = gen.DeclareLocal(surrogateType)
    match constructor with
    | ValueSome ctr ->
        gen.Emit(OpCodes.Newobj, ctr)
        gen.Emit(OpCodes.Stloc, cell)
    | ValueNone ->
        gen.Emit(OpCodes.Ldloca_S, cell)
        gen.Emit(OpCodes.Initobj, surrogateType)

    let argIsNull = gen.DefineLabel()
    if not recordType.IsValueType
    then
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Brfalse, argIsNull)

    for (recordField, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldloca_S else OpCodes.Ldloc), cell)
        gen.Emit((if recordType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Call, recordField.GetMethod)
        gen.Emit(OpCodes.Stfld, surrogateField)

    gen.MarkLabel(argIsNull)
    gen.Emit(OpCodes.Ldloc, cell)
    gen.Emit(OpCodes.Ret)

    surrogateType.CreateTypeInfo ()


type private TypeBuilder with
    member tb.DefineOpExplicit(src : Type, dst : Type) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static
        tb.DefineMethod("op_Explicit", attr, dst, [| src |])

    member tb.SetProtoContractAttribute(useConstructor : bool) =
        let t = typeof<ProtoBuf.ProtoContractAttribute>
        CustomAttributeBuilder(
            t.GetConstructor [||], [||]
            , [| t.GetProperty "ImplicitFields" ; t.GetProperty "SkipConstructor" |]
            , [| box ProtoBuf.ImplicitFields.AllFields ; box (not useConstructor) |]
        ) |> tb.SetCustomAttribute


let private emitValueUnionSurrogate (surrogateModule: ModuleBuilder) (unionType: Type) (useValueTypeSurrogate: bool) =
    let struct (surrogateType, constructor) = surrogateTypeDeclaration surrogateModule unionType useValueTypeSurrogate
    surrogateType.SetProtoContractAttribute(false)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            struct (caseInfo, [|
                for fi in caseInfo.GetFields() ->
                    struct (fi, surrogateType.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public))
            |])
    |]

    let surrogateTagField =
        let rec chooseName name =
            match unionType.GetMember(name, BindingFlags.Public ||| BindingFlags.NonPublic) with
            | [||] -> name
            | _ -> chooseName ("_" + name)
        surrogateType.DefineField(chooseName "__tag", typeof<int>, FieldAttributes.Public)

    // Define op_Implicit methods that Protobuf calls to create unionType from surrogate.
    begin
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)

        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, surrogateFields) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            for (originField, surrogateField) in surrogateFields do
                gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Ldfld, surrogateField)
                emitStackTopZeroCheck gen originField.PropertyType
            gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
            gen.Emit(OpCodes.Ret)
    end

    // Define op_Implicit methods that Protobuf calls to create surrogate from unionType.
    begin
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let resultCell = gen.DeclareLocal(surrogateType)
        match constructor with
        | ValueSome ctr ->
            gen.Emit(OpCodes.Newobj, ctr)
            gen.Emit(OpCodes.Stloc, resultCell)
        | ValueNone ->
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit(OpCodes.Initobj, surrogateType)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)

        match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
        | :? PropertyInfo as tag ->
            gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
            gen.Emit(OpCodes.Call, tag.GetMethod)
        | :? MethodInfo as tag when tag.IsStatic ->
            gen.Emit(OpCodes.Call, tag)
        | smth -> failwithf "Unexpected tag member: %A" smth
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, surrogateFields) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            gen.Emit((if surrogateType.IsValueType then OpCodes.Ldloca_S else OpCodes.Ldloc), resultCell)
            for (originField, surrogateField) in surrogateFields do
                gen.Emit(OpCodes.Dup)
                gen.Emit(OpCodes.Ldarga_S, 0)
                gen.Emit(OpCodes.Call, originField.GetMethod)
                gen.Emit(OpCodes.Stfld, surrogateField)
            gen.Emit(OpCodes.Ldc_I4, caseInfo.Tag)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Ldloc, resultCell)
            gen.Emit(OpCodes.Ret)
    end

    surrogateType.CreateTypeInfo ()


let private emitUnionSurrogate' (surrogateModule: ModuleBuilder) (unionType: Type) =
    let surrogateType =
        let name = sprintf "ProtoBuf.FSharp.Surrogates.Generated.%s" unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    surrogateType.SetProtoContractAttribute(false)


    let caseBaseType =
        let attr = TypeAttributes.Public ||| TypeAttributes.NestedPublic ||| TypeAttributes.Serializable
        surrogateType.DefineNestedType("Case", attr, typeof<obj>)
    caseBaseType.SetProtoContractAttribute(false)

    let baseDefaultConstructor =
        let ctr = caseBaseType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
        let gen = ctr.GetILGenerator()
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
        gen.Emit(OpCodes.Ret)
        ctr

    let extractBaseMethod =
        let attr = MethodAttributes.Public ||| MethodAttributes.Virtual
        let extract = caseBaseType.DefineMethod("extract", attr, unionType, [| |])
        let gen = extract.GetILGenerator()
        gen.Emit(OpCodes.Newobj, typeof<System.NotImplementedException>.GetConstructor [||])
        gen.Emit(OpCodes.Throw)
        extract

    let surrogateItemField = surrogateType.DefineField("Item", caseBaseType, FieldAttributes.Public)

    let surrogateConstructor =
        let ctr = surrogateType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| caseBaseType |])
        let gen = ctr.GetILGenerator()
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Ldarg_1)
        gen.Emit(OpCodes.Stfld, surrogateItemField)
        gen.Emit(OpCodes.Ret)
        ctr

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            let subtype =
                let attr = TypeAttributes.Public ||| TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                surrogateType.DefineNestedType("Case" + caseInfo.Name, attr, caseBaseType)
            subtype.SetProtoContractAttribute(true)

            let fields = [| for fi in caseInfo.GetFields() -> subtype.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public) |]

            let constructor =
                let parameters = fields |> Array.map (fun f -> f.FieldType)
                let ctr = subtype.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, parameters)
                let gen = ctr.GetILGenerator()
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Call, baseDefaultConstructor)
                for (index, field) in Seq.indexed fields do
                    gen.Emit(OpCodes.Ldarg_0)
                    gen.Emit(OpCodes.Ldarg, 1 + index)
                    gen.Emit(OpCodes.Stfld, field)
                gen.Emit(OpCodes.Ret)
                ctr

            if Array.length fields > 0 then
                let ctr = subtype.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
                let gen = ctr.GetILGenerator()
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Call, baseDefaultConstructor)
                gen.Emit(OpCodes.Ret)

            begin
                let attr = MethodAttributes.Public ||| MethodAttributes.Virtual
                let extract = subtype.DefineMethod(extractBaseMethod.Name, attr, unionType, [| |])
                subtype.DefineMethodOverride(extract, extractBaseMethod)
                let gen = extract.GetILGenerator()
                for field in fields do
                    gen.Emit(OpCodes.Ldarg_0)
                    gen.Emit(OpCodes.Ldfld, field)
                    emitStackTopZeroCheck gen field.FieldType
                gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
                gen.Emit(OpCodes.Ret)
            end

            struct (caseInfo, subtype, constructor)
    |]

    // Define op_Implicit methods that Protobuf calls to create unionType from surrogate.
    let fromSurrogate =
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateItemField)
        gen.Emit(OpCodes.Callvirt, extractBaseMethod)
        gen.Emit(OpCodes.Ret)
        conv

    // Define op_Implicit methods that Protobuf calls to create surrogate from unionType.
    let toSurrogate =
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let argIsNull = gen.DefineLabel()
        if not unionType.IsValueType then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, argIsNull)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)

        match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
        | :? PropertyInfo as tag ->
            gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
            gen.Emit(OpCodes.Call, tag.GetMethod)
        | :? MethodInfo as tag when tag.IsStatic ->
            gen.Emit(OpCodes.Call, tag)
        | smth -> failwithf "Unexpected tag member: %A" smth
        gen.Emit(OpCodes.Switch, jumpTable)

        gen.MarkLabel(argIsNull)
        gen.Emit(OpCodes.Ldnull)
        gen.Emit(OpCodes.Newobj, surrogateConstructor)
        gen.Emit(OpCodes.Ret)

        for (caseInfo, _, caseConstructor) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            for originField in caseInfo.GetFields() do
                gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Call, originField.GetMethod)
            gen.Emit(OpCodes.Newobj, caseConstructor)
            gen.Emit(OpCodes.Newobj, surrogateConstructor)
            gen.Emit(OpCodes.Ret)

        conv

    for subtype in unionType.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) |> Seq.filter (fun sub -> sub.IsSubclassOf unionType) do
        let conv = surrogateType.DefineOpExplicit(surrogateType, subtype)
        let gen = conv.GetILGenerator()
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Call, fromSurrogate)
        gen.Emit(OpCodes.Castclass, subtype)
        gen.Emit(OpCodes.Ret)

        let conv = surrogateType.DefineOpExplicit(subtype, surrogateType)
        let gen = conv.GetILGenerator()
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Call, toSurrogate)
        gen.Emit(OpCodes.Ret)

    let surrogate = surrogateType.CreateTypeInfo ()
    let subsBase = caseBaseType.CreateTypeInfo()
    let subs = [|
        for (tag, (_, t, _)) in Seq.indexed cases ->
            //t.SetProtoIncludeAttribute(1000 + tag, subsBase)
            t.CreateTypeInfo()
    |]
    struct (surrogate, subsBase, subs)


[<RequireQualifiedAccess>]
type internal TypeConstructionStrategy =
    | NoCustomConstructor // Uses default Protobuf-net behaviour
    | CustomFactoryMethod of factoryMethod : MethodInfo
    | ObjectSurrogate of surrogateType : TypeInfo

let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"
let private metaInfoTypeCache = ConcurrentDictionary<Type, TypeConstructionStrategy>()

let getTypeConstructionMethod (typeToAdd : Type) (fields : FieldInfo[]) =
    let zeroValuesForFields = ZeroValues.calculateApplicableFields fields
    if Array.isEmpty zeroValuesForFields
    then TypeConstructionStrategy.NoCustomConstructor
    else
        metaInfoTypeCache.GetOrAdd(typeToAdd, fun _ ->
            if typeToAdd.IsValueType && FSharpType.IsRecord (typeToAdd, true) then
                emitRecordSurrogate surrogateModule typeToAdd typeToAdd.IsValueType |> TypeConstructionStrategy.ObjectSurrogate
            elif typeToAdd.IsValueType && FSharpType.IsUnion (typeToAdd, true) then
                emitValueUnionSurrogate surrogateModule typeToAdd true |> TypeConstructionStrategy.ObjectSurrogate
            else emitFactory typeToAdd zeroValuesForFields |> TypeConstructionStrategy.CustomFactoryMethod
        )


let private subCache = ConcurrentDictionary<Type, struct (TypeInfo * TypeInfo * TypeInfo[])>()

let getUnionSurrogate (unionType : Type) =
    subCache.GetOrAdd(unionType, fun _ ->
        if not unionType.IsValueType then
            emitUnionSurrogate' surrogateModule unionType
        else
            let t = emitValueUnionSurrogate surrogateModule unionType true
            struct (t, null, [| |])
    )
