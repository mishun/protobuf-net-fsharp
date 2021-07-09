module internal ProtoBuf.FSharp.CodeGen

open System
open FSharp.Reflection
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open MethodHelpers


type private TypeBuilder with
    member tb.DefineOpExplicit(src : Type, dst : Type) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static
        tb.DefineMethod("op_Explicit", attr, dst, [| src |])

    member tb.SetProtoContractAttribute(skipConstructor : bool) =
        let t = typeof<ProtoBuf.ProtoContractAttribute>
        CustomAttributeBuilder(
            t.GetConstructor [||], [||]
            , [| t.GetProperty "ImplicitFields" ; t.GetProperty "SkipConstructor" |]
            , [| box ProtoBuf.ImplicitFields.AllFields ; box skipConstructor |]
        ) |> tb.SetCustomAttribute

    member tb.DefineFieldForProtobuf(fi : PropertyInfo) =
        tb.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public) // Do something with name and attributes?

type private FieldBuilder with
    member fb.SetProtoMemberAttribute(tag : int) =
        let t = typeof<ProtoBuf.ProtoMemberAttribute>
        CustomAttributeBuilder(t.GetConstructor [| typeof<int> |], [| box tag |]) |> fb.SetCustomAttribute


let private emitDefaultValueViaCell (gen : ILGenerator) (tp : Type) =
    let cell = gen.DeclareLocal(tp)
    gen.Emit(OpCodes.Ldloca_S, cell)
    gen.Emit(OpCodes.Initobj, tp)
    gen.Emit(OpCodes.Ldloc, cell)

let private emitZeroValueOntoEvaluationStack (gen: ILGenerator) (getterType: MethodType) =
    match getterType with
    | MethodType.MethodInfo mi ->
        gen.EmitCall(OpCodes.Call, mi, null) 
    | MethodType.PropertyInfo pi ->
        gen.EmitCall(OpCodes.Call, pi.GetMethod, null) 
    | MethodType.FieldInfo fi ->
        gen.Emit(OpCodes.Ldsfld, fi)
    | MethodType.NewArray elementType ->
        gen.Emit(OpCodes.Ldc_I4_0) // Push length onto the stack.
        gen.Emit(OpCodes.Newarr, elementType) // Initialise array with length.

let private emitStackTopZeroCheck (gen : ILGenerator) (topType : Type) =
    if topType.IsGenericParameter then
        let skipZeroCheck = gen.DefineLabel()
        gen.Emit(OpCodes.Ldtoken, topType)
        gen.Emit(OpCodes.Call, MethodHelpers.getMethodInfo <@ ZeroValues.isApplicableTo @> [| |])
        gen.Emit(OpCodes.Brfalse, skipZeroCheck)
        gen.Emit(OpCodes.Dup)
        gen.Emit(OpCodes.Brtrue, skipZeroCheck)
        gen.Emit(OpCodes.Pop)
        gen.Emit(OpCodes.Call, MethodHelpers.getMethodInfo <@ ZeroValues.getZeroValue @> [| topType |])
        gen.MarkLabel(skipZeroCheck)
    else
        ZeroValues.getZeroValueMethodInfoOpt topType |> Option.iter (fun getValue ->
            let skipZeroCheck = gen.DefineLabel()
            gen.Emit(OpCodes.Dup)
            gen.Emit(OpCodes.Brtrue, skipZeroCheck)
            gen.Emit(OpCodes.Pop)
            emitZeroValueOntoEvaluationStack gen getValue
            gen.MarkLabel(skipZeroCheck)
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
            emitDefaultValueViaCell gen propertyType
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


let private getGenericArgs (t : Type) =
    if t.IsGenericTypeDefinition then
        t.GetGenericArguments() |> ValueSome
    else ValueNone

let private defineGenericArgs (args : ValueOption<Type[]>) (tb : TypeBuilder) =
    args |> ValueOption.iter (fun args -> tb.DefineGenericParameters [| for arg in args -> arg.Name |] |> ignore)

let private substituteGenericArgs args (t : Type) =
    match args with
    | ValueNone -> t
    | ValueSome args -> t.MakeGenericType args

let private emitSurrogateContent (tb : TypeBuilder) (targetType : Type) (targetFields : PropertyInfo[]) (targetGenerate : MethodBase) (isVirtual : bool) (baseConstructor : ConstructorInfo) =
    let fields = [| for fi in targetFields -> struct (fi, tb.DefineFieldForProtobuf(fi)) |]
    let constructor =
        let paramType = if targetType.IsValueType then targetType.MakeByRefType() else targetType
        tb.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| paramType |])
    begin
        let gen = constructor.GetILGenerator()
        if not tb.IsValueType then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Call, match baseConstructor with | null -> typeof<obj>.GetConstructor [||] | ctr -> ctr)
        for (originField, surrogateField) in fields do
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, originField.GetMethod)
            gen.Emit(OpCodes.Stfld, surrogateField)
        gen.Emit(OpCodes.Ret)
    end
    let extractMethod =
        let attr = if isVirtual then MethodAttributes.Public ||| MethodAttributes.Virtual else MethodAttributes.Public
        tb.DefineMethod("Extract", attr, targetType, [| |])
    begin
        let gen = extractMethod.GetILGenerator()
        for (_, surrogateField) in fields do
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldfld, surrogateField)
            emitStackTopZeroCheck gen surrogateField.FieldType
        match targetGenerate with
        | :? ConstructorInfo as ctr -> gen.Emit(OpCodes.Newobj, ctr)
        | :? MethodInfo as method -> gen.Emit(OpCodes.Call, method)
        | smth -> failwithf "Expected constructor or static method, but got %A" smth
        gen.Emit(OpCodes.Ret)
    end
    struct (constructor, extractMethod)

let private surrogatePrefix = "ProtoBuf.FSharp.Surrogates.Generated"

/// Emits a record surrogate. Intended to be used to support value type records ONLY since Protobuf-net at time of writing does not support custom ValueTypes/Structs.
let private emitRecordSurrogate (surrogateModule: ModuleBuilder) (recordType: Type) (useValueTypeSurrogate: bool) =
    let genericArgs = getGenericArgs recordType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix recordType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if useValueTypeSurrogate
        then surrogateModule.DefineType(name, attr, typeof<ValueType>)
        else surrogateModule.DefineType(name, attr)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(useValueTypeSurrogate)
    let defaultConstructor =
        if surrogateType.IsValueType
        then ValueNone
        else surrogateType.DefineDefaultConstructor MethodAttributes.Public |> ValueSome

    let struct (constructor, extractMethod) =
        emitSurrogateContent surrogateType recordType
            (FSharpType.GetRecordFields(recordType, true))
            (FSharpValue.PreComputeRecordConstructorInfo(recordType, true))
            false null

    // Define op_Explicit methods that Protobuf calls to create recordType from surrogate.
    let conv = surrogateType.DefineOpExplicit(surrogateType, recordType)
    let gen = conv.GetILGenerator()
    gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
    gen.Emit(OpCodes.Call, extractMethod)
    gen.Emit(OpCodes.Ret)

    // Define op_Explicit methods that Protobuf calls to create surrogate from recordType.
    let conv = surrogateType.DefineOpExplicit(recordType, surrogateType)
    let gen = conv.GetILGenerator()
    let argIsNotNull = gen.DefineLabel()
    if not recordType.IsValueType then
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Brtrue, argIsNotNull)
        match defaultConstructor with
        | ValueSome ctr -> gen.Emit(OpCodes.Newobj, ctr)
        | ValueNone -> emitDefaultValueViaCell gen surrogateType
        gen.Emit(OpCodes.Ret)
    gen.MarkLabel(argIsNotNull)
    gen.Emit((if recordType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
    gen.Emit(OpCodes.Newobj, constructor)
    gen.Emit(OpCodes.Ret)

    surrogateType.CreateTypeInfo()

let private emitGetUnionTag (gen : ILGenerator) (unionType: Type) =
    match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
    | :? PropertyInfo as tag ->
        gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Call, tag.GetMethod)
    | :? MethodInfo as tag when tag.IsStatic ->
        gen.Emit(OpCodes.Call, tag)
    | smth -> failwithf "Unexpected tag member: %A" smth

let relevantUnionSubtypes (unionType: Type) = seq {
    for tt in unionType.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) do
        let subtype =
            if unionType.IsGenericType && tt.IsGenericTypeDefinition
                then tt.MakeGenericType(unionType.GetGenericArguments())
                else tt
        if subtype.IsSubclassOf unionType then
            yield subtype
}

let private emitUnionSurrogateWithTag (surrogateModule: ModuleBuilder) (unionType: Type) =
    let genericArgs = getGenericArgs unionType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(false)

    let unionCases = FSharpType.GetUnionCases(unionType, true)

    let tagEnum = surrogateType.DefineNestedType("TagsForCases", TypeAttributes.NestedPublic ||| TypeAttributes.Sealed, typeof<Enum>, null)
    tagEnum.DefineField("value__", typeof<int>, FieldAttributes.Private ||| FieldAttributes.SpecialName) |> ignore
    for caseInfo in unionCases do
        tagEnum.DefineField(caseInfo.Name, tagEnum, FieldAttributes.Public ||| FieldAttributes.Literal ||| FieldAttributes.Static).SetConstant(caseInfo.Tag)
    tagEnum.CreateTypeInfo() |> ignore

    let surrogateTagField = surrogateType.DefineField("Tag", tagEnum, FieldAttributes.Public)
    surrogateTagField.SetProtoMemberAttribute(1)

    let cases = [|
        for caseInfo in unionCases ->
            let caseData =
                match caseInfo.GetFields() with
                | [||] -> ValueNone
                | caseFields ->
                    let subtype =
                        let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                        surrogateType.DefineNestedType("Case" + caseInfo.Name, attr)
                    defineGenericArgs genericArgs subtype
                    subtype.SetProtoContractAttribute(false)
                    subtype.DefineDefaultConstructor MethodAttributes.Public |> ignore
                    let struct (constructor, extractMethod) =
                        emitSurrogateContent subtype unionType caseFields (FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true)) false null
                    subtype.CreateTypeInfo() |> ignore

                    let caseDataField = surrogateType.DefineField("Data" + caseInfo.Name, subtype, FieldAttributes.Public)
                    caseDataField.SetProtoMemberAttribute(2 + caseInfo.Tag)
                    struct (caseDataField, constructor, extractMethod) |> ValueSome
            struct (caseInfo, caseData)
    |]

    let fromSurrogate =
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, caseStructure) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            match caseStructure with
            | ValueSome struct (caseDataField, _, extractMethod) ->
                gen.Emit(OpCodes.Ldarga_S, 0)
                gen.Emit(OpCodes.Ldfld, caseDataField)
                gen.Emit(OpCodes.Call, extractMethod)
            | ValueNone ->
                gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
            gen.Emit(OpCodes.Ret)
        conv

    let toSurrogate =
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let resultCell = gen.DeclareLocal(surrogateType)
        gen.Emit(OpCodes.Ldloca_S, resultCell)
        gen.Emit(OpCodes.Initobj, surrogateType)

        let endLabel = gen.DefineLabel()
        if not unionType.IsValueType then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, endLabel)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable)
        gen.Emit(OpCodes.Br, endLabel)

        for (caseInfo, caseStructure) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            caseStructure |> ValueOption.iter (fun struct (caseDataField, caseConstructor, _) ->
                gen.Emit(OpCodes.Ldloca_S, resultCell)
                gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Newobj, caseConstructor)
                gen.Emit(OpCodes.Stfld, caseDataField)
            )
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit(OpCodes.Ldc_I4, caseInfo.Tag)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Br, endLabel)

        gen.MarkLabel(endLabel)
        gen.Emit(OpCodes.Ldloc, resultCell)
        gen.Emit(OpCodes.Ret)
        conv

    for subtype in relevantUnionSubtypes unionType do
        surrogateType
            .DefineOpExplicit(surrogateType, subtype)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, fromSurrogate)

        surrogateType
            .DefineOpExplicit(subtype, surrogateType)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, toSurrogate)

    surrogateType.CreateTypeInfo ()

let private emitUnionSurrogateWithSubtypes (surrogateModule: ModuleBuilder) (unionType: Type) =
    let genericArgs = getGenericArgs unionType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(false)

    let caseBaseType =
        let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Abstract ||| TypeAttributes.Serializable
        surrogateType.DefineNestedType("Base", attr)
    defineGenericArgs genericArgs caseBaseType
    caseBaseType.SetProtoContractAttribute(false)
    let baseDefaultConstructor = caseBaseType.DefineDefaultConstructor MethodAttributes.Public
    let extractBaseMethod =
        let attr = MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.Abstract
        caseBaseType.DefineMethod("Extract", attr, unionType, [| |])
    caseBaseType.CreateTypeInfo() |> ignore

    let surrogateTagField = surrogateType.DefineField("Tag", caseBaseType, FieldAttributes.Public)
    surrogateTagField.SetProtoMemberAttribute(1)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            let subtype =
                let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                surrogateType.DefineNestedType("Case" + caseInfo.Name, attr, caseBaseType)
            defineGenericArgs genericArgs subtype
            subtype.SetProtoContractAttribute(false)
            let struct (constructor, extractMethod) =
                emitSurrogateContent subtype unionType
                    (caseInfo.GetFields())
                    (FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
                    true baseDefaultConstructor
            subtype.DefineMethodOverride(extractMethod, if genericArgs.IsSome then TypeBuilder.GetMethod(caseBaseType, extractBaseMethod) else extractBaseMethod :> _)
            begin // DefineDefaultConstructor doesn't work here
                let ctr = subtype.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
                let gen = ctr.GetILGenerator()
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Call, baseDefaultConstructor)
                gen.Emit(OpCodes.Ret)
            end
            struct (caseInfo, subtype, constructor)
    |]

    let fromSurrogate =
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Callvirt, extractBaseMethod)
        gen.Emit(OpCodes.Ret)
        conv

    let toSurrogate =
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let resultCell = gen.DeclareLocal(surrogateType)
        gen.Emit(OpCodes.Ldloca_S, resultCell)
        gen.Emit(OpCodes.Initobj, surrogateType)

        let endLabel = gen.DefineLabel()
        if not unionType.IsValueType then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, endLabel)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable)
        gen.Emit(OpCodes.Br, endLabel)

        for (caseInfo, _, caseConstructor) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
            gen.Emit(OpCodes.Newobj, caseConstructor)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Br, endLabel)

        gen.MarkLabel(endLabel)
        gen.Emit(OpCodes.Ldloc, resultCell)
        gen.Emit(OpCodes.Ret)
        conv

    for subtype in relevantUnionSubtypes unionType do
        surrogateType
            .DefineOpExplicit(surrogateType, subtype)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, fromSurrogate)

        surrogateType
            .DefineOpExplicit(subtype, surrogateType)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, toSurrogate)

    begin
        let method = surrogateType.DefineMethod("RegisterIntoModel", MethodAttributes.Public ||| MethodAttributes.Static, null, [| typeof<ProtoBuf.Meta.RuntimeTypeModel> |])
        let gen = method.GetILGenerator()
        let metaTypeCell = gen.DeclareLocal(typeof<ProtoBuf.Meta.MetaType>)
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Ldtoken, substituteGenericArgs genericArgs caseBaseType)
        gen.Emit(OpCodes.Ldc_I4_1)
        gen.Emit(OpCodes.Call, typeof<ProtoBuf.Meta.RuntimeTypeModel>.GetMethod("Add"))
        gen.Emit(OpCodes.Stloc, metaTypeCell)
        for (caseInfo, subclass, _) in cases do
            gen.Emit(OpCodes.Ldloc, metaTypeCell)
            gen.Emit(OpCodes.Ldc_I4, 1000 + caseInfo.Tag)
            gen.Emit(OpCodes.Ldtoken, substituteGenericArgs genericArgs subclass)
            gen.Emit(OpCodes.Call, typeof<ProtoBuf.Meta.MetaType>.GetMethod("AddSubType", [| typeof<int> ; typeof<Type> |]))
            gen.Emit(OpCodes.Pop)
        gen.Emit(OpCodes.Ret)
    end

    let surrogate = surrogateType.CreateTypeInfo ()
    for (_, sub, _) in cases do
        sub.CreateTypeInfo() |> ignore
    surrogate


let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"
let private surrogateCache = ConcurrentDictionary<Type, Lazy<TypeInfo>>()

let private makeSurrogate (typeToAdd : Type) =
    match typeToAdd with
    | t when FSharpType.IsUnion(t, true) ->
        if t.IsValueType then
            lazy (emitUnionSurrogateWithTag surrogateModule typeToAdd)
        else
            lazy (emitUnionSurrogateWithSubtypes surrogateModule typeToAdd)
    | t when FSharpType.IsRecord(t, true) ->
        lazy emitRecordSurrogate surrogateModule typeToAdd true
    | t ->
        failwithf "No surrogate construction method for type %A" t

let getSurrogate (typeToAdd : Type) =
    if typeToAdd.IsGenericType then
        let surrogateDef = surrogateCache.GetOrAdd(typeToAdd.GetGenericTypeDefinition(), makeSurrogate).Value
        surrogateDef.MakeGenericType(typeToAdd.GetGenericArguments())
    else
        surrogateCache.GetOrAdd(typeToAdd, makeSurrogate).Value :> Type


[<RequireQualifiedAccess>]
type internal TypeConstructionStrategy =
    | NoCustomConstructor // Uses default Protobuf-net behaviour
    | CustomFactoryMethod of factoryMethod : MethodInfo
    | ObjectSurrogate

let private factoryCache = ConcurrentDictionary<Type, Lazy<MethodInfo>>()

let getTypeConstructionMethod (typeToAdd : Type) (fields : FieldInfo[]) =
    match ZeroValues.calculateApplicableFields fields with
    | [| |] -> TypeConstructionStrategy.NoCustomConstructor
    | zeroValuesForFields ->
        match typeToAdd with
        | t when t.IsValueType && FSharpType.IsRecord(t, true) ->
            TypeConstructionStrategy.ObjectSurrogate
        | t when t.IsValueType && FSharpType.IsUnion(t, true) ->
            TypeConstructionStrategy.ObjectSurrogate
        | _ ->
            let factory = factoryCache.GetOrAdd(typeToAdd, fun _ -> lazy emitFactory typeToAdd zeroValuesForFields).Value
            TypeConstructionStrategy.CustomFactoryMethod factory
