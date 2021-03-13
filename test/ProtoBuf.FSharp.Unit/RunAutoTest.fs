module ProtoBuf.FSharp.Unit.RunAutoTest

open System
open System.IO
open Expecto
open Expecto.Expect
open FsCheck
open Microsoft.FSharp.Quotations.Patterns
open ProtoBuf.FSharp
open ProtoBuf.Meta


// F# does not allow nulls although FsCheck tries to stress C# interoperability.
// Disabling it here because this library is for wrapping F# types only.
type DataGenerator =
    static member Generate() : Arbitrary<string[]> = 
        Gen.oneof ([ "One"; "Two"; "" ] |> List.map Gen.constant) 
        |> Gen.listOf
        |> Gen.map List.toArray
        |> Arb.fromGen
        
    static member GenerateNonNullString() : Arbitrary<string> =
        Arb.Default.StringWithoutNullChars().Generator
            |> Gen.map (fun x -> x.Get)
            |> Gen.filter (box >> Operators.isNull >> not)
            |> Arb.fromGen


let testRoundtrip<'t when 't : equality> (otherDependentRecordTypes : Type[]) (typeToTest: 't) = 
    let model = RuntimeTypeModel.Create() |> Serialiser.registerTypeIntoModel<'t>
    for dependentRecordType in otherDependentRecordTypes do
        Serialiser.registerRuntimeTypeIntoModel dependentRecordType model |> ignore
    model.CompileInPlace()

    let cloned = model.DeepClone(typeToTest)
    equal (unbox cloned) (typeToTest) "Protobuf deep clone"
    use ms = new MemoryStream()
    Serialiser.serialise model ms typeToTest
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let rtData = Serialiser.deserialise<'t> model ms
    equal rtData typeToTest "Type not equal"


let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<DataGenerator> ] }


let buildTest<'t when 't : equality> (name, typesToRegister) =
    testCase name <| fun () -> Check.One(config, testRoundtrip<'t> typesToRegister)


[<Tests>]
let test() =
    let moduleName = "ProtoBuf.FSharp.Unit." + nameof AutoTestTypes
    testList "Auto tests" [
        let root = Type.GetType moduleName
        for rawType in root.GetNestedTypes() do
            for boxed in rawType.GetCustomAttributes(typeof<TestNameAttribute>, true) do
                let attr : TestNameAttribute = unbox boxed
                let tp =
                    if rawType.IsGenericType then
                        rawType.MakeGenericType attr.GenericParams
                     else rawType
                let name =
                    match attr.Name with
                    | null -> sprintf "Roundtrip %s" tp.FullName
                    | name -> name

                match <@ buildTest ("", [||]) @> with
                | Call (_, method, _) ->
                    yield
                        method
                            .GetGenericMethodDefinition()
                            .MakeGenericMethod(tp)
                            .CreateDelegate<Func<string, Type[], Test>>()
                            .Invoke(name, attr.DependentTypeParamters)
                | _ -> ()
    ]
