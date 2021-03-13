namespace ProtoBuf.FSharp.Unit

open Expecto
open Expecto.Expect
open ProtoBuf.FSharp
open ProtoBuf.Meta

module TestUnionRoundtrip =
    // This test is just to show how the schema will be look like for other consumers. It is expected to fail so isn't used normally.
    let manualTest = 
        testCase 
            "Generate schema" 
            (fun () ->  
                let model = RuntimeTypeModel.Create() |> Serialiser.registerUnionIntoModel<AutoTestTypes.UnionNine>
                model.CompileInPlace()
                let schema = model.GetSchema(typeof<AutoTestTypes.UnionNine>)
                equal schema "" "Schema generated")

    [<Tests>]
    let test() =
        testList 
            "Union Test Cases" 
            [ //manualTest
            ]
