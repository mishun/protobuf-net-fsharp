namespace ProtoBuf.FSharp.Unit

open Expecto
open AutoTestTypes

module TestRecordRoundtrip = 
    let manualTestCases = [
        testCase "Can serialise empty array, string and option" <| fun () -> RunAutoTest.testRoundtrip [||] { One = ""; Two = 1; Three = [||]; Four = None } 
        testCase "Can serialise option containing value" <| fun () -> RunAutoTest.testRoundtrip [||] { One = ""; Two = 1; Three = [||]; Four = Some "TEST" }
        testCase "Can serialise string, array and option containing value" <| fun () -> RunAutoTest.testRoundtrip [||] { One = "TEST"; Two = 1; Three = [| "TEST1" |]; Four = Some "TEST" }
    ]

    [<Tests>]
    let test() = 
        testList 
            "Record Test Cases" 
            [ yield! manualTestCases
            ]
