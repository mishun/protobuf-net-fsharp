module ProtoBuf.FSharp.Unit.AutoTestTypes


[<RequireQualifiedAccess; TestName("Single case DU")>]
type UnionOne = | One

[<RequireQualifiedAccess; TestName("Multi case DU, No Fields")>]
type UnionTwo = | One | Two

[<RequireQualifiedAccess; TestName("One has string")>]
type UnionThree = | One | Two of string

[<RequireQualifiedAccess; TestName("All have fields")>]
type UnionFour = | One of int | Two of string

[<RequireQualifiedAccess; TestName("More than one field per case")>]
type UnionFive = | One of int | Two of test1: string * test2: int

[<RequireQualifiedAccess; TestName("More than one field per case; has array type")>]
type UnionSix = | One of int | Two of test1: string * test2: int array

[<RequireQualifiedAccess; TestName("More than one field per case; has array type and option type")>]
type UnionSeven = | One of int option | Two of test1: int option * test2: int array

[<RequireQualifiedAccess; TestName("Single case union with data")>]
type UnionEight = | One of int option * two: int array

[<TestName("Union with generic<int>; two cases", GenericParams = [| typeof<int> |])>]
[<TestName("Union with generic<string>; two cases", GenericParams = [| typeof<string> |])>]
type SerialisableOption<'t> = 
    | SerialisableSome of 't
    | SerialisableNone

[<TestName("Union with generic; single case union", GenericParams = [| typeof<string> |])>]
type Wrapper<'t> = | Wrapper of 't

[<TestName("More than 4 cases; one case with no fields")>]
type UnionNine = 
    | CaseOne of numbers: int array // If any of the above show it.
    | CaseTwo of strings: string array
    | CaseThreee of singleData: string
    | CaseFour


[<TestName("Standard record with a simple C# list")>]
type TestRecordOne = 
    {
        One: string
        Two: int
        Three: string[]
        Four: string option
    }

[<TestName("All optional fields")>]
type TestRecordTwo = { TwoOne: string option; TwoTwo: int option }

[<TestName("Record CLIMutable type"); CLIMutable>]
type TestRecordThree = { Three: string; Four: int }

[<TestName("Record with mutable field")>]
type TestRecordFour = {
    mutable Flag : bool
    String : string
}


[<Struct; TestName("Struct Record")>]
type TestRecordFive = {
    Flag : bool
    String : string
}

type TestEnum =
    | OptionA = 1uy
    | OptionB = 2uy
    | OptionC = 69uy

[<TestName("Internal Record")>]
type TestRecordSix = internal {
    Field : struct (int * bool * int64)
    Number : int64
    DecimalNumber : decimal
    EnumField : TestEnum
    String : string
    mutable Date : System.DateTime
}

[<Struct; TestName("Struct Record with weird field names", [| typeof<SerialisableOption<string>> |], GenericParams = [| typeof<string> |])>]
type TestRecordSeven<'t> = {
    ``__$uperF!eld__`` : bool
    ``String#`` : string
    mutable Stuff : SerialisableOption<'t>[]
}

type InnerNestedRecordWithCollections = {
    ArrayData: int array
    StringData: string
}

[<TestName("Struct nested record with arrays and strings", [| typeof<InnerNestedRecordWithCollections> |])>]
type NestedRecordWithZeroValues = {
    NestedCollectedData: InnerNestedRecordWithCollections array
    NestedData: InnerNestedRecordWithCollections
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with primitive collections")>]
type StructRecordWithCollectionTestCases = {
    TextCollection: string array
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with inner complex types", [| typeof<InnerNestedRecordWithCollections> |])>]
type StructRecordWithNestedTypes = {
    DataCollection: InnerNestedRecordWithCollections array
    Data: InnerNestedRecordWithCollections
}
