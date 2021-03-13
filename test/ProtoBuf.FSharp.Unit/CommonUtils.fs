namespace ProtoBuf.FSharp.Unit

open System

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = true)>]
type TestNameAttribute(name: string, dependentTypes: Type array) = 
    inherit Attribute()
    new (name: string) = TestNameAttribute(name, [||])
    member __.Name = name
    member __.DependentTypeParamters = dependentTypes

    member val GenericParams : Type[] = [||] with get, set
