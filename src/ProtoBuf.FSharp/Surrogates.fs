namespace ProtoBuf.FSharp.Surrogates

open ProtoBuf
open ProtoBuf.Meta

[<Struct; CLIMutable; ProtoContract>]
type Optional<'t> = {
    [<ProtoMember(1)>] HasValue: bool
    [<ProtoMember(2)>] Item: 't
} with
    static member op_Implicit (w: Optional<'t>) : 't option =
        if w.HasValue then Some w.Item else None

    static member op_Implicit (o: 't option) =
        match o with
        | Some(o) -> { Item = o; HasValue = true }
        | None -> { HasValue = false; Item = Unchecked.defaultof<_> }

    static member RegisterIntoModel (model : RuntimeTypeModel) =
        let surrogateModelType = model.Add(typeof<Optional<'t>>, true)
        surrogateModelType.Name <- "Optional" + typeof<'t>.Name
