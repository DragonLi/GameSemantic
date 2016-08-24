// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open GS.ICA.AST

[<EntryPoint>]
let main argv = 
    let ast = GS.ICA.Pars.Main.parse "new x in new y in ((x := 2); (y := (!x + 3)); if !x then x := (!x + 1) else x := (!y + !x + 3))"
    printfn "%A" ast
    let t = GS.ICA.AST.translate ast.[0] []
    let r = 
        match t with 
        | T t -> t.PostAndReply(fun rc -> GS.ICA.AST.Q rc)    
    printfn "result=%A" r
    0 // return an integer exit code
