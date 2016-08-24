// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open GS.ICA.AST

[<EntryPoint>]
let main argv = 
    //let ast = GS.ICA.Pars.Main.parse "ss(skip; skip) + skip; !skip + fix \x.(m || n + 8)"
    //let ast = GS.ICA.Pars.Main.parse "(\ x . x (2) + 8)(\y.y + y)"
    //let ast = GS.ICA.Pars.Main.parse "(\ x . x (2) + (x (8)))(\y.y + y)"
    //let ast = GS.ICA.Pars.Main.parse "new x in !x + 1"
    //let ast = GS.ICA.Pars.Main.parse "(\ f . new x in (f(x) + (f(x))))(\ x . !x + 1)"
    //let ast = GS.ICA.Pars.Main.parse "(\ g . (\ f . new x in (f(x) + (g(x))))(\ a . a := (!a + 1)))(\ b . b := (!b + 2))"
    //let ast = GS.ICA.Pars.Main.parse "(\ g . (\ f . new x in ((g(x))+(!(f(x))) + 1))(\ a . a ))(\ b . b := (!b + 2))"
    let ast = GS.ICA.Pars.Main.parse "new x in (((x := 2);(x := (!x + 1)));(!x))"
    printfn "%A" ast
    let t = GS.ICA.AST.translate ast.[0] []
    let r = 
        match t with 
        | T t -> t.PostAndReply(fun rc -> GS.ICA.AST.Q rc)    
    printfn "result=%A" r
    0 // return an integer exit code
