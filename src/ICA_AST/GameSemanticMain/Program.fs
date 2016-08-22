// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let ast = GS.ICA.Pars.Main.parse "ss(skip; skip) + skip; !skip + \x.(m || n + 8)"
    printfn "%A" ast
    0 // return an integer exit code
