// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let ast = GS.ICA.Pars.Main.parse "skip; skip + skip; skip"
    printfn "%A" ast
    0 // return an integer exit code
