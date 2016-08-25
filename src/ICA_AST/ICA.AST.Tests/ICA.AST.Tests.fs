module GS.ICA.AST.Tests

open NUnit.Framework
open GS.ICA.AST
open Microsoft.FSharp.Collections
open System

let astTest (ast:List<_>) expectRes =
    let t = GS.ICA.AST.translate ast.[0] []
    let r = 
        match t with 
        | T t -> t.PostAndReply(fun rc -> GS.ICA.AST.Q rc)    
    printfn "result=%A" r
    Assert.AreEqual(r, expectRes, "Incorrect result!")  

[<TestFixture>]
type ``ICA Tests`` () =            
    [<Test>]
    member this.``Test 1.`` () = 
        let ast = GS.ICA.Pars.Main.parse "new x in new y in ((x := 2); (y := (!x + 3)); if !x then x := (!x + 1) else x := (!y + !x + 3))"
        astTest ast 10

    [<Test>]
    member this.``Test 2.`` () = 
        let ast = GS.ICA.Pars.Main.parse "new x in ((x := 2); if !x then x := (!x + 1) else x := (!x + 3))"
        astTest ast 5

    [<Test>]
    member this.``Test 3.`` () = 
        let ast = GS.ICA.Pars.Main.parse "new x in ((x := 2);(x := (!x + 1));(!x))"
        astTest ast 3

    [<Test>]
    member this.``Test 4.`` () = 
        let ast = GS.ICA.Pars.Main.parse "(\ g . (\ f . new x in ((g(x))+(!(f(x))) + 1))(\ a . a ))(\ b . b := (!b + 2))"
        astTest ast 5

    [<Test>]
    member this.``Test 5.`` () = 
        let ast = GS.ICA.Pars.Main.parse "(\ g . (\ f . new x in (f(x) + (g(x))))(\ a . a := (!a + 1)))(\ b . b := (!b + 2))"
        astTest ast 4

    [<Test>]
    member this.``Test 6.`` () = 
        let ast = GS.ICA.Pars.Main.parse "(\ f . new x in (f(x) + (f(x))))(\ x . !x + 1)"
        astTest ast 2

    [<Test>]
    member this.``Test 7.`` () = 
        let ast = GS.ICA.Pars.Main.parse "new x in !x + 1"
        astTest ast 1

    [<Test>]
    member this.``Test 8.`` () = 
        let ast = GS.ICA.Pars.Main.parse "(\ x . x (2) + (x (8)))(\y.y + y)"
        astTest ast 20

    [<Test>]
    member this.``Test 9.`` () = 
        let ast = GS.ICA.Pars.Main.parse "(\ x . x (2) + 8)(\y.y + y)"
        astTest ast 12

    //[<Test>] It doesn't make sense now :)
    member this.``Test 10.`` () = 
        let ast = GS.ICA.Pars.Main.parse "ss(skip; skip) + skip; !skip + fix \x.(m || n + 8)"
        astTest ast 10
