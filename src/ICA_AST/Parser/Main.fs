module GS.ICA.Pars.Main

open GS.ICA.Parser
open GS.ICA.Lexer
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST


let parse str =
    let lb = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString str
    let tokens = seq { while not lb.IsPastEndOfStream do yield GS.ICA.Lexer.token lb}
    let ast = GS.ICA.Parser.buildAst tokens
    let args = {
        tokenToRange = fun _ -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    match ast with
    | Parser.Success(ast,_,errors) ->
        ast.ChooseSingleAst()
        let ast =  GS.ICA.Parser.translate args ast errors
        ast
    | _ -> failwith "Всё очень плохо!!!!!" 

