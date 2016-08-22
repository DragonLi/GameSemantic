
# 2 "ICA.Parser.fs"
module GS.ICA.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

# 1 "ICA.yrd"

open GS.ICA.AST

# 14 "ICA.Parser.fs"
type Token =
    | ASSG of (string)
    | BANG of (string)
    | BINOP of (string)
    | DOT of (string)
    | DOUBLEBAR of (string)
    | ELSE of (string)
    | FALSE of (string)
    | GRAB of (string)
    | IDENT of (string)
    | IF of (string)
    | IN of (string)
    | LAMBDA of (string)
    | LBR of (string)
    | NEW of (string)
    | NUM of (string)
    | RBR of (string)
    | RELEASE of (string)
    | RNGLR_EOF of (string)
    | SEMAPHORE of (string)
    | SEMI of (string)
    | SKIP of (string)
    | THEN of (string)
    | TRUE of (string)

let genLiteral (str : string) (data : string) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | ASSG x -> box x
    | BANG x -> box x
    | BINOP x -> box x
    | DOT x -> box x
    | DOUBLEBAR x -> box x
    | ELSE x -> box x
    | FALSE x -> box x
    | GRAB x -> box x
    | IDENT x -> box x
    | IF x -> box x
    | IN x -> box x
    | LAMBDA x -> box x
    | LBR x -> box x
    | NEW x -> box x
    | NUM x -> box x
    | RBR x -> box x
    | RELEASE x -> box x
    | RNGLR_EOF x -> box x
    | SEMAPHORE x -> box x
    | SEMI x -> box x
    | SKIP x -> box x
    | THEN x -> box x
    | TRUE x -> box x

let numToString = function
    | 0 -> "block"
    | 1 -> "error"
    | 2 -> "program"
    | 3 -> "stmt"
    | 4 -> "yard_start_rule"
    | 5 -> "ASSG"
    | 6 -> "BANG"
    | 7 -> "BINOP"
    | 8 -> "DOT"
    | 9 -> "DOUBLEBAR"
    | 10 -> "ELSE"
    | 11 -> "FALSE"
    | 12 -> "GRAB"
    | 13 -> "IDENT"
    | 14 -> "IF"
    | 15 -> "IN"
    | 16 -> "LAMBDA"
    | 17 -> "LBR"
    | 18 -> "NEW"
    | 19 -> "NUM"
    | 20 -> "RBR"
    | 21 -> "RELEASE"
    | 22 -> "RNGLR_EOF"
    | 23 -> "SEMAPHORE"
    | 24 -> "SEMI"
    | 25 -> "SKIP"
    | 26 -> "THEN"
    | 27 -> "TRUE"
    | _ -> ""

let tokenToNumber = function
    | ASSG _ -> 5
    | BANG _ -> 6
    | BINOP _ -> 7
    | DOT _ -> 8
    | DOUBLEBAR _ -> 9
    | ELSE _ -> 10
    | FALSE _ -> 11
    | GRAB _ -> 12
    | IDENT _ -> 13
    | IF _ -> 14
    | IN _ -> 15
    | LAMBDA _ -> 16
    | LBR _ -> 17
    | NEW _ -> 18
    | NUM _ -> 19
    | RBR _ -> 20
    | RELEASE _ -> 21
    | RNGLR_EOF _ -> 22
    | SEMAPHORE _ -> 23
    | SEMI _ -> 24
    | SKIP _ -> 25
    | THEN _ -> 26
    | TRUE _ -> 27

let isLiteral = function
    | ASSG _ -> false
    | BANG _ -> false
    | BINOP _ -> false
    | DOT _ -> false
    | DOUBLEBAR _ -> false
    | ELSE _ -> false
    | FALSE _ -> false
    | GRAB _ -> false
    | IDENT _ -> false
    | IF _ -> false
    | IN _ -> false
    | LAMBDA _ -> false
    | LBR _ -> false
    | NEW _ -> false
    | NUM _ -> false
    | RBR _ -> false
    | RELEASE _ -> false
    | RNGLR_EOF _ -> false
    | SEMAPHORE _ -> false
    | SEMI _ -> false
    | SKIP _ -> false
    | THEN _ -> false
    | TRUE _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 2; 4; 0; 0; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]
let private rules = [|0; 9; 2; 0; 2; 3; 24; 0; 3; 27; 11; 19; 25; 13; 16; 13; 8; 2; 2; 17; 2; 20; 3; 7; 2; 14; 2; 26; 2; 10; 2; 18; 13; 15; 2; 6; 2; 13; 5; 2; 23; 13; 15; 2; 12; 13; 21; 13; 17; 2; 20|]
let private rulesStart = [|0; 3; 4; 5; 8; 9; 10; 11; 12; 13; 14; 18; 22; 25; 31; 35; 37; 40; 44; 46; 48; 51|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber None leftSide)

let private lists_gotos = [|1; 47; 7; 13; 15; 16; 18; 21; 27; 31; 34; 38; 39; 41; 45; 46; 2; 3; 4; 5; 6; 8; 10; 9; 11; 12; 14; 17; 19; 20; 22; 23; 24; 25; 26; 28; 29; 30; 32; 33; 35; 36; 37; 40; 42; 43; 44|]
let private small_gotos =
        [|16; 0; 131073; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 65537; 589840; 131088; 0; 131089; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 196609; 1114130; 262160; 0; 131091; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 327682; 1114130; 1310740; 458754; 458773; 1572886; 524304; 0; 131095; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 589825; 1114130; 655376; 24; 131097; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 720897; 589840; 786433; 1114130; 851984; 0; 131098; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 917505; 1114130; 1048577; 851995; 1179649; 327708; 1245200; 0; 131101; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 1310721; 1114130; 1376272; 0; 131102; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 1441794; 1114130; 1703967; 1507344; 0; 131104; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 1572866; 655393; 1114130; 1638416; 0; 131106; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 1703937; 1114130; 1769473; 852003; 1835009; 524324; 1900560; 0; 131109; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 1966081; 1114130; 2031632; 0; 131110; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 2097154; 1114130; 1310759; 2228225; 852008; 2293761; 983081; 2359312; 0; 131114; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 2424833; 1114130; 2555905; 852011; 2686977; 852012; 2752513; 983085; 2818064; 0; 131118; 196610; 393219; 720900; 786437; 851974; 917511; 1048584; 1114121; 1179658; 1245195; 1376268; 1507341; 1638414; 1769487; 2883585; 1114130; 3080193; 1114130|]
let gotos = Array.zeroCreate 48
for i = 0 to 47 do
        gotos.[i] <- Array.zeroCreate 28
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|1,1|]; [|0,3|]; [|11,4|]; [|4,1|]; [|12,3|]; [|3,3|]; [|3,3; 1,1|]; [|15,2|]; [|6,1|]; [|18,2|]; [|9,1|]; [|16,3|]; [|13,6|]; [|10,4|]; [|20,3|]; [|14,4|]; [|7,1|]; [|19,2|]; [|17,4|]; [|8,1|]; [|5,1|]|]
let private small_reduces =
        [|65544; 458752; 589824; 655360; 1114112; 1310720; 1441792; 1572864; 1703936; 196616; 458753; 589825; 655361; 1114113; 1310721; 1441793; 1572865; 1703937; 393224; 458754; 589826; 655362; 1114114; 1310722; 1441794; 1572866; 1703938; 458760; 458755; 589827; 655363; 1114115; 1310723; 1441795; 1572867; 1703939; 589832; 458756; 589828; 655364; 1114116; 1310724; 1441796; 1572868; 1703940; 720904; 458757; 589829; 655365; 1114118; 1310725; 1441797; 1572869; 1703941; 917512; 458759; 589831; 655367; 1114119; 1310727; 1441799; 1572871; 1703943; 983048; 458760; 589832; 655368; 1114120; 1310728; 1441800; 1572872; 1703944; 1114120; 458761; 589833; 655369; 1114121; 1310729; 1441801; 1572873; 1703945; 1179656; 458762; 589834; 655370; 1114122; 1310730; 1441802; 1572874; 1703946; 1310728; 458763; 589835; 655371; 1114123; 1310731; 1441803; 1572875; 1703947; 1703944; 458764; 589836; 655372; 1114124; 1310732; 1441804; 1572876; 1703948; 1966088; 458765; 589837; 655373; 1114125; 1310733; 1441805; 1572877; 1703949; 2162696; 458766; 589838; 655374; 1114126; 1310734; 1441806; 1572878; 1703950; 2424840; 458767; 589839; 655375; 1114127; 1310735; 1441807; 1572879; 1703951; 2490376; 458768; 589840; 655376; 1114128; 1310736; 1441808; 1572880; 1703952; 2621448; 458769; 589841; 655377; 1114129; 1310737; 1441809; 1572881; 1703953; 2883592; 458770; 589842; 655378; 1114130; 1310738; 1441810; 1572882; 1703954; 2949128; 458771; 589843; 655379; 1114131; 1310739; 1441811; 1572883; 1703955; 3014664; 458772; 589844; 655380; 1114132; 1310740; 1441812; 1572884; 1703956|]
let reduces = Array.zeroCreate 48
for i = 0 to 47 do
        reduces.[i] <- Array.zeroCreate 28
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 48
for i = 0 to 47 do
        zeroReduces.[i] <- Array.zeroCreate 28
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [47]
let private accStates = Array.zeroCreate 48
for i = 0 to 47 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 22
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,(new AST(new Family(21, new Nodes([||])), null)), null); null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,(new AST(new Family(21, new Nodes([||])), null)), null); null; null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_block * '_rnglr_type_error * '_rnglr_type_program * '_rnglr_type_stmt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_block)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with DOUBLEBAR _rnglr_val -> [_rnglr_val] | a -> failwithf "DOUBLEBAR expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 6 "ICA.yrd"
                                                              Par (p1, p2) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 247 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_block)  |> List.iter (fun (i) -> 
              _rnglr_cycle_res := (
                
# 7 "ICA.yrd"
                                   i
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 266 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_program) 
            )
# 6 "ICA.yrd"
               : '_rnglr_type_yard_start_rule) 
# 276 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwithf "SEMI expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_block) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 9 "ICA.yrd"
                                                    Seq (p1, p2) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 9 "ICA.yrd"
               : '_rnglr_type_block) 
# 299 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt)  |> List.iter (fun (i) -> 
              _rnglr_cycle_res := (
                
# 10 "ICA.yrd"
                                 i 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 9 "ICA.yrd"
               : '_rnglr_type_block) 
# 318 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with TRUE _rnglr_val -> [_rnglr_val] | a -> failwithf "TRUE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 13 "ICA.yrd"
                           True
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 338 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with FALSE _rnglr_val -> [_rnglr_val] | a -> failwithf "FALSE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 13 "ICA.yrd"
                                          False
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 358 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NUM _rnglr_val -> [_rnglr_val] | a -> failwithf "NUM expected, but %A found" a )
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 13 "ICA.yrd"
                                                          Num (int n)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 378 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SKIP _rnglr_val -> [_rnglr_val] | a -> failwithf "SKIP expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 14 "ICA.yrd"
                           Skip
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 398 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
             |> List.iter (fun (i) -> 
              _rnglr_cycle_res := (
                
# 15 "ICA.yrd"
                              Var i
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 418 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LAMBDA _rnglr_val -> [_rnglr_val] | a -> failwithf "LAMBDA expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
               |> List.iter (fun (i1) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with DOT _rnglr_val -> [_rnglr_val] | a -> failwithf "DOT expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_program) 
                   |> List.iter (fun (p) -> 
                    _rnglr_cycle_res := (
                      
# 16 "ICA.yrd"
                                                           Lambda (Var i1, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 444 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_program)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LBR _rnglr_val -> [_rnglr_val] | a -> failwithf "LBR expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RBR _rnglr_val -> [_rnglr_val] | a -> failwithf "RBR expected, but %A found" a )
                   |> List.iter (fun (_rnglr_var_1) -> 
                    _rnglr_cycle_res := (
                      
# 17 "ICA.yrd"
                                                           App (p1, p2) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 469 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with BINOP _rnglr_val -> [_rnglr_val] | a -> failwithf "BINOP expected, but %A found" a )
               |> List.iter (fun (op) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 18 "ICA.yrd"
                                                       Expr (p1, p2, op) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 492 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IF _rnglr_val -> [_rnglr_val] | a -> failwithf "IF expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_program) 
               |> List.iter (fun (cond) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with THEN _rnglr_val -> [_rnglr_val] | a -> failwithf "THEN expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_program) 
                   |> List.iter (fun (p1) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with ELSE _rnglr_val -> [_rnglr_val] | a -> failwithf "ELSE expected, but %A found" a )
                     |> List.iter (fun (_rnglr_var_2) -> 
                      ((unbox _rnglr_children.[5]) : '_rnglr_type_program) 
                       |> List.iter (fun (p2) -> 
                        _rnglr_cycle_res := (
                          
# 19 "ICA.yrd"
                                                                                 If (cond, p1, p2)
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 522 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NEW _rnglr_val -> [_rnglr_val] | a -> failwithf "NEW expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
               |> List.iter (fun (i) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with IN _rnglr_val -> [_rnglr_val] | a -> failwithf "IN expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_program) 
                   |> List.iter (fun (p) -> 
                    _rnglr_cycle_res := (
                      
# 20 "ICA.yrd"
                                                      New (Var i, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 548 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with BANG _rnglr_val -> [_rnglr_val] | a -> failwithf "BANG expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_program) 
               |> List.iter (fun (p) -> 
                _rnglr_cycle_res := (
                  
# 21 "ICA.yrd"
                                        Deref p 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 570 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
             |> List.iter (fun (i) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ASSG _rnglr_val -> [_rnglr_val] | a -> failwithf "ASSG expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p) -> 
                  _rnglr_cycle_res := (
                    
# 22 "ICA.yrd"
                                                  Assg (Var i, p) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 594 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMAPHORE _rnglr_val -> [_rnglr_val] | a -> failwithf "SEMAPHORE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
               |> List.iter (fun (i) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with IN _rnglr_val -> [_rnglr_val] | a -> failwithf "IN expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_program) 
                   |> List.iter (fun (p) -> 
                    _rnglr_cycle_res := (
                      
# 23 "ICA.yrd"
                                                             Sem (Var i, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 620 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAB _rnglr_val -> [_rnglr_val] | a -> failwithf "GRAB expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
               |> List.iter (fun (i) -> 
                _rnglr_cycle_res := (
                  
# 24 "ICA.yrd"
                                     Grab (Var i) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 642 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with RELEASE _rnglr_val -> [_rnglr_val] | a -> failwithf "RELEASE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with IDENT _rnglr_val -> [_rnglr_val] | a -> failwithf "IDENT expected, but %A found" a )
               |> List.iter (fun (i) -> 
                _rnglr_cycle_res := (
                  
# 25 "ICA.yrd"
                                        Release (Var i)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 664 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBR _rnglr_val -> [_rnglr_val] | a -> failwithf "LBR expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_program) 
               |> List.iter (fun (p) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBR _rnglr_val -> [_rnglr_val] | a -> failwithf "RBR expected, but %A found" a )
                 |> List.iter (fun (_rnglr_var_1) -> 
                  _rnglr_cycle_res := (
                    
# 26 "ICA.yrd"
                                            p
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 688 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 706 "ICA.Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_block)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_program)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
