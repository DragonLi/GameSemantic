
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
    | 0 -> "error"
    | 1 -> "program"
    | 2 -> "yard_start_rule"
    | 3 -> "ASSG"
    | 4 -> "BANG"
    | 5 -> "BINOP"
    | 6 -> "DOT"
    | 7 -> "DOUBLEBAR"
    | 8 -> "ELSE"
    | 9 -> "FALSE"
    | 10 -> "GRAB"
    | 11 -> "IDENT"
    | 12 -> "IF"
    | 13 -> "IN"
    | 14 -> "LAMBDA"
    | 15 -> "LBR"
    | 16 -> "NEW"
    | 17 -> "NUM"
    | 18 -> "RBR"
    | 19 -> "RELEASE"
    | 20 -> "RNGLR_EOF"
    | 21 -> "SEMAPHORE"
    | 22 -> "SEMI"
    | 23 -> "SKIP"
    | 24 -> "THEN"
    | 25 -> "TRUE"
    | _ -> ""

let tokenToNumber = function
    | ASSG _ -> 3
    | BANG _ -> 4
    | BINOP _ -> 5
    | DOT _ -> 6
    | DOUBLEBAR _ -> 7
    | ELSE _ -> 8
    | FALSE _ -> 9
    | GRAB _ -> 10
    | IDENT _ -> 11
    | IF _ -> 12
    | IN _ -> 13
    | LAMBDA _ -> 14
    | LBR _ -> 15
    | NEW _ -> 16
    | NUM _ -> 17
    | RBR _ -> 18
    | RELEASE _ -> 19
    | RNGLR_EOF _ -> 20
    | SEMAPHORE _ -> 21
    | SEMI _ -> 22
    | SKIP _ -> 23
    | THEN _ -> 24
    | TRUE _ -> 25

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
let leftSide = [|1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 2|]
let private rules = [|25; 9; 17; 23; 11; 14; 11; 6; 1; 1; 15; 1; 18; 1; 5; 1; 12; 1; 24; 1; 8; 1; 16; 11; 13; 1; 1; 22; 1; 4; 1; 11; 3; 1; 1; 7; 1; 21; 11; 13; 1; 10; 11; 19; 11; 1|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 9; 13; 16; 22; 26; 29; 31; 34; 37; 41; 43; 45; 46|]
let startRule = 17

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber None leftSide)

let private lists_gotos = [|1; 11; 13; 14; 16; 19; 25; 29; 33; 34; 36; 40; 41; 2; 4; 6; 9; 3; 5; 7; 8; 10; 12; 15; 17; 18; 20; 21; 22; 23; 24; 26; 27; 28; 30; 31; 32; 35; 37; 38; 39|]
let private small_gotos =
        [|13; 65536; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 65540; 327693; 458766; 983055; 1441808; 131085; 65553; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 196612; 327693; 458766; 983055; 1441808; 262157; 65554; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 327684; 327693; 458766; 983055; 1441808; 393229; 65555; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 458757; 327693; 458766; 983055; 1179668; 1441808; 589837; 65557; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 655364; 327693; 458766; 983055; 1441808; 720909; 65558; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 786436; 327693; 458766; 983055; 1441808; 917505; 720919; 1048577; 196632; 1114125; 65561; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 1179652; 327693; 458766; 983055; 1441808; 1245197; 65562; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 1310725; 327693; 458766; 983055; 1441808; 1572891; 1376269; 65564; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 1441797; 327693; 458766; 524317; 983055; 1441808; 1507341; 65566; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 1572868; 327693; 458766; 983055; 1441808; 1638401; 720927; 1703937; 393248; 1769485; 65569; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 1835012; 327693; 458766; 983055; 1441808; 1900545; 720930; 1966081; 852003; 2031629; 65572; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 2097156; 327693; 458766; 983055; 1441808; 2228225; 720933; 2359297; 720934; 2424833; 852007; 2490381; 65576; 262145; 589826; 655363; 720900; 786437; 917510; 1048583; 1114120; 1245193; 1376266; 1507339; 1638412; 2555908; 327693; 458766; 983055; 1441808|]
let gotos = Array.zeroCreate 42
for i = 0 to 41 do
        gotos.[i] <- Array.zeroCreate 26
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
let private lists_reduces = [|[|7,3|]; [|13,3|]; [|6,4|]; [|10,3|]; [|11,2|]; [|1,1|]; [|15,2|]; [|4,1|]; [|12,3|]; [|8,6|]; [|5,4|]; [|9,4|]; [|2,1|]; [|16,2|]; [|14,4|]; [|3,1|]; [|0,1|]|]
let private small_reduces =
        [|196616; 327680; 458752; 524288; 983040; 1179648; 1310720; 1441792; 1572864; 327688; 327681; 458753; 524289; 983041; 1179649; 1310721; 1441793; 1572865; 524296; 327682; 458754; 524290; 983042; 1179650; 1310722; 1441794; 1572866; 655368; 327683; 458755; 524291; 983043; 1179651; 1310723; 1441795; 1572867; 786440; 327684; 458756; 524292; 983044; 1179652; 1310724; 1441796; 1572868; 851976; 327685; 458757; 524293; 983045; 1179653; 1310725; 1441797; 1572869; 983048; 327686; 458758; 524294; 983046; 1179654; 1310726; 1441798; 1572870; 1048584; 327687; 458759; 524295; 983047; 1179655; 1310727; 1441799; 1572871; 1179656; 327688; 458760; 524296; 983048; 1179656; 1310728; 1441800; 1572872; 1572872; 327689; 458761; 524297; 983049; 1179657; 1310729; 1441801; 1572873; 1835016; 327690; 458762; 524298; 983050; 1179658; 1310730; 1441802; 1572874; 2097160; 327691; 458763; 524299; 983051; 1179659; 1310731; 1441803; 1572875; 2162696; 327692; 458764; 524300; 983052; 1179660; 1310732; 1441804; 1572876; 2293768; 327693; 458765; 524301; 983053; 1179661; 1310733; 1441805; 1572877; 2555912; 327694; 458766; 524302; 983054; 1179662; 1310734; 1441806; 1572878; 2621448; 327695; 458767; 524303; 983055; 1179663; 1310735; 1441807; 1572879; 2686984; 327696; 458768; 524304; 983056; 1179664; 1310736; 1441808; 1572880|]
let reduces = Array.zeroCreate 42
for i = 0 to 41 do
        reduces.[i] <- Array.zeroCreate 26
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
let zeroReduces = Array.zeroCreate 42
for i = 0 to 41 do
        zeroReduces.[i] <- Array.zeroCreate 26
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 42
for i = 0 to 41 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 20
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,(new AST(new Family(18, new Nodes([||])), null)), null); null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,(new AST(new Family(18, new Nodes([||])), null)), null); null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_program * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with TRUE _rnglr_val -> [_rnglr_val] | a -> failwithf "TRUE expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              _rnglr_cycle_res := (
                
# 7 "ICA.yrd"
                           True
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 242 "ICA.Parser.fs"
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
                
# 7 "ICA.yrd"
                                          False
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 262 "ICA.Parser.fs"
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
                
# 7 "ICA.yrd"
                                                          Num (int n)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 282 "ICA.Parser.fs"
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
                
# 8 "ICA.yrd"
                           Skip
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 302 "ICA.Parser.fs"
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
                
# 9 "ICA.yrd"
                              Var i
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 322 "ICA.Parser.fs"
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
                      
# 10 "ICA.yrd"
                                                           Lambda (Var i1, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 348 "ICA.Parser.fs"
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
                      
# 11 "ICA.yrd"
                                                           App (p1, p2) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 373 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_program)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with BINOP _rnglr_val -> [_rnglr_val] | a -> failwithf "BINOP expected, but %A found" a )
               |> List.iter (fun (op) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 12 "ICA.yrd"
                                                          Expr (p1, p2, op) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 396 "ICA.Parser.fs"
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
                          
# 13 "ICA.yrd"
                                                                                 If (cond, p1, p2)
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 426 "ICA.Parser.fs"
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
                      
# 14 "ICA.yrd"
                                                      New (Var i, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 452 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_program)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwithf "SEMI expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 15 "ICA.yrd"
                                                      Seq (p1, p2) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 475 "ICA.Parser.fs"
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
                  
# 16 "ICA.yrd"
                                        Deref p 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 497 "ICA.Parser.fs"
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
                    
# 17 "ICA.yrd"
                                                  Assg (Var i, p) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 521 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_program)  |> List.iter (fun (p1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with DOUBLEBAR _rnglr_val -> [_rnglr_val] | a -> failwithf "DOUBLEBAR expected, but %A found" a )
               |> List.iter (fun (_rnglr_var_0) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_program) 
                 |> List.iter (fun (p2) -> 
                  _rnglr_cycle_res := (
                    
# 18 "ICA.yrd"
                                                           Par (p1, p2) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 544 "ICA.Parser.fs"
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
                      
# 19 "ICA.yrd"
                                                             Sem (Var i, p) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 570 "ICA.Parser.fs"
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
                  
# 20 "ICA.yrd"
                                     Grab (Var i) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 592 "ICA.Parser.fs"
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
                  
# 21 "ICA.yrd"
                                        Release (Var i)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 6 "ICA.yrd"
               : '_rnglr_type_program) 
# 614 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_program) 
            )
# 6 "ICA.yrd"
               : '_rnglr_type_yard_start_rule) 
# 624 "ICA.Parser.fs"
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
# 642 "ICA.Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_program)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
