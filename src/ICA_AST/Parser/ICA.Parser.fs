
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
    | FIX of (string)
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
    | FIX x -> box x
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
    | 12 -> "FIX"
    | 13 -> "GRAB"
    | 14 -> "IDENT"
    | 15 -> "IF"
    | 16 -> "IN"
    | 17 -> "LAMBDA"
    | 18 -> "LBR"
    | 19 -> "NEW"
    | 20 -> "NUM"
    | 21 -> "RBR"
    | 22 -> "RELEASE"
    | 23 -> "RNGLR_EOF"
    | 24 -> "SEMAPHORE"
    | 25 -> "SEMI"
    | 26 -> "SKIP"
    | 27 -> "THEN"
    | 28 -> "TRUE"
    | _ -> ""

let tokenToNumber = function
    | ASSG _ -> 5
    | BANG _ -> 6
    | BINOP _ -> 7
    | DOT _ -> 8
    | DOUBLEBAR _ -> 9
    | ELSE _ -> 10
    | FALSE _ -> 11
    | FIX _ -> 12
    | GRAB _ -> 13
    | IDENT _ -> 14
    | IF _ -> 15
    | IN _ -> 16
    | LAMBDA _ -> 17
    | LBR _ -> 18
    | NEW _ -> 19
    | NUM _ -> 20
    | RBR _ -> 21
    | RELEASE _ -> 22
    | RNGLR_EOF _ -> 23
    | SEMAPHORE _ -> 24
    | SEMI _ -> 25
    | SKIP _ -> 26
    | THEN _ -> 27
    | TRUE _ -> 28

let isLiteral = function
    | ASSG _ -> false
    | BANG _ -> false
    | BINOP _ -> false
    | DOT _ -> false
    | DOUBLEBAR _ -> false
    | ELSE _ -> false
    | FALSE _ -> false
    | FIX _ -> false
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
let leftSide = [|2; 2; 4; 0; 0; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]
let private rules = [|0; 9; 2; 0; 2; 3; 25; 0; 3; 28; 11; 20; 26; 14; 17; 14; 8; 2; 2; 18; 2; 21; 3; 7; 2; 15; 2; 27; 2; 10; 2; 19; 14; 16; 2; 6; 2; 14; 5; 2; 24; 14; 16; 2; 13; 14; 22; 14; 12; 2; 18; 2; 21|]
let private rulesStart = [|0; 3; 4; 5; 8; 9; 10; 11; 12; 13; 14; 18; 22; 25; 31; 35; 37; 40; 44; 46; 48; 50; 53|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber None leftSide)

let private lists_gotos = [|1; 49; 7; 13; 15; 16; 18; 20; 23; 29; 33; 36; 40; 41; 43; 47; 48; 2; 3; 4; 5; 6; 8; 10; 9; 11; 12; 14; 17; 19; 21; 22; 24; 25; 26; 27; 28; 30; 31; 32; 34; 35; 37; 38; 39; 42; 44; 45; 46|]
let private small_gotos =
        [|17; 0; 131073; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 65537; 589841; 131089; 0; 131090; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 196609; 1179667; 262161; 0; 131092; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 327682; 1179667; 1376277; 458754; 458774; 1638423; 524305; 0; 131096; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 589825; 1179667; 655377; 25; 131098; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 720897; 589841; 786433; 1179667; 851985; 0; 131099; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 917505; 1179667; 1048593; 0; 131100; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 1114113; 1179667; 1179649; 917533; 1310721; 327710; 1376273; 0; 131103; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 1441793; 1179667; 1507345; 0; 131104; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 1572866; 1179667; 1769505; 1638417; 0; 131106; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 1703938; 655395; 1179667; 1769489; 0; 131108; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 1835009; 1179667; 1900545; 917541; 1966081; 524326; 2031633; 0; 131111; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 2097153; 1179667; 2162705; 0; 131112; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 2228226; 1179667; 1376297; 2359297; 917546; 2424833; 1048619; 2490385; 0; 131116; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 2555905; 1179667; 2686977; 917549; 2818049; 917550; 2883585; 1048623; 2949137; 0; 131120; 196610; 393219; 720900; 786437; 851974; 917511; 983048; 1114121; 1179658; 1245195; 1310732; 1441805; 1572878; 1703951; 1835024; 3014657; 1179667; 3211265; 1179667|]
let gotos = Array.zeroCreate 50
for i = 0 to 49 do
        gotos.[i] <- Array.zeroCreate 29
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
let private lists_reduces = [|[|1,1|]; [|0,3|]; [|11,4|]; [|4,1|]; [|12,3|]; [|3,3|]; [|3,3; 1,1|]; [|15,2|]; [|6,1|]; [|20,2|]; [|18,2|]; [|9,1|]; [|16,3|]; [|13,6|]; [|10,4|]; [|21,3|]; [|14,4|]; [|7,1|]; [|19,2|]; [|17,4|]; [|8,1|]; [|5,1|]|]
let private small_reduces =
        [|65544; 458752; 589824; 655360; 1179648; 1376256; 1507328; 1638400; 1769472; 196616; 458753; 589825; 655361; 1179649; 1376257; 1507329; 1638401; 1769473; 393224; 458754; 589826; 655362; 1179650; 1376258; 1507330; 1638402; 1769474; 458760; 458755; 589827; 655363; 1179651; 1376259; 1507331; 1638403; 1769475; 589832; 458756; 589828; 655364; 1179652; 1376260; 1507332; 1638404; 1769476; 720904; 458757; 589829; 655365; 1179654; 1376261; 1507333; 1638405; 1769477; 917512; 458759; 589831; 655367; 1179655; 1376263; 1507335; 1638407; 1769479; 983048; 458760; 589832; 655368; 1179656; 1376264; 1507336; 1638408; 1769480; 1114120; 458761; 589833; 655369; 1179657; 1376265; 1507337; 1638409; 1769481; 1245192; 458762; 589834; 655370; 1179658; 1376266; 1507338; 1638410; 1769482; 1310728; 458763; 589835; 655371; 1179659; 1376267; 1507339; 1638411; 1769483; 1441800; 458764; 589836; 655372; 1179660; 1376268; 1507340; 1638412; 1769484; 1835016; 458765; 589837; 655373; 1179661; 1376269; 1507341; 1638413; 1769485; 2097160; 458766; 589838; 655374; 1179662; 1376270; 1507342; 1638414; 1769486; 2293768; 458767; 589839; 655375; 1179663; 1376271; 1507343; 1638415; 1769487; 2555912; 458768; 589840; 655376; 1179664; 1376272; 1507344; 1638416; 1769488; 2621448; 458769; 589841; 655377; 1179665; 1376273; 1507345; 1638417; 1769489; 2752520; 458770; 589842; 655378; 1179666; 1376274; 1507346; 1638418; 1769490; 3014664; 458771; 589843; 655379; 1179667; 1376275; 1507347; 1638419; 1769491; 3080200; 458772; 589844; 655380; 1179668; 1376276; 1507348; 1638420; 1769492; 3145736; 458773; 589845; 655381; 1179669; 1376277; 1507349; 1638421; 1769493|]
let reduces = Array.zeroCreate 50
for i = 0 to 49 do
        reduces.[i] <- Array.zeroCreate 29
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
let zeroReduces = Array.zeroCreate 50
for i = 0 to 49 do
        zeroReduces.[i] <- Array.zeroCreate 29
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
let private small_acc = [49]
let private accStates = Array.zeroCreate 50
for i = 0 to 49 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 23
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,(new AST(new Family(22, new Nodes([||])), null)), null); null; null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,(new AST(new Family(22, new Nodes([||])), null)), null); null; null; null|]
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
# 252 "ICA.Parser.fs"
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
# 271 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_program) 
            )
# 6 "ICA.yrd"
               : '_rnglr_type_yard_start_rule) 
# 281 "ICA.Parser.fs"
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
# 304 "ICA.Parser.fs"
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
# 323 "ICA.Parser.fs"
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
# 343 "ICA.Parser.fs"
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
# 363 "ICA.Parser.fs"
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
# 383 "ICA.Parser.fs"
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
# 403 "ICA.Parser.fs"
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
# 423 "ICA.Parser.fs"
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
# 449 "ICA.Parser.fs"
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
# 474 "ICA.Parser.fs"
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
# 497 "ICA.Parser.fs"
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
# 527 "ICA.Parser.fs"
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
# 553 "ICA.Parser.fs"
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
# 575 "ICA.Parser.fs"
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
# 599 "ICA.Parser.fs"
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
# 625 "ICA.Parser.fs"
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
# 647 "ICA.Parser.fs"
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
# 669 "ICA.Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (int * int)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with FIX _rnglr_val -> [_rnglr_val] | a -> failwithf "FIX expected, but %A found" a )
             |> List.iter (fun (_rnglr_var_0) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_program) 
               |> List.iter (fun (p) -> 
                _rnglr_cycle_res := (
                  
# 26 "ICA.yrd"
                                      Fix p
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 691 "ICA.Parser.fs"
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
                    
# 27 "ICA.yrd"
                                            p
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 12 "ICA.yrd"
               : '_rnglr_type_stmt) 
# 715 "ICA.Parser.fs"
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
# 733 "ICA.Parser.fs"
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
