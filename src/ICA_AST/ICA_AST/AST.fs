module GS.ICA.AST
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns


type ast = 
 | True
 | False
 | Num of int
 | Skip
 | Var of string
 | Lambda of ast*ast
 | App of ast*ast
 | Expr of ast*ast*string
 | If of ast*ast*ast
 | New of ast*ast
 | Seq of ast*ast
 | Deref of ast
 | Assg of ast*ast
 | Par of ast*ast
 | Sem of ast*ast
 | Grab of ast
 | Release of ast
 | Fix of ast

type Q<'retVal> = Q of AsyncReplyChannel<'retVal>

let mkTransducer name retVal =
    let inbox = MailboxProcessor.Start(
        fun inbox ->
            let rec loop i =
                async{
                    let! msg = inbox.Receive()
                    match msg with
                    | Q rc ->
                        printfn "tr name = %A" name
                        rc.Reply (retVal())
                        return! loop i            
                }
            loop 0)
    inbox

type tRes<'t> =
    | L of (tRes<'t> -> tRes<'t>)
    | T of 't
    | P of tRes<'t>*(tRes<'t> -> tRes<'t>)

let eval t =
    match t with 
    | T (t:MailboxProcessor<_>) -> t.PostAndReply(fun rc -> Q rc)
    | _ -> failwith "incorrect"


let c = ref 0
let getName s =
    let r = s + "_" + (string !c) 
    incr c
    r

let rec translate ast context =
    match ast with 
    | Num n -> T(mkTransducer (getName ("num_" + string n)) (fun () -> n))
    | Expr(l, r, op) ->
        let l' =  translate l context
        let r' = translate r context 
        T(mkTransducer (getName "expr") (fun () -> eval l' + eval r'))
    | Lambda ((Var s), ast) ->
        L(fun s' -> translate ast ((s,s')::context))
    | Var s -> 
        let fst = context |> List.tryFind (fun (name,t) -> name = s)
        match fst with 
        | Some (s,t) -> t
        | _ -> failwithf "var %A not binded" s
    | App (a,b)  ->
        let at = translate a context
        match at with
        | L f -> f (translate b context)
    | Deref e ->
        let v = translate e context
        match v with
        | P (get, set) -> get
    | Assg (v, e) ->
        let v' = translate v context
        match v' with
        | P (get, set) -> set (translate e context)    
    | New ((Var s), e) -> 
        let v =
                let store = ref 0
                P(T(mkTransducer (getName ("get_" + s)) (fun () -> !store)), 
                  fun t ->
                    let f () =
                        store := eval t
                        !store
                    T (mkTransducer (getName ("set_" + s)) f))
        translate e ((s,v)::context)
    | Seq (e1, e2) ->
        let t1 = translate e1 context
        let t2 = translate e2 context
        T (mkTransducer (getName "seq") 
                        (fun () -> eval t1
                                   eval t2))
    | If (c,t,e) ->
        let c' = translate c context
        let t' = translate t context
        let e' = translate e context
        T (mkTransducer
                        (getName "if")
                        (fun () ->
                            let c = eval c'
                            if c = 0
                            then eval t'
                            else eval e'
                         ))

//let next =
//        let counter = ref 0
//        fun () ->
//            incr counter
//            !counter    
//
//let mkTransducerQ retVal =    
//    let getId = next
//    let newName = Microsoft.FSharp.Quotations.Var ("f" + string (getId()))
//    fun b -> Microsoft.FSharp.Quotations.Expr.l (newName, %retVal, b)    
//
//
//let evalQ t =
//    match t with 
//    | T (t:MailboxProcessor<_>) -> t.PostAndReply(fun rc -> Q rc)
//    | _ -> failwith "incorrect"
//
//let rec translateQ ast context =
//    match ast with 
//    | Num n -> T(mkTransducerQ <@n@>)
//    | Expr(l, r, op) ->
//        let l' =  translate l context
//        let r' = translate r context 
//        T(mkTransducer (fun () -> eval l' + eval r'))
//    | Lambda ((Var s), ast) ->
//        L(fun s' -> translate ast ((s,s')::context))
//    | Var s -> 
//        let fst = context |> List.tryFind (fun (name,t) -> name = s)
//        match fst with 
//        | Some (s,t) -> t
//        | _ -> failwithf "var %A not binded" s
//    | App (a,b)  ->
//        let at = translate a context
//        match at with
//        | L f -> f (translate b context)
//    | Deref e ->
//        let v = translate e context
//        match v with
//        | P (get, set) -> get
//    | Assg (v, e) ->
//        let v' = translate v context
//        match v' with
//        | P (get, set) -> set (translate e context)    
//    | New ((Var s), e) -> 
//        let v =
//                let store = ref 0
//                P(T(mkTransducer (fun () -> !store)), 
//                  fun t ->
//                    let f () =
//                        store := eval t
//                        !store
//                    T (mkTransducer f))     
//        translate e ((s,v)::context)
//    | Seq (e1, e2) ->
//        let t1 = translate e1 context
//        let t2 = translate e2 context
//        T (mkTransducer (fun () -> eval t1
//                                   eval t2))
//    | If (c,t,e) ->
//        let c' = translate c context
//        let t' = translate t context
//        let e' = translate e context
//        T (mkTransducer (fun () ->
//                            let c = eval c'
//                            if c = 0
//                            then eval t'
//                            else eval e'
//                         ))