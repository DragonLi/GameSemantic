module GS.ICA.AST

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

type termType =
    | Tuple of termType*termType
    | Fun of termType*termType
    | N

type Q<'retVal> = Q of AsyncReplyChannel<'retVal>

let mkTransducer retVal =
    let inbox = MailboxProcessor.Start(
        fun inbox ->
            let rec loop i =
                async{
                    let! msg = inbox.Receive()
                    match msg with
                    | Q rc ->
                        rc.Reply (retVal())
                        return! loop i            
                }
            loop 0)
    inbox

//let rec typeAst ast =
//    match 

type tRes<'t> =
    | L of (tRes<'t> -> tRes<'t>)
    | T of 't
    | P of tRes<'t>*(tRes<'t> -> tRes<'t>)

let eval t =
    match t with T (t:MailboxProcessor<_>) -> t.PostAndReply(fun rc -> Q rc)

let rec translate ast context =
    match ast with 
    | Num n -> T(mkTransducer (fun () -> n))
    | Expr(l, r, op) ->
        let l' = 
            match translate l context with
            | T t -> t
            | _ -> failwith "incorrect"
        let r' = 
            match translate r context with
            | T t -> t
            | _ -> failwith "incorrect"
        T(mkTransducer (fun () -> l'.PostAndReply(fun rc -> Q rc) + r'.PostAndReply(fun rc -> Q rc)
                        ))
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
                P(T(mkTransducer (fun () ->
                                        printfn "deref V = %A" !store 
                                        !store)), 
                  fun t ->
                    let f () = 
                        printfn "S1=%A" !store
                        match t with
                        | T t -> store := t.PostAndReply(fun rc -> Q rc)
                        printfn "S2=%A" !store
                        !store
                    T (mkTransducer (f)))     
        translate e ((s,v)::context)
    | Seq (e1, e2) ->
        let t1 = translate e1 context
        let t2 = translate e2 context
        T (mkTransducer (fun () -> eval t1
                                   eval t2))
    | If (c,t,e) ->
        let c' = translate c context
        let t' = translate t context
        let e' = translate e context
        T (mkTransducer (fun () ->
                            let c = eval c'
                            if c = 0
                            then eval t'
                            else eval e'
                         ))
        
(*        new_var (\x.M)
        ((var->N)->N)
        (((N*(N->N))->N)->N) ((N*(N->N))->N)

        *)
        
//let genConst v = fst<_,_> q -> v
//
//let binOp l r op = 
//    fst<_,_> q -> (l q) op (r q)
//
//let genLambda v b = 
//    fun x -> 
//
//let genAppl a b = 
//    inject a (gen b) : fst<_,_>  
    
