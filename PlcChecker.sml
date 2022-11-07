(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList
exception ERRO_NOSSO1
exception ERRO_NOSSO2

fun equalityType (IntT) = true
    | equalityType (BoolT) = true
    | equalityType (FunT(_, _)) = false
    | equalityType (SeqT(t)) = equalityType t
    | equalityType (ListT []) = true
    | equalityType (ListT(h::t)) = equalityType h andalso equalityType(ListT(t));

fun teval (e:expr) (env: plcType env) : plcType = 
        case e of
            Var(x) => lookup env x
        | ConI(_) => IntT
        | ConB(_) => BoolT
        | List(l) => ListT(map (fn x => teval x env) l)
        | ESeq(SeqT(t)) => SeqT(t)
        | Let(x, e1, e2) =>
            let 
                val tX = teval e1 env
                val env' = (x, tX)::env
            in
                teval e2 env'
            end
        | Letrec(f, types, argName, retType, body, e1) =>
            let
                val env' = (f, FunT(types, retType))::(argName, types)::env
                val env'' = (f, FunT(types, retType))::env
                val tBody = teval body env'
            in
                if tBody = retType
                then 
                    teval e1 env'' 
                else 
                    raise WrongRetType
            end
        | Anon(types, argName, body) =>
            (case argName of
                    "()" => FunT(ListT [], teval body env)
                | _ => FunT(types, teval body ((argName, types)::env))
            )
        | Call(e1, e2) =>
            let
                val t1 = teval e1 env
                val t2 = teval e2 env
            in
                (case t1 of
                        FunT(aT, rT) => if aT = t2 
                            then 
                                rT 
                            else
                                raise CallTypeMisM
                    | _ => raise NotFunc
                )
            end
        | If(cond, e1, e2) =>
            let
                val tCond = teval cond env
                val t1 = teval e1 env
                val t2 = teval e2 env
            in
                if tCond = BoolT 
                then 
                    if t1 = t2 
                    then t1 
                    else raise DiffBrTypes
                else 
                    raise IfCondNotBool
            end
        | Match(e1, mList) =>
            let
                val retType = 
                    if mList = [] 
                    then raise NoMatchResults
                    else teval (#2(hd mList)) env
                    
                fun validMatchTypes (t1, t2, []) = t2
                    | validMatchTypes (t1, t2, (SOME(mExpr), result)::t) =
                        if (teval mExpr env) = t1 
                        then 
                            if (teval result env) = t2
                            then validMatchTypes(t1, t2, t)
                            else raise MatchResTypeDiff
                        else 
                            raise MatchCondTypesDiff
                    | validMatchTypes (t1, t2, (NONE, result)::t) =
                        if (teval result env) = t2
                        then 
                            validMatchTypes(t1, t2, t)
                        else 
                            raise MatchResTypeDiff
            in
                validMatchTypes(teval e1 env, retType, mList)
            end
        | Prim1(opr, e1) =>
            let 
                val t1 = teval e1 env
            in 
                (case (opr, t1) of
                        ("!", BoolT) => BoolT
                    | ("-", IntT) => IntT
                    | ("hd", SeqT(t)) => 
                        (case e1 of
                                ESeq(_) => raise EmptySeq
                            | _ => t
                        )
                    | ("tl", SeqT(t)) => 
                        (case e1 of
                                ESeq(_) => raise EmptySeq
                            | _ => SeqT(t)
                        )
                    | ("ise", SeqT(t)) => BoolT
                    | ("print", _) => ListT []
                    | _ =>  raise UnknownType
                )
            end
        | Prim2(opr, e1, e2) =>
            let 
                val t1 = teval e1 env
                val t2 = teval e2 env
            in
                (case (opr, t1, t2) of
                        ("&&", BoolT, BoolT) => BoolT
                    | ("+", IntT, IntT) => IntT
                    | ("-", IntT, IntT) => IntT
                    | ("*", IntT, IntT) => IntT
                    | ("/", IntT, IntT) => IntT
                    | ("<", IntT, IntT) => BoolT
                    | ("<=", IntT, IntT) => BoolT
                    | ("=",_ , _) =>
                        if t1 = t2 then 
                            if equalityType t1 then BoolT else raise UnknownType
                        else raise NotEqTypes
                    | ("!=", _, _) =>
                        if t1 = t2 then 
                            if equalityType t1 then BoolT else raise UnknownType
                        else raise NotEqTypes
                    | ("::", _, _) => 
                        (case t2 of
                                SeqT(b) => 
                                    if t1 = b then 
                                        SeqT(t1) 
                                    else 
                                        raise UnknownType
                            | _ =>  raise UnknownType
                        )
                    | (";", _, _) => t2
                    | _ =>  raise UnknownType
                )
            end
        | Item(i, e1) =>
            let
                fun findIth (n, []) = raise ListOutOfRange
                    | findIth(n, h::t) = if n = i then h else findIth(n + 1, t)
            in
                (case (teval e1 env) of
                        ListT(l) => findIth (1, l)
                    | _ => raise OpNonList
                )
                
            end
        | _ => raise UnknownType
;
(* use "myTest.sml"; *)
