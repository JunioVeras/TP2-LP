(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (env: plcVal env) : plcVal =
        (case e of
                Var(x) => lookup env x
            | ConI(i) => IntV(i)
            | ConB(b) => BoolV(b)
            | List(l) => ListV(map (fn x => eval x env) l)
            | ESeq(_) => SeqV([])
            | Let(x, e1, e2) =>
                let 
                    val tX = eval e1 env
                    val env' = (x, tX)::env
                in
                    eval e2 env'
                end
            (* | Letrec(f, types, argName, retType, body, e1) =>
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
                    if tCond = BoolV 
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
                end *)
            | Prim1(opr, e1) =>
                let 
                    val t1 = eval e1 env
                in 
                    (case (opr, t1) of
                            ("!", BoolV(b)) => BoolV(not b)
                        | ("-", IntV(i)) => IntV(~i)
                        (* | ("hd", SeqV(l)) => hd l *)
                        (* | ("tl", SeqV(t)) => 
                            (case e1 of
                                    ESeq(_) => raise EmptySeq
                                | _ => SeqV(t)
                            )
                        | ("ise", SeqV(t)) => BoolV *)
                        | ("print", e) =>
                            TextIO.output(TextIO.stdOut, val2string (eval e env)); ListV []
                    )
                end
            (* | Prim2(opr, e1, e2) =>
                let 
                    val t1 = teval e1 env
                    val t2 = teval e2 env
                in
                    (case (opr, t1, t2) of
                            ("&&", BoolV, BoolV) => BoolV
                        | ("+", IntV, IntV) => IntV
                        | ("-", IntV, IntV) => IntV
                        | ("*", IntV, IntV) => IntV
                        | ("/", IntV, IntV) => IntV
                        | ("<", IntV, IntV) => BoolV
                        | ("<=", IntV, IntV) => BoolV
                        | ("=",_ , _) =>
                            if t1 = t2 then 
                                if equalityType t1 then BoolV else raise UnknownType
                            else raise NotEqTypes
                        | ("!=", _, _) =>
                            if t1 = t2 then 
                                if equalityType t1 then BoolV else raise UnknownType
                            else raise NotEqTypes
                        | ("::", _, _) => 
                            (case t2 of
                                    SeqV(b) => 
                                        if t1 = b then 
                                            SeqV(t1) 
                                        else 
                                            raise UnknownType
                                | _ =>  raise UnknownType
                            )
                        | (";", _, _) => t2
                        | _ =>  raise UnknownType
                    )
                end *)
            (* | Item(i, e) =>
                let
                    fun findIth (n, []) = raise ListOutOfRange
                        | findIth(n, h::t) = if n = i then h else findIth(n + 1, t)
                in
                    (case (teval e env) of
                            ListT(l) => findIth (1, l)
                        | _ => raise OpNonList
                    )
                
                end *)
        );
(* use "myTest.sml"; *)