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
            | Letrec(f, _, argName, _, body, e1) =>
                let
                    val env' = (f, Clos(f, argName, body, env))::env
                in
                    eval e1 env'
                end
            | Anon(_, argName, body) => Clos("", argName, body, env)
            | Call(e1, e2) =>
                let
                    val func = eval e1 env
                    val Clos(f, argName, body, env' ) = func
                    val v2 = eval e2 env
                    val env'' = ((argName, v2)::(f, func)::env')
                in
                    eval body env''
                end
            | If(cond, e1, e2) =>
                let
                    val BoolV(b) = eval cond env
                    val v1 = eval e1 env
                    val v2 = eval e2 env
                in
                    if b then v1 else v2
                end
            | Match(e1, mList) =>
                let
                    val v1 = eval e1 env
                    fun matchValue (v, []) = raise NoMatchResults
                        | matchValue (v, (SOME mExpr, res)::t) = 
                            if v = (eval mExpr env)
                            then eval res env
                            else matchValue(v, t)
                        | matchValue (v, (NONE, res)::t) = eval res env
                in
                    matchValue(v1, mList)
                end
            | Prim1(opr, e1) =>
                let 
                    val v1 = eval e1 env
                in 
                    (case (opr, v1) of
                            ("!", BoolV(b)) => BoolV(not b)
                        | ("-", IntV(i)) => IntV(~i)
                        | ("hd", SeqV(l)) => hd l
                        | ("tl", SeqV(l)) => SeqV(tl l)
                        | ("ise", SeqV(l)) => BoolV(l = [])
                        | ("print", _) =>
                            let 
                                val a = TextIO.output(TextIO.stdOut, val2string (eval e1 env) ^ "\n")
                            in
                                ListV []
                            end
                    )
                end
            | Prim2(opr, e1, e2) =>
                let 
                    val v1 = eval e1 env
                    val v2 = eval e2 env
                in
                    (case (opr, v1, v2) of
                            ("&&", BoolV(b1), BoolV(b2)) => BoolV(b1 andalso b2)
                        | ("+", IntV(i1), IntV(i2)) => IntV(i1 + i2)
                        | ("-", IntV(i1), IntV(i2)) => IntV(i1 - i2)
                        | ("*", IntV(i1), IntV(i2)) => IntV(i1 * i2)
                        | ("/", IntV(i1), IntV(i2)) => IntV(i1 div i2)
                        | ("<", IntV(i1), IntV(i2)) => BoolV(i1 < i2)
                        | ("<=", IntV(i1), IntV(i2)) => BoolV(i1 <= i2)
                        | ("=", _, _) => BoolV(v1 = v2)
                        | ("!=", _, _) => BoolV(v1 <> v2)
                        | ("::", _, SeqV(l)) => SeqV(v1::l)
                        | (";", _, _) => v2
                    )
                end
            | Item(i, e1) =>
                let
                    val ListV(l) = eval e1 env
                    fun findIth (n, []) = raise ListOutOfRange
                        | findIth(n, h::t) = if n = i then h else findIth(n + 1, t)
                in
                    findIth(1, l)
                end
        );
(* use "myTest.sml"; *)