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

fun teval (e:expr) (env: plcType env) : plcType = 
        case e of
            Var(x) => lookup env x
        | ConI(_) => IntT
        | ConB(_) => BoolT
        | ESeq(t)  => 
            (case t of 
                    (SeqT(a)) => SeqT(a)
                | _ => raise UnknownType
            )
        | List(l) => ListT(map (fn x => teval x env) l)
        | Let(x, e1, e2) =>
            let 
                val tX = teval e1 env
                val env' = (x, tX)::env
            in
                teval e2 env'
            end
        (* | Letrec(f, argT, argN, return) => *)
        | Prim2(opr, e1, e2) =>
            let 
                val t1 = teval e1 env
                val t2 = teval e2 env
            in 
                (case (opr, t1, t2) of
                        ("+", IntT, IntT) => IntT
                    | ("-", IntT, IntT) => IntT
                    | ("*", IntT, IntT) => IntT
                    | ("/", IntT, IntT) => IntT
                    | (";" , _ , _) => t2
                    | _ =>  raise UnknownType
                )
            end
        | _   =>  raise UnknownType

;