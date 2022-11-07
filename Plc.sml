(* Plc interpreter main file *)

fun run (e:expr) : string =
        let
            val t = teval e []
        in
            val2string (eval e []) ^ " : " ^ type2string t
        end;