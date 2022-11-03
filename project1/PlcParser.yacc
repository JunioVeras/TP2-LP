datatype decl = varDecl of string * expr 
                | funDecl of string * (plcType * string) list * expr 
                | funrecDecl of string * (plcType * string) list * plcType * expr; 

(* Convert a list into a string *)
fun myList2string (conv, l) =
    case l of
      [] => ""
    | h::[] => conv(h)
    | h::ts => conv(h) ^ ", " ^ myList2string (conv, ts);

fun myType2string t =
    case t of
      BoolT => "BoolT"
    | IntT => "IntT"
    | ListT [] => "ListT []"
    | ListT ts => "ListT([" ^ myList2string (myType2string, ts) ^ "])"
    | SeqT t1 => "SeqT(" ^ myType2string(t1) ^ ")"
    | FunT (t1, t2) => "FunT(" ^ myType2string(t1) ^ ", " ^ myType2string(t2) ^ ")";

fun exp2string (ConI(i)) = "ConI(" ^ Int.toString(i) ^ ")"
  | exp2string (ConB(b)) = "ConB(" ^ Bool.toString(b) ^ ")"
  | exp2string (ESeq(t)) = "ESeq(" ^ myType2string(t) ^ ")"
  | exp2string (Var(x)) = "Var(\"" ^ x ^ "\")"
  | exp2string (Let(x, e1, e2)) = "Let(\"" ^ x ^ "\", " ^ exp2string(e1) ^ ", " ^ exp2string(e2) ^ ")"
  | exp2string (Letrec(f, typeL, varName, return, e1, e2)) = "Letrec(\"" ^ f ^ "\", " ^ myType2string(typeL) ^ ", \"" ^ varName ^ "\", " ^ myType2string(return) ^ ", " ^ exp2string(e1) ^ ", " ^ exp2string(e2) ^ ")"
  | exp2string (Prim1(operation, x)) = "Prim1(\"" ^ operation ^ "\", " ^ exp2string(x) ^ ")"
  | exp2string (Prim2(operation, x, y)) = "Prim2(\"" ^ operation ^ "\", " ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ")"
  | exp2string (If(x, y, z)) = "If(" ^ exp2string(x) ^ ", " ^ exp2string(y) ^ ", " ^ exp2string(z) ^ ")"
  | exp2string (Match(e, l)) = (
        let fun options2string (SOME(e1), e) = "(SOME(" ^ exp2string(e1) ^ "), " ^ exp2string(e) ^ ")"
              | options2string (NONE, e) = "(NONE, " ^ exp2string(e) ^ ")"
        in 
              "Match(" ^ exp2string(e) ^ ", [" ^ myList2string(options2string , l) ^ "])"
        end)
  | exp2string (Call(f, arg)) = "Call(" ^ exp2string(f) ^ ", " ^ exp2string(arg) ^ ")"
  | exp2string (List(l)) = "List([" ^ myList2string(exp2string, l) ^ "])"
  | exp2string (Item(i, x)) = "Item(" ^ Int.toString(i) ^ ", " ^ exp2string(x) ^ ")"
  | exp2string (Anon(typeL, name, e)) = "Anon(" ^ myType2string(typeL) ^ ", \"" ^ name ^ "\", " ^ exp2string(e) ^ ")";

fun convertDecl (varDecl(name, e)) Prog = Let(name, e, Prog)
  | convertDecl (funDecl(name, types, e)) Prog = Let(name, makeAnon(types, e), Prog)
  | convertDecl (funrecDecl(name, types, returnType, e)) Prog = makeFun(name, types, returnType, e, Prog);

%%

%name PlcParser
%pos int

%right SEMI ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ DIF
%left LESS LESSEQ
%right INFIX
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HD TL ISE PRINT
%left OBRACE

%term   NUM of int      |
        PLUS            |
        MINUS           |
        TIMES           |
        DIV             |
        EQ              |
        DIF             |
        LESS            |
        LESSEQ          |

        BOOL of bool    |
        AND             |
        NOT             |

        HD              |
        TL              |
        ISE             |
        INFIX           |

        PRINT           |

        OPAR            |
        CPAR            |
        OBRACE          |
        CBRACE          |
        OBRACKET        |
        CBRACKET        |
        SEMI            |
        COMMA           |

        NAME of string  |
        VAR             |
        FN              |
        FUN             |
        END             |
        REC             |
        ARROW           |
        BAR             |
        EMPTY           |
        COLON           |
        ANON_ARROW      |

        IF              |
        THEN            |
        ELSE            |
        MATCH           |
        WITH            |

        NIL_T           |
        BOOL_T          |
        INT_T           |

        EOF

%nonterm    Start of expr       |
            Prog of expr        |
            Decl of decl        |
            Expr of expr        |
            AtomExpr of expr    |
            Const of expr       |
            Type of plcType     |
            AtomType of plcType |
            Types of plcType    |
            AppExpr of expr     |
            Comps of expr       |
            Args of (plcType * string) list             |
            TypedVar of (plcType * string)              |
            Params of (plcType * string) list           |
            MatchExpr of (expr option * expr) list      |
            CondExpr of expr option

%eop EOF
%noshift EOF
%start Start

%%

Start   : Prog                  (Prog)

Prog    : Expr                  (Expr)
        | Decl SEMI Prog        (convertDecl Decl Prog)

Decl    : VAR NAME EQ Expr      (varDecl(NAME, Expr))
        | FUN NAME Args EQ Expr (funDecl(NAME, Args, Expr))
        | FUN REC NAME Args COLON Type EQ Expr (funrecDecl(NAME, Args, Type, Expr))

Expr    : AtomExpr              (AtomExpr)
        | AppExpr               (AppExpr)
        | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
        | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
        | NOT Expr              (Prim1("!", Expr))
        | MINUS Expr            (Prim1("-", Expr))
        | HD Expr               (Prim1("hd", Expr))
        | TL Expr               (Prim1("tl", Expr))
        | ISE Expr              (Prim1("ise", Expr))
        | PRINT Expr            (Prim1("print", Expr))
        | Expr AND Expr         (Prim2("&&", Expr1, Expr2))
        | Expr PLUS Expr        (Prim2("+", Expr1, Expr2))
        | Expr MINUS Expr       (Prim2("-", Expr1, Expr2))
        | Expr TIMES Expr       (Prim2("*", Expr1, Expr2))
        | Expr DIV Expr         (Prim2("/", Expr1, Expr2))
        | Expr EQ Expr          (Prim2("=", Expr1, Expr2))
        | Expr DIF Expr         (Prim2("!=", Expr1, Expr2))
        | Expr LESS Expr        (Prim2("<", Expr1, Expr2))
        | Expr LESSEQ Expr      (Prim2("<=", Expr1, Expr2))
        | Expr INFIX Expr       (Prim2("::", Expr1, Expr2))
        | Expr SEMI Expr        (Prim2(";", Expr1, Expr2))
        | Expr OBRACE NUM CBRACE(Item(NUM, Expr))

AtomExpr: Const                 (Const)
        | NAME                  (Var(NAME))
        | OBRACKET Prog CBRACKET(Prog)
        | OPAR Expr CPAR        (Expr)
        | OPAR Comps CPAR       (Comps)
        | FN Args ANON_ARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr     (Call(AtomExpr1, AtomExpr2))
        | AppExpr AtomExpr      (Call(AppExpr, AtomExpr))

Const   : BOOL                  (ConB(BOOL))
        | NUM                   (ConI(NUM))
        | OPAR CPAR             (List([]))
        | OPAR Type OBRACE CBRACE CPAR (ESeq(Type))

Comps   : Expr COMMA Expr       (List([Expr1, Expr2]))
        | Expr COMMA Comps      (List(Expr::(let val List(l) = Comps in l end)))

MatchExpr:END                   ([])
        | BAR CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr                  (SOME(Expr))
        | EMPTY                 (NONE)

Args    : OPAR CPAR             ([])
        | OPAR Params CPAR      (Params)

Params  : TypedVar              ([TypedVar])
        | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME             ((Type, NAME))

Type    : AtomType              (AtomType)
        | OPAR Types CPAR       (Types)
        | OBRACE Type CBRACE    (SeqT(Type))
        | Type ARROW Type       (FunT(Type1, Type2))

AtomType: NIL_T                 (ListT [])
        | BOOL_T                (BoolT)
        | INT_T                 (IntT)
        | OPAR Type CPAR        (Type)

Types   : Type COMMA Type       (ListT([Type1, Type2]))
        | Type COMMA Types      (ListT(Type::(let val ListT(l) = Types in l end)))