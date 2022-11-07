(* Infrastructure to run the Plc interpreter *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "project1/Environ.sml";
use "project1/Absyn.sml";
use "project1/PlcParserAux.sml";
use "project1/PlcParser.yacc.sig";
use "project1/PlcParser.yacc.sml";
use "project1/PlcLexer.lex.sml";
use "project1/Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;
TextIO.output(TextIO.stdOut, "\n\n");
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";
use "testCases.sml";

TextIO.output(TextIO.stdOut, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");

val interpFile = TextIO.openAppend "Plc-Output";
val caseIdx = ref 1

fun getNth (n: int) (i: int) [] = raise ERRO_NOSSO1
    | getNth (n) (i) (h::t) =
        if n = i then h else getNth (n) (i+1) (t);

fun writeResult r = 
        let
            val res = run r
            val idx = !caseIdx
        in
            TextIO.output(TextIO.stdOut, (Int.toString idx ^ ". " ^ res ^ "\n")); caseIdx := !caseIdx + 1
        end;

(* Test interpreter *)
(* map (fn x => writeResult (#2(x))) cases; *)
(* writeResult (#2(getNth 1 1 cases)); *)

(* val abs = fromFile "tests/t1.plc"; *)
val abs = fromString "var x = 8; -x";
val result = run abs;