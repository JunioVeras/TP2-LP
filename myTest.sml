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
(* use "PlcInterp.sml";
use "Plc.sml"; *)

TextIO.output(TextIO.stdOut, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");

(* val abs = fromFile "PlcPrograms/Prog1.plc"; *)
val abs = fromString "ise ((fn () => 1 end )::([Nil -> Int] []))";
val result = teval abs [];