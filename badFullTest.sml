(* Infrastructure to run the Plc interpreter*)

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
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";
use "testCases.sml";

val interpFile = TextIO.openAppend "Bad-Plc-Output";
val caseIdx = ref 1

fun getNth n i [] = []
	| getNth n i (h::t) =
		if n = i then h else getNth n i+1 t;

fun writeResult r = 
		let
			val res = run r
			val idx = !caseIdx
		in
			TextIO.output(interpFile, (Int.toString idx ^ ". " ^ res ^ "\n")); caseIdx := !caseIdx + 1
		end;

(* Test interpreter *)

writeResult (#2(getNth 3 0 bad))

TextIO.closeOut interpFile;

OS.Process.exit(OS.Process.success);
