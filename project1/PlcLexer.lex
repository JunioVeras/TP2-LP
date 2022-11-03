(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* Define what to do when the end of the file is reached. *)
val commentNextingLevel = ref 0;
val posi = ref 0
fun eof () =
   let
     val CNL = !commentNextingLevel
   in
     if CNL = 0 then
       Tokens.EOF(0,0)
  else
      (TextIO.output(TextIO.stdOut, "\n*** Unfinished Commentary! ***\n");raise Fail("Unfinished commentary."))
  end

(* A function to print a message error on the screen. *)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
                "line ", (Int.toString l), ": ", e, "\n"
            ])
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
        let
            val lineNum = !lineNumber
        in
            Int.toString lineNum
        end

(* Initialize the lexer. *)
fun init() = ()

(* DEFINITIONS *)
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit   = [0-9];
ws      = [\ \t];
name   = [A-Za-z_][A-Za-z_0-9]*;
comment = \(\*(.|\n)*\*\);
commentNF = \(\*(.|\n)*;

%%
\n       	=> (lineNumber := (!lineNumber) + 1; lex());

{comment}   => (lex());

";"      	=> (Tokens.SEMI(!posi, !posi));
","      	=> (Tokens.COMMA(!posi, !posi));
"("      	=> (Tokens.OPAR(!posi,!posi));
")"      	=> (Tokens.CPAR(!posi,!posi));
"["         => (Tokens.OBRACE(!posi,!posi));
"]"         => (Tokens.CBRACE(!posi,!posi));
"{"         => (Tokens.OBRACKET(!posi,!posi));
"}"         => (Tokens.CBRACKET(!posi,!posi));

{ws}+    	=> (lex());

"if"        => (Tokens.IF(!posi,!posi));
"then"      => (Tokens.THEN(!posi,!posi));
"else"      => (Tokens.ELSE(!posi,!posi));
"match"     => (Tokens.MATCH(!posi,!posi));
"with"      => (Tokens.WITH(!posi,!posi));
"!"         => (Tokens.NOT(!posi,!posi));
"hd"        => (Tokens.HD(!posi,!posi));
"tl"        => (Tokens.TL(!posi,!posi));
"ise"       => (Tokens.ISE(!posi,!posi));
"print"     => (Tokens.PRINT(!posi,!posi));
"&&"        => (Tokens.AND(!posi,!posi));
"+"      	=> (Tokens.PLUS(!posi,!posi));
"-"      	=> (Tokens.MINUS(!posi,!posi));
"*"      	=> (Tokens.TIMES(!posi,!posi));
"/"      	=> (Tokens.DIV(!posi,!posi));
"="      	=> (Tokens.EQ(!posi,!posi));
"!="     	=> (Tokens.DIF(!posi,!posi));
"<"      	=> (Tokens.LESS(!posi,!posi));
"<="     	=> (Tokens.LESSEQ(!posi,!posi));
"::"     	=> (Tokens.INFIX(!posi,!posi));

"true"   	=> (Tokens.BOOL(true, !posi, !posi));
"false" 	=> (Tokens.BOOL(false, !posi, !posi));

"Nil" 	    => (Tokens.NIL_T(!posi, !posi));
"Bool" 	    => (Tokens.BOOL_T(!posi, !posi));
"Int" 	    => (Tokens.INT_T(!posi, !posi));

"fn"      	=> (Tokens.FN(!posi, !posi));
"fun"      	=> (Tokens.FUN(!posi, !posi));
"end"      	=> (Tokens.END(!posi, !posi));
"rec"      	=> (Tokens.REC(!posi, !posi));
"->"      	=> (Tokens.ARROW(!posi, !posi));
"|"         => (Tokens.BAR(!posi, !posi));
"_"         => (Tokens.EMPTY(!posi, !posi));
":"         => (Tokens.COLON(!posi, !posi));
"=>"      	=> (Tokens.ANON_ARROW(!posi, !posi));

"var"      	=> (Tokens.VAR(!posi, !posi));

{name}      => (Tokens.NAME(yytext, !posi, !posi));
{digit}+ 	=> (Tokens.NUM(valOf (Int.fromString yytext), !posi, !posi));

.        	=> (error ("ignoring bad character " ^ yytext, !posi, !posi); lex());
