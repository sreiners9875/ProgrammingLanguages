(*#line 33.10 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*)functor Target_LexFn(val getNextTokenPos : string -> {line: word, column: word})(*#line 1.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*)(* ============================================================================================== *) 
datatype lexresult	= SHELL of string * string * {line: word, column: word};
val error 			= fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof 			= fn () => SHELL("","eof",getNextTokenPos(""))
(* ============================================================================================== *)
(* ------------------------------------------------------------------ *)
(* assumes that ">" does not occur as part of a nonterminal symbol *)
fun generateSchemaTokenName( yytext ) =
    let
		fun split(x, []   ) =  raise General.Fail("an_error")
		  | split(x, y::ys) = if x=y then ys else split(x,ys);
													
		fun splitFirst(symbol,[])    = 	[] (* symbol was not in the input list *)
		  | splitFirst(symbol,x::xs) = 	if x = symbol 
						then (* found split point *)
							[]
						else (* keep looking      *)
							x::splitFirst(symbol,xs);
																		
        val s0   = explode(yytext);
        val s1   = split(#"<",s0);
        val s2   = splitFirst(#">",s1);  
    in
        implode(explode("!#schema_variable_") @ s2)        
    end;

(* ------------------------------------------------------------------ *) 

(* ============================================================================================== *) 

(*#line 36.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\072\073\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\072\070\003\003\003\069\067\003\066\065\064\062\061\059\003\058\
\\056\054\054\054\054\054\054\054\054\054\003\053\051\049\047\003\
\\003\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\044\003\003\043\003\
\\003\009\039\009\009\035\029\009\009\025\009\009\009\009\009\009\
\\020\009\009\009\016\009\009\011\009\009\009\008\006\005\004\003\
\\003"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\012\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\013\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\014\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\015\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\017\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\018\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\019\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\021\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\022\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\023\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\024\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\028\010\010\010\010\010\010\010\026\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\027\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\032\010\010\010\010\010\010\010\010\010\010\010\010\010\030\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\031\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\033\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\034\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\036\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\037\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\038\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\040\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\041\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\042\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (44, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\045\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (47, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (49, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\055\055\055\055\055\055\055\055\055\055\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (56, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\057\057\057\057\057\057\057\057\057\057\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\060\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (62, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\063\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (67, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (70, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (72, 
"\000\000\000\000\000\000\000\000\000\073\073\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\073\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 123)], trans = 0},
{fin = [(N 9),(N 123)], trans = 0},
{fin = [(N 61),(N 123)], trans = 0},
{fin = [(N 53),(N 123)], trans = 6},
{fin = [(N 49)], trans = 0},
{fin = [(N 59),(N 123)], trans = 0},
{fin = [(N 114),(N 123)], trans = 9},
{fin = [(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 11},
{fin = [(N 114)], trans = 12},
{fin = [(N 114)], trans = 13},
{fin = [(N 114)], trans = 14},
{fin = [(N 84),(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 16},
{fin = [(N 114)], trans = 17},
{fin = [(N 114)], trans = 18},
{fin = [(N 104),(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 20},
{fin = [(N 114)], trans = 21},
{fin = [(N 114)], trans = 22},
{fin = [(N 114)], trans = 23},
{fin = [(N 94),(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 25},
{fin = [(N 114)], trans = 26},
{fin = [(N 65),(N 114)], trans = 9},
{fin = [(N 73),(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 29},
{fin = [(N 114)], trans = 30},
{fin = [(N 88),(N 114)], trans = 9},
{fin = [(N 114)], trans = 32},
{fin = [(N 114)], trans = 33},
{fin = [(N 114)], trans = 18},
{fin = [(N 114),(N 123)], trans = 35},
{fin = [(N 114)], trans = 36},
{fin = [(N 114)], trans = 37},
{fin = [(N 78),(N 114)], trans = 9},
{fin = [(N 114),(N 123)], trans = 39},
{fin = [(N 114)], trans = 40},
{fin = [(N 114)], trans = 41},
{fin = [(N 70),(N 114)], trans = 9},
{fin = [(N 17),(N 123)], trans = 0},
{fin = [(N 123)], trans = 44},
{fin = [], trans = 45},
{fin = [(N 118)], trans = 0},
{fin = [(N 31),(N 123)], trans = 47},
{fin = [(N 37)], trans = 0},
{fin = [(N 1),(N 123)], trans = 49},
{fin = [(N 40)], trans = 0},
{fin = [(N 29),(N 123)], trans = 51},
{fin = [(N 34)], trans = 0},
{fin = [(N 57),(N 123)], trans = 0},
{fin = [(N 108),(N 111),(N 123)], trans = 54},
{fin = [(N 108),(N 111)], trans = 54},
{fin = [(N 108),(N 111),(N 123)], trans = 56},
{fin = [(N 111)], trans = 56},
{fin = [(N 13),(N 123)], trans = 0},
{fin = [(N 21),(N 123)], trans = 59},
{fin = [(N 27)], trans = 0},
{fin = [(N 55),(N 123)], trans = 0},
{fin = [(N 19),(N 123)], trans = 62},
{fin = [(N 24)], trans = 0},
{fin = [(N 11),(N 123)], trans = 0},
{fin = [(N 5),(N 123)], trans = 0},
{fin = [(N 3),(N 123)], trans = 0},
{fin = [(N 51),(N 123)], trans = 67},
{fin = [(N 46)], trans = 0},
{fin = [(N 15),(N 123)], trans = 0},
{fin = [(N 7),(N 123)], trans = 70},
{fin = [(N 43)], trans = 0},
{fin = [(N 121),(N 123)], trans = 72},
{fin = [(N 121)], trans = 72}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => let val yytext=yymktext() in (*#line 46.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 600.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 104 => let val yytext=yymktext() in (*#line 81.23 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL("boolean", yytext, getNextTokenPos(yytext)) (*#line 602.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 108 => let val yytext=yymktext() in (*#line 82.23 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL("integer", yytext, getNextTokenPos(yytext)) (*#line 604.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 11 => let val yytext=yymktext() in (*#line 51.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 606.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 111 => let val yytext=yymktext() in (*#line 83.23 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL("integer"   , yytext,     getNextTokenPos(yytext))    (*#line 608.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 114 => let val yytext=yymktext() in (*#line 84.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL("id", yytext, getNextTokenPos(yytext)) (*#line 610.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 118 => let val yytext=yymktext() in (*#line 86.35 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL("" , yytext, getNextTokenPos(yytext))    (*#line 612.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 121 => let val yytext=yymktext() in (*#line 87.23 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) getNextTokenPos(yytext); lex() (*#line 614.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 123 => let val yytext=yymktext() in (*#line 89.23 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) error("ignored an unprintable character: " ^ yytext); getNextTokenPos(yytext); lex() (*#line 616.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 13 => let val yytext=yymktext() in (*#line 52.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 618.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 53.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 620.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 17 => let val yytext=yymktext() in (*#line 54.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 622.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 19 => let val yytext=yymktext() in (*#line 55.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 624.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 21 => let val yytext=yymktext() in (*#line 56.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 626.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 24 => let val yytext=yymktext() in (*#line 57.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 628.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 27 => let val yytext=yymktext() in (*#line 58.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 630.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 29 => let val yytext=yymktext() in (*#line 59.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 632.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 3 => let val yytext=yymktext() in (*#line 47.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 634.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 31 => let val yytext=yymktext() in (*#line 60.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 636.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 34 => let val yytext=yymktext() in (*#line 61.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 638.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 37 => let val yytext=yymktext() in (*#line 62.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 640.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 40 => let val yytext=yymktext() in (*#line 63.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 642.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 43 => let val yytext=yymktext() in (*#line 64.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 644.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 46 => let val yytext=yymktext() in (*#line 65.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 646.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 49 => let val yytext=yymktext() in (*#line 66.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 648.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 5 => let val yytext=yymktext() in (*#line 48.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 650.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 51 => let val yytext=yymktext() in (*#line 67.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 652.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 53 => let val yytext=yymktext() in (*#line 68.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 654.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 55 => let val yytext=yymktext() in (*#line 69.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 656.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 57 => let val yytext=yymktext() in (*#line 70.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 658.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 59 => let val yytext=yymktext() in (*#line 71.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 660.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 61 => let val yytext=yymktext() in (*#line 72.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 662.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 65 => let val yytext=yymktext() in (*#line 73.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 664.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 7 => let val yytext=yymktext() in (*#line 49.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 666.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 70 => let val yytext=yymktext() in (*#line 74.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 668.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 73 => let val yytext=yymktext() in (*#line 75.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 670.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 78 => let val yytext=yymktext() in (*#line 76.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 672.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 84 => let val yytext=yymktext() in (*#line 77.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 674.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 88 => let val yytext=yymktext() in (*#line 78.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 676.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 9 => let val yytext=yymktext() in (*#line 50.11 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 678.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| 94 => let val yytext=yymktext() in (*#line 79.15 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec"*) SHELL(yytext, yytext, getNextTokenPos(yytext)) (*#line 680.1 "C:\Users\spenc\Desktop\Folders\UNO\UNO_Spring_2023\Principles_of_Programming_Languages\Group3_M4\ProgrammingLanguages\Group3_M4_Package\Transformation\bin\target_tokens.spec.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
