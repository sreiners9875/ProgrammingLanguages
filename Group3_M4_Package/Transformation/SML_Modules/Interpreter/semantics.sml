(* =========================================================================================================== *)
structure Semantics =
struct


(* This makes contents of the Model structure directly accessible (i.e., the prefix "Model." is not needed. *)            
open Model; 
            
(* This makes the internal representation of parse trees directly accessible. *)            
open CONCRETE_REPRESENTATION;

(* The following tree structure, defined in the CONCERETE_REPRESENTATION structure, is used in the TL System:

    datatype NODE_INFO = info of { id : IntInf.int, line : int * int , column : int * int, label : string };
	datatype INODE = inode of string * NODE_INFO
	                 | ...  
															
	datatype ITREE = itree of INODE * ITREE list;
*)


(* =========================================================================================================== *)
(* Here is where you add the M and E (as well as any other) definitions you developed in M2. The primary challenge here
   is to translate the parse expression notation we used in M2 to the actual SML tree patterns used in the TL System. 
   
   Example:
   
   M1: <stmtList> ::= <stmt> ";" <stmList>
   
   M2: M( [[ stmt_1 ; stmtList_1 ]], m) = M(stmtList_1, M(stmt_1,m))
    
   M4: 
        M( itree(inode("stmtList",_),
                    [
                        stmt,       (* this is a regular variable in SML and has no other special meaning *)
                        semiColon,  (* this is a regular variable in SML and has no other special meaning *)
                        stmtList    (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        
        Note that the above M4 implementation will match ANY tree whose root is "stmtList" having three children.
        Such matches can be further constrained by explicitly exposing more of the tree structure.
        
        M( itree(inode("stmtList",_),
                    [
                        stmt,                       (* this is a regular variable in SML and has no other special meaning *)
                        itree(inode(";",_), [] ),   (* A semi-colon is a leaf node. All leaf nodes have an empty children list. *)
                        
                        stmtList                    (* this is a regular variable in SML and has no other special meaning *) 
                    ]
                ),
           m
           
        ) = M( stmtList, M(stmt, m) )  
        
        Note that the above M4 implementation will match ANY tree satisifying the following constraints:
            (1) the root is "stmtList"
            (2) the root has three children
            (3) the second child is a semi-colon   
*)

fun E'( itree(inode("expression",_), [logicalOr] ), m) = E'(logicalOr, m)

  (* LOGICAL OR *)
  | E'( itree(inode("logicalOr",_), [logicalOr, itree(inode("||",_), []), logicalAnd] ), m) =
        let
            val t1 = E'(logicalOr, m)
            val t2 = E'(logicalAnd, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL
            else ERROR
        end
        
  | E'( itree(inode("logicalOr",_), [logicalAnd] ), m) = E'(logicalAnd, m)
  
  (* LOGICAL AND *)
  | E'( itree(inode("logicalAnd",_), [logicalAnd, itree(inode("&&",_), []), equality] ), m) = 
        let
            val t1 = E'(logicalAnd, m)
            val t2 = E'(equality, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL
            else ERROR
        end
    
  | E'( itree(inode("logicalAnd",_), [equality] ), m) = E'(equality, m)
  
  (* EQUALITY *)
  | E'( itree(inode("equality",_), [equality, itree(inode("==",_), []), relational] ), m) =
        let
            val t1 = E'(equality, m)
            val t2 = E'(relational, m)
        in
            if t1 = BOOL andalso t2 = BOOL then BOOL
            else if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
        
  | E'( itree(inode("equality",_), [equality, itree(inode("!=",_), []), relational] ), m) =
        let
            val t1 = E'(equality, m)
            val t2 = E'(relational, m)
        in
            if t1 = BOOL andalso t2 = BOOL then BOOL
            else if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
        
  | E'( itree(inode("equality",_), [relational] ), m) = E'(relational, m)
    
  (* RELATIONAL *)
  | E'( itree(inode("relational",_), [relational, itree(inode(">",_), []), additive] ), m) =
        let
            val t1 = E'(relational, m)
            val t2 = E'(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
   
  | E'( itree(inode("relational",_), [relational, itree(inode(">=",_), []), additive] ), m) =
        let
            val t1 = E'(relational, m)
            val t2 = E'(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | E'( itree(inode("relational",_), [relational, itree(inode("<",_), []), additive] ), m) =
        let
            val t1 = E'(relational, m)
            val t2 = E'(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | E'( itree(inode("relational",_), [relational, itree(inode("<=",_), []), additive] ), m) =
        let
            val t1 = E'(relational, m)
            val t2 = E'(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | E'( itree(inode("relational",_), [additive] ), m) = E'(additive, m)
  
  (* ADDITIVE *)
  | E'( itree(inode("additive",_), [additive, itree(inode("+",_), []), multiplicative] ), m) =
        let
            val t1 = E'(additive, m)
            val t2 = E'(multiplicative, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("additive",_), [additive, itree(inode("-",_), []), multiplicative] ), m) =
        let
            val t1 = E'(additive, m)
            val t2 = E'(multiplicative, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("additive",_), [multiplicative] ), m) = E'(multiplicative, m)
  
  (* MULTIPLICATIVE *)
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("*",_), []), factor] ), m) =
        let
            val t1 = E'(multiplicative, m)
            val t2 = E'(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("/",_), []), factor] ), m) =
        let
            val t1 = E'(multiplicative, m)
            val t2 = E'(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("%",_), []), factor] ), m) =
        let
            val t1 = E'(multiplicative, m)
            val t2 = E'(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("multiplicative",_), [factor] ), m) = E'(factor, m)
  
  (* FACTOR *)
  | E'( itree(inode("factor",_), [itree(inode("~",_), []), factor] ), m) =
        let
            val t1 = E'(factor, m)
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("factor",_), [itree(inode("!",_), []), factor] ), m) =
        let
            val t1 = E'(factor, m)
        in
            if t1 = BOOL then BOOL
            else ERROR
        end
  
  | E'( itree(inode("factor",_), [exponent] ), m) = E'(exponent, m)
  
  (* EXPONENT *)
  | E'( itree(inode("exponent",_), [base, itree(inode("^",_), []), exponent] ), m) =
        let
            val t1 = E'(base, m)
            val t2 = E'(exponent, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("exponent",_), [base] ), m) = E'(base, m)
  
  (* BASE *)
  | E'( itree(inode("base",_), [itree(inode("(",_), []), expression, itree(inode(")",_), [])] ), m) = E'(expression, m)
  
  | E'( itree(inode("base",_), [itree(inode("|",_), []), expression, itree(inode("|",_), [])] ), m) = E'(expression, m)
  
  | E'( itree(inode("base",_), [modifiedId] ), m) = E'(modifiedId, m)
  
  (* MODIFIED ID *)
  | E'( itree(inode("modifiedId",_), [child] ), m) = E'(child, m)
  
  (* PREFIX *)
  | E'( itree(inode("prefix",_), [itree(inode("++",_), []), id] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("prefix",_), [itree(inode("--",_), []), id] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  (* POSTFIX *)
  | E'( itree(inode("postfix",_), [id, itree(inode("++",_), [])] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | E'( itree(inode("postfix",_), [id, itree(inode("--",_), [])] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  (* DATATYPES *)
  | E'( id as itree(inode("id",_), [_] ), m) = getType(accessEnv(getLeaf(id), m))

  | E'( itree(inode("integer",_), [_] ), m) = INT

  | E'( itree(inode("boolean",_), [_] ), m) = BOOL
  
  (* ERROR HANDLING *)
  | E' _ = raise Fail("Error in Model.E' - this should never occur")

fun M( itree(inode("prog",_), [ statementList ] ), m) = M(statementList, m)
  | M( itree(inode("statementList",_), [ statement, statementList ] ), m) = M(statementList, M(statement, m))
  | M( itree(inode("statementList",_), [ epsilon ] ), m) = m
  
  (* USED FOR ASSIGNMENT ";", DECLARATION ";", OUTPUT ";", PREFIX ";", POSTFIX ";", INITIALIZATION ";" *)
  | M( itree(inode("statement",_), [ child, itree(inode(";",_), [])] ), m) = M(child, m)
  
  (* USED FOR FORLOOP, WHILELOOP, IFTHEN, IFTHENELSE, BLOCK *)
  | M( itree(inode("statement",_), [ child] ), m) = M(child, m)
  
  (* DECLARATION *)
  
  | M( itree(inode("declaration",_), [itree(inode("int",_), []), id] ), m) = updateEnv(getLeaf(id), INT, Model.getCounter(m), m)
  | M( itree(inode("declaration",_), [itree(inode("bool",_), []), id] ), m) = updateEnv(getLeaf(id), BOOL, Model.getCounter(m), m)
  
  (* ASSIGNMENT *)
  
  | M( itree(inode("assignment",_), [id, itree(inode("=",_), []), expression] ), m) = 
        let
            val t1 = E'(expression, m)
            val t2 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = t2 then m
            else tError("Type error[assignment].")
        end
  
  (* INITIALIZATION *)
  | M( itree(inode("initialization",_), [itree(inode("int",_), []), id, itree(inode("=",_), []), expression] ), m) =
        let
            val m1 = updateEnv(getLeaf(id), INT, Model.getCounter(m), m)
            val t1 = E'(expression, m)
        in
            if t1 = INT then m1
            else tError("Type error[initialization].")
        end
        
  | M( itree(inode("initialization",_), [itree(inode("bool",_), []), id, itree(inode("=",_), []), expression] ), m) =
       let
            val m1 = updateEnv(getLeaf(id), BOOL, Model.getCounter(m), m)
            val t1 = E'(expression, m)
        in
            if t1 = BOOL then m1
            else tError("Type error[initialization].")
        end
  
  (* BLOCK *)
  | M( itree(inode("block",_), [itree(inode("{",_), []), statementList, itree(inode("}",_), [])] ), m) =
        let
            val m1 = M(statementList, m)
        in
            m
        end
  
  (* FORLOOP *)
  | M( itree(inode("forLoop",_), [itree(inode("for",_), []), itree(inode("(",_), []), forInitial, itree(inode(";",_), []), expression, itree(inode(";",_), []), modifiedId, itree(inode(")",_), []), block] ), m) =
        let
            val m1 = M(forInitial, m)
            val t1 = E'(expression, m1)
            val t2 = E'(modifiedId, m1)
            
            val m2 = M(block, m1)
        in
            if t1 = BOOL then m
            else tError("Type error[forLoop].")
        end
  
  (* FORINITIAL *)
  | M( itree(inode("forInitial",_), [itree(inode("int",_), []), id, itree(inode("=",_), []), expression] ), m) =
        let
            val m1 = updateEnv(getLeaf(id), INT, Model.getCounter(m), m)
            val t1 = E'(expression, m)
        in
            if t1 = INT then m1
            else tError("Type error[forInitial].")
        end
  
  (* WHILELOOP *)
  | M( itree(inode("whileLoop",_), [itree(inode("while",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m) =
        let
            val m1 = M(block, m)
            val t1 = E'(expression, m)
        in
            if t1 = BOOL then m
            else tError("Type error[whileLoop].")
        end
  
  (* IFTHEN *)
  | M( itree(inode("ifThen",_), [itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m) =
        let
            val t1 = E'(expression, m)
            val m1 = M(block, m)
        in
            if t1 = BOOL then m
            else tError("Type error[ifThen].")
        end
  
  (* IFTHENELSE *)
  | M( itree(inode("ifThenElse",_), [itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block0, itree(inode("else",_), []), block1 ] ), m) =
        let
            val t1 = E'(expression, m)
            val m1 = M(block0, m)
            val m2 = M(block1, m)
        in
            if t1 = BOOL then m
            else tError("Type error[ifThenElse].")
        end
  
  (* OUTPUT *)
  | M( itree(inode("output",_), [itree(inode("print",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []) ] ), m) =
        let
            val t1 = E'(expression, m)
        in
            if t1 = BOOL orelse t1 = INT then m
            else tError("Type error[output].")
        end
  
  (* ERROR HANDLING *)
  | M( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  | M _ = raise Fail("Error in Model.M - this should never occur")

(* =========================================================================================================== *)
end (* struct *)
(* =========================================================================================================== *)