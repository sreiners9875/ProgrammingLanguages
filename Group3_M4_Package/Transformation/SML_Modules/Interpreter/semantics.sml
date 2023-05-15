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
            val(v1, m1) = E'(logicalOr, m)
        in
            if dnvToBool v1 then (v1, m1)
            else E'(logicalAnd, m1)
        end
        
  | E'( itree(inode("logicalOr",_), [logicalAnd] ), m) = E'(logicalAnd, m)
  
  (* LOGICAL AND *)
  | E'( itree(inode("logicalAnd",_), [logicalAnd, itree(inode("&&",_), []), equality] ), m) = 
        let
            val(v1, m1) = E'(logicalAnd, m)
        in
            if dnvToBool v1 then E'(equality, m1)
            else (v1, m1)
        end
    
  | E'( itree(inode("logicalAnd",_), [equality] ), m) = E'(equality, m)
  
  (* EQUALITY *)
  | E'( itree(inode("equality",_), [equality, itree(inode("==",_), []), relational] ), m) =
        let
            val(v1, m1) = E'(equality, m)
            val(v2, m2) = E'(relational, m1)
        in
            (Boolean (dnvToInt v1 = dnvToInt v2), m2)
        end
        
  | E'( itree(inode("equality",_), [equality, itree(inode("!=",_), []), relational] ), m) =
        let
            val(v1, m1) = E'(equality, m)
            val(v2, m2) = E'(relational, m1)
        in
            (Boolean (dnvToInt v1 <> dnvToInt v2), m2)
        end
        
  | E'( itree(inode("equality",_), [relational] ), m) = E'(relational, m)
    
  (* RELATIONAL *)
  | E'( itree(inode("relational",_), [relational, itree(inode(">",_), []), additive] ), m) =
        let
            val(v1, m1) = E'(relational, m)
            val(v2, m2) = E'(additive, m1)
        in
            (Boolean (dnvToInt v1 > dnvToInt v2), m2)
        end
   
  | E'( itree(inode("relational",_), [relational, itree(inode(">=",_), []), additive] ), m) =
        let
            val(v1, m1) = E'(relational, m)
            val(v2, m2) = E'(additive, m1)
        in
            (Boolean (dnvToInt v1 >= dnvToInt v2), m2)
        end
  
  | E'( itree(inode("relational",_), [relational, itree(inode("<",_), []), additive] ), m) =
        let
            val(v1, m1) = E'(relational, m)
            val(v2, m2) = E'(additive, m1)
        in
            (Boolean (dnvToInt v1 < dnvToInt v2), m2)
        end
  
  | E'( itree(inode("relational",_), [relational, itree(inode("<=",_), []), additive] ), m) =
        let
            val(v1, m1) = E'(relational, m)
            val(v2, m2) = E'(additive, m1)
        in
            (Boolean (dnvToInt v1 <= dnvToInt v2), m2)
        end
  
  | E'( itree(inode("relational",_), [additive] ), m) = E'(additive, m)
  
  (* ADDITIVE *)
  | E'( itree(inode("additive",_), [additive, itree(inode("+",_), []), multiplicative] ), m) =
        let
            val(v1, m1) = E'(additive, m)
            val(v2, m2) = E'(multiplicative, m1)
        in
            (Integer (dnvToInt v1 + dnvToInt v2), m2)
        end
  
  | E'( itree(inode("additive",_), [additive, itree(inode("-",_), []), multiplicative] ), m) =
        let
            val(v1, m1) = E'(additive, m)
            val(v2, m2) = E'(multiplicative, m1)
        in
            (Integer (dnvToInt v1 - dnvToInt v2), m2)
        end
  
  | E'( itree(inode("additive",_), [multiplicative] ), m) = E'(multiplicative, m)
  
  (* MULTIPLICATIVE *)
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("*",_), []), factor] ), m) =
        let
            val(v1, m1) = E'(multiplicative, m)
            val(v2, m2) = E'(factor, m1)
        in
            (Integer (dnvToInt v1 * dnvToInt v2), m2)
        end
  
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("/",_), []), factor] ), m) =
        let
            val(v1, m1) = E'(multiplicative, m)
            val(v2, m2) = E'(factor, m1)
        in
            (Integer (dnvToInt v1 div dnvToInt v2), m2)
        end
  
  | E'( itree(inode("multiplicative",_), [multiplicative, itree(inode("%",_), []), factor] ), m) =
        let
            val(v1, m1) = E'(multiplicative, m)
            val(v2, m2) = E'(factor, m1)
        in
            (Integer (dnvToInt v1 mod dnvToInt v2), m2)
        end
  
  | E'( itree(inode("multiplicative",_), [factor] ), m) = E'(factor, m)
  
  (* FACTOR *)
  | E'( itree(inode("factor",_), [itree(inode("~",_), []), factor] ), m) =
        let
            val(v1, m1) = E'(factor, m)
        in
            (Integer (~ (dnvToInt v1)), m1)
        end
  
  | E'( itree(inode("factor",_), [itree(inode("!",_), []), factor] ), m) =
        let
            val(v1, m1) = E'(factor, m)
        in
            (Boolean (not (dnvToBool v1)), m1)
        end
  
  | E'( itree(inode("factor",_), [exponent] ), m) = E'(exponent, m)
  
  (* EXPONENT *)
  | E'( itree(inode("exponent",_), [base, itree(inode("^",_), []), exponent] ), m) =
        let
            val(v1, m1) = E'(base, m)
            val(v2, m2) = E'(exponent, m1)
            
            fun power(x, 0) = 1
              | power(x, y) = x * power(x, y-1);
        in
            (Integer (power(dnvToInt v1, dnvToInt v2)), m2)
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
            val idLoc = getLoc(accessEnv(getLeaf(id), m))
            val idStore = dnvToInt (accessStore(idLoc, m))
            val m1 = updateStore(idLoc, Integer(idStore + 1), m)
        in
            (Integer (idStore + 1), m1)
        end
  
  | E'( itree(inode("prefix",_), [itree(inode("--",_), []), id] ), m) =
       let
            val idLoc = getLoc(accessEnv(getLeaf(id), m))
            val idStore = dnvToInt (accessStore(idLoc, m))
            val m1 = updateStore(idLoc, Integer(idStore - 1), m)
        in
            (Integer (idStore - 1), m1)
        end
  
  (* POSTFIX *)
  | E'( itree(inode("postfix",_), [id, itree(inode("++",_), [])] ), m) =
        let
            val idLoc = getLoc(accessEnv(getLeaf(id), m))
            val idStore = dnvToInt (accessStore(idLoc, m))
            val m1 = updateStore(idLoc, Integer(idStore + 1), m)
        in
            (Integer (idStore), m1)
        end
  
  | E'( itree(inode("postfix",_), [id, itree(inode("--",_), [])] ), m) =
        let
            val idLoc = getLoc(accessEnv(getLeaf(id), m))
            val idStore = dnvToInt (accessStore(idLoc, m))
            val m1 = updateStore(idLoc, Integer(idStore - 1), m)
        in
            (Integer (idStore), m1)
        end
  
  (* DATATYPES *)
  | E'( integer as itree(inode("integer",_), [_] ), m) = (Integer (valOf(Int.fromString((getLeaf(integer))))), m)
  | E'( boolean as itree(inode("boolean",_), [_] ), m) = (Boolean (valOf(Bool.fromString((getLeaf(boolean))))), m)
  | E'( id as itree(inode("id",_), [_] ), m) = 
    let
        val (t, l) = accessEnv(getLeaf(id), m)
        val v1 = accessStore(l, m)
    in
        (v1, m)
    end
  
  (* ERROR HANDLING *)
  | E' _ = raise Fail("Error in Model.E' - this should never occur")

fun M(  itree(inode("prog",_), 
                [ 
                    stmt_list
                ] 
             ), 
        m
    ) = m
        
  | M(  itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  
  | M _ = raise Fail("error in Semantics.M - this should never occur")

(*
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
            val(v1, m1) = E'(expression, m)
            val loc = getLoc(accessEnv(getLeaf(id)))
            val m2 = updateStore(loc, v1, m1)
        in
            m2
        end
  
  (* INITIALIZATION *)
  | M( itree(inode("initialization",_), [itree(inode("int",_), []), id, itree(inode("=",_), []), expression] ), m) =
        let
            val loc = getLoc(accessEnv(getLeaf(id))
            val m2 = updateEnv(id, INT, loc)
        in
            m2
        end
        
  | M( itree(inode("initialization",_), [itree(inode("bool",_), []), id, itree(inode("=",_), []), expression] ), m) =
       let
            val loc = getLoc(accessEnv(getLeaf(id))
            val m2 = updateEnv(id, BOOL, loc)
        in
            m2
        end
  
  (* BLOCK *)
  | M( itree(inode("block",_), [itree(inode("{",_), []), statementList, itree(inode("}",_), [])] ), m) =
        
  
  (* FORLOOP *)
  | M( itree(inode("forLoop",_), [itree(inode("for",_), []), itree(inode("(",_), []), forInitial, itree(inode(";",_), []), expression, itree(inode(";",_), []), modifiedId, itree(inode(")",_), []), block] ), m) =
        
  
  (* FORINITIAL *)
  | M( itree(inode("forInitial",_), [itree(inode("int",_), []), id, itree(inode("=",_), []), expression] ), m) =
        
  
  (* WHILELOOP *)
  | M( itree(inode("whileLoop",_), [itree(inode("while",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m) =
        
  
  (* IFTHEN *)
  | M( itree(inode("ifThen",_), [itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m) =
        
  
  (* IFTHENELSE *)
  | M( itree(inode("ifThenElse",_), [itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block0, itree(inode("else",_), []), block1 ] ), m) =
        
  
  (* OUTPUT *)
  | M( itree(inode("output",_), [itree(inode("print",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []) ] ), m) =
        
  
  (* ERROR HANDLING *)
  | M( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  | M _ = raise Fail("Error in Model.M - this should never occur")
  
  *)
(* =========================================================================================================== *)
end (* struct *)
(* =========================================================================================================== *)