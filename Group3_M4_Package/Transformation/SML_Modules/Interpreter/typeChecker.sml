(* =========================================================================================================== *)
structure TypeChecker =
struct

open Model;
open CONCRETE_REPRESENTATION;

(* =========================================================================================================== *)
(*
    Here is where your typeCheck and typeOf definitions go. The primary challenge here is to translate the parse 
    expression notation we used in M2 to the actual SML tree patterns used in the TL System. See the comments in
    the semantics.sml file for a more detailed discussion on this topic. 
*)

(* FORMATTING *)

(* | typeCheck( itree(inode("",_), [ ] ), m) = *)

(* itree(inode("",_), []) *)

fun typeOf( itree(inode("expression",_), [logicalOr] ), m) = typeOf(logicalOr, m)

  (* LOGICAL OR *)
  | typeOf( itree(inode("logicalOr",_), [logicalOr, itree(inode("||",_), []), logicalAnd] ), m) =
        let
            val t1 = typeOf(logicalOr, m)
            val t2 = typeOf(logicalAnd, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL
            else ERROR
        end
        
  | typeOf( itree(inode("logicalOr",_), [logicalAnd] ), m) = typeOf(logicalAnd, m)
  
  (* LOGICAL AND *)
  | typeOf( itree(inode("logicalAnd",_), [logicalAnd, equality] ), m) = 
        let
            val t1 = typeOf(logicalAnd, m)
            val t2 = typeOf(equality, m)
        in
            if t1 = t2 andalso t1 = BOOL then BOOL
            else ERROR
        end
    
  | typeOf( itree(inode("logicalAnd",_), [equality] ), m) = typeOf(equality, m)
  
  (* EQUALITY *)
  | typeOf( itree(inode("equality",_), [equality, itree(inode("==",_), []), relational] ), m) =
        let
            val t1 = typeOf(equality, m)
            val t2 = typeOf(relational, m)
        in
            if t1 = BOOL andalso t2 = BOOL then BOOL
            else if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
        
  | typeOf( itree(inode("equality",_), [equality, itree(inode("!=",_), []), relational] ), m) =
        let
            val t1 = typeOf(equality, m)
            val t2 = typeOf(relational, m)
        in
            if t1 = BOOL andalso t2 = BOOL then BOOL
            else if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
        
  | typeOf( itree(inode("equality",_), [relational] ), m) = typeOf(relational, m)
    
  (* RELATIONAL *)
  | typeOf( itree(inode("relational",_), [relational, itree(inode(">",_), []), additive] ), m) =
        let
            val t1 = typeOf(relational, m)
            val t2 = typeOf(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
   
  | typeOf( itree(inode("relational",_), [relational, itree(inode(">=",_), []), additive] ), m) =
        let
            val t1 = typeOf(relational, m)
            val t2 = typeOf(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | typeOf( itree(inode("relational",_), [relational, itree(inode("<",_), []), additive] ), m) =
        let
            val t1 = typeOf(relational, m)
            val t2 = typeOf(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | typeOf( itree(inode("relational",_), [relational, itree(inode("<=",_), []), additive] ), m) =
        let
            val t1 = typeOf(relational, m)
            val t2 = typeOf(additive, m)
        in
            if t1 = INT andalso t2 = INT then BOOL
            else ERROR
        end
  
  | typeOf( itree(inode("relational",_), [additive] ), m) = typeOf(additive, m)
  
  (* ADDITIVE *)
  | typeOf( itree(inode("additive",_), [additive, itree(inode("+",_), []), multiplicative] ), m) =
        let
            val t1 = typeOf(additive, m)
            val t2 = typeOf(multiplicative, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("additive",_), [additive, itree(inode("-",_), []), multiplicative] ), m) =
        let
            val t1 = typeOf(additive, m)
            val t2 = typeOf(multiplicative, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("additive",_), [multiplicative] ), m) = typeOf(multiplicative, m)
  
  (* MULTIPLICATIVE *)
  | typeOf( itree(inode("multiplicative",_), [multiplicative, itree(inode("*",_), []), factor] ), m) =
        let
            val t1 = typeOf(multiplicative, m)
            val t2 = typeOf(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("multiplicative",_), [multiplicative, itree(inode("/",_), []), factor] ), m) =
        let
            val t1 = typeOf(multiplicative, m)
            val t2 = typeOf(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("multiplicative",_), [multiplicative, itree(inode("&",_), []), factor] ), m) =
        let
            val t1 = typeOf(multiplicative, m)
            val t2 = typeOf(factor, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("multiplicative",_), [factor] ), m) = typeOf(factor, m)
  
  (* FACTOR *)
  | typeOf( itree(inode("factor",_), [itree(inode("~",_), []), factor] ), m) =
        let
            val t1 = typeOf(factor, m)
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("factor",_), [itree(inode("!",_), []), factor] ), m) =
        let
            val t1 = typeOf(factor, m)
        in
            if t1 = BOOL then BOOL
            else ERROR
        end
  
  | typeOf( itree(inode("factor",_), [exponent] ), m) = typeOf(exponent, m)
  
  (* EXPONENT *)
  | typeOf( itree(inode("exponent",_), [base, itree(inode("^",_), []), exponent] ), m) =
        let
            val t1 = typeOf(base, m)
            val t2 = typeOf(exponent, m)
        in
            if t1 = INT andalso t2 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("exponent",_), [base] ), m) = typeOf(base, m)
  
  (* BASE *)
  | typeOf( itree(inode("base",_), [itree(inode("(",_), []), expression, itree(inode(")",_), [])] ), m) = typeOf(expression, m)
  
  | typeOf( itree(inode("base",_), [itree(inode("|",_), []), expression, itree(inode("|",_), [])] ), m) = typeOf(expression, m)
  
  | typeOf( itree(inode("base",_), [modifiedId] ), m) = typeOf(modifiedId, m)
  
  (* MODIFIED ID *)
  | typeOf( itree(inode("modifiedId",_), [child] ), m) = typeOf(child, m)
  
  (* PREFIX *)
  | typeOf( itree(inode("prefix",_), [itree(inode("++",_), []), id] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("prefix",_), [itree(inode("--",_), []), id] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  (* POSTFIX *)
  | typeOf( itree(inode("postfix",_), [id, itree(inode("++",_), [])] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  | typeOf( itree(inode("postfix",_), [id, itree(inode("--",_), [])] ), m) =
        let
            val t1 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = INT then INT
            else ERROR
        end
  
  (* DATATYPES *)
  | typeOf( id as itree(inode("id",_), [_] ), m) = getType(accessEnv(getLeaf(id), m))

  | typeOf( itree(inode("integer",_), [_] ), m) = INT

  | typeOf( itree(inode("boolean",_), [_] ), m) = BOOL
  
  (* ERROR HANDLING *)
  | typeOf _ = raise Fail("Error in Model.typeOf - this should never occur")

fun typeCheck( itree(inode("prog",_), [ statementList ] ), m) = typeCheck(statementList, m)
  | typeCheck( itree(inode("statementList",_), [ statement, statementList ] ), m) = typeCheck(statementList, typeCheck(statement, m))
  | typeCheck( itree(inode("statementList",_), [ epsilon ] ), m) = m
  
  (* USED FOR ASSIGNMENT ";", DECLARATION ";", OUTPUT ";", PREFIX ";", POSTFIX ";", INITIALIZATION ";" *)
  | typeCheck( itree(inode("statement",_), [ child, itree(inode(";",_), [])] ), m) = typeCheck(child, m)
  
  (* USED FOR FORLOOP, WHILELOOP, IFTHEN, IFTHENELSE, BLOCK *)
  | typeCheck( itree(inode("statement",_), [ child] ), m) = typeCheck(child, m)
  
  | typeCheck( itree(inode("declaration",_), [itree(inode("int",_), []), id] ), m) = updateEnv(getLeaf(id), INT, Model.getCounter(m), m)
  | typeCheck( itree(inode("declaration",_), [itree(inode("bool",_), []), id] ), m) = updateEnv(getLeaf(id), BOOL, Model.getCounter(m), m)
  
  | typeCheck( itree(inode("assignment",_), [id, itree(inode("=",_), []), expression] ), m) = 
        let
            val t1 = typeOf(expression, m)
            val t2 = getType(accessEnv(getLeaf(id), m))
        in
            if t1 = t2 then m
            else tError("Type error[assignment].")
        end
  
  (* ERROR HANDLING *)
  | typeCheck( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  | typeCheck _ = raise Fail("Error in Model.typeCheck - this should never occur")
  
(* =========================================================================================================== *)  
end (* struct *)
(* =========================================================================================================== *)