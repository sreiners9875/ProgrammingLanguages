(* =========================================================================================================== *)
structure Semantics =
struct


(* This makes contents of the Model structure directly accessible (i.e., the prefix "Model." is not needed. *)            
open Model
open TypeChecker
(* This makes the internal representation of parse trees directly accessible. *)            
open CONCRETE_REPRESENTATION;

(*  Start ignore  - created to test model *)
fun ExpUnaryLogical(logicalOr, m0) = (logicalOr, m0)
fun ExpBinaryLogical(logicalOr,opr:string , logicalAnd, m0) = 
    let
        val (v1, m1) = ExpUnaryLogical(logicalOr, m0)
        val (v2, m2) = ExpUnaryLogical(logicalAnd, m1)
    in
        if opr = "||" then
            if v1 then (Bool.toString(v1), m1)
            else
                (Bool.toString(v2), m2)
        else 
            if opr = "&&" then
                if not v1 then (Bool.toString(v1), m1)
                else
                    (Bool.toString(v2), m2)
            else
                if opr = "==" then
                    (Bool.toString(v1 = v2), m2)
                else
                if opr = "!=" then
                    (Bool.toString(v1 <> v2), m2)
                    else ("error", m2)
            
    end
    
fun logialNegation(factor,m0) =
    let
        val (v1, m1) = ExpUnaryLogical(factor, m0)
    in
        (not v1, m1)
    end    
fun ExpUnaryComparison(equality, m0) = (equality, m0)

fun ExpBinaryComparison(equality, opr:string, relational, m0) =
    let
        val (v1, m1) = ExpUnaryComparison(equality, m0)
        val (v2, m2) = ExpUnaryComparison(relational, m1)
    in
        if opr = "==" then (Bool.toString(v1 = v2), m2)
        else
            if opr = "!=" then (Bool.toString(v1 <> v2), m2)
            else
                if opr = "<" then (Bool.toString(v1 < v2), m2)
                else
                    if opr = ">" then (Bool.toString(v1 > v2), m2)
                    else
                        if opr = "<=" then (Bool.toString(v1 <= v2), m2)
                    else
                        if opr = ">=" then (Bool.toString(v1 >= v2), m2)
                        else 
                            ("error", m2)                  
    end
    
fun ExpMathematicalUnary(multiplicative, m0) = (multiplicative, m0)
fun ExpMathematicalBinary(additive, opr:string, multiplicative, m0) = 
    let
        val (v1, m1) = ExpMathematicalUnary(additive, m0)
        val (v2, m2) = ExpMathematicalUnary(multiplicative, m1)
    in
        if opr = "+" then (Int.toString(v1 + v2), m2)
        else
            if opr = "-" then (Int.toString(v1 - v2), m2)
            else
                if opr = "*" then (Int.toString(v1 * v2), m2)
                else
                    if opr = "/" then (Int.toString(v1 div v2), m2)
                    else
                        if opr = "%" then (Int.toString(v1 mod v2), m2)
                    else
                        if opr = "^" then
                            let
                                 fun computeExp(x,y) =
                                     if y = 0 then 1
                                     else
                                         x * computeExp(x, y-1)
                                val v3 = computeExp (v1, v2)

                            in
                                (Int.toString(v3), m2)
                            end
                        else 
                            ("error", m2) 
    end
    
fun negativeInteger(opr:string ,factor,m0) =
    if opr = "~" then
        let
            val (v1, m1) = ExpMathematicalUnary(factor, m0)
        in
            (Int.toString(~v1), m1)
        end
    else
    ("error", m0)

fun ExpGetValOfId(id, m0) =
    let
        val loc = getLoc(accessEnv(id, m0))
        val v1 = accessStore(loc, m0)
    in
        (v1, m0)
    end
        
fun ExpPreInc(id, m0) =
    let
        val(v1, m1) = ExpGetValOfId(id, m0)
        val v2 = valOf(Int.fromString(v1)) + 1
        val loc = getLoc(accessEnv(id, m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
    in
        (v2, m2)
    end       
fun ExpPreDec(id, m0) =
        let
            val(v1, m1) = ExpGetValOfId(id, m0)
            val v2 = valOf(Int.fromString(v1)) - 1
        val loc = getLoc(accessEnv(id, m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
        in
            (v2, m2)
        end
fun ExpPostInc(id, m0) =
    let
        val(v1, m1) = ExpGetValOfId(id, m0)
        val v2 = valOf(Int.fromString(v1)) + 1
        val loc = getLoc(accessEnv(id, m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)

    in
        (v1, m2)
    end       
fun ExpPostDec(id, m0) =
    let
        val(v1, m1) = ExpGetValOfId(id, m0)
        val v2 = valOf(Int.fromString(v1)) - 1
        val loc = getLoc(accessEnv(id, m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
    in
        (v1, m2)
    end

fun declaration(typ: string, id:string , m0) =
    let
        val m1 = updateEnv(id, typ, ~1, m0)
    in
        m1
    end
fun boolAssignment(id, expression, m0) =
        let
            val (v1, m1) = ExpUnaryLogical(Bool.fromString(expression), m0)
            val loc = getLoc(accessEnv( id, m1))
            val m2 = updateStore(loc, Bool.toString(valOf v1), m1)
        in
            m2 
        end
fun intAssignment(id, expression, m0) =
    let
        val (v1, m1) = ExpMathematicalUnary(Int.fromString(expression), m0)
        val loc = getLoc(accessEnv( id, m1))
        val m2 = updateStore(loc, Int.toString(valOf v1), m1)
    in
        m2
    end

        
fun intInitialization(id, expression, m0) =
    let
        val m1 = declaration("int", id:string , m0)
        val m2 = intAssignment(id, expression, m1)
    in
        m2
    end
    
fun boolInitialization(id, expression, m0) =
    let
        val m1 = declaration("bool", id , m0)
        val m2 = boolAssignment(id, expression, m1)
    in
        m2
    end
fun ModPreInc(id, m0) =
    let
        val (v1, m1) = ExpPreInc(id, m0)
    in
        m1
    end
fun ModPreDec(id, m0) =
    let
        val (v1, m1) = ExpPreDec(id, m0)
    in
        m1
    end
fun ModPostInc(id, m0) =
    let
        val (v1, m1) = ExpPostInc(id, m0)
    in
        m1
    end
fun ModPostDec(id, m0) =
    let
        val (v1, m1) = ExpPostDec(id, m0)
    in
        m1
    end

fun ModStatementList(statementList, m0) = m0

fun ModStatemen(statement, m0) = m0
    
fun ModBlock(block, (environ:env, str:store)) =
    let
        val (env1, s1) = ModStatementList(block, (environ, str))
        val m1 = (environ,s1) 
    in
        m1
    end
    
fun ModIf(expression, block, m0) =
    let
        val (v1, m1) = ExpUnaryComparison(valOf(Bool.fromString(expression)), m0)
    in
        if v1 then ModBlock(block, m1)
        else
            m1
    end
    
fun ModIfThenElse(expression, block1, block2, m0) =
    let
        val (v1, m1) = ExpUnaryComparison(valOf(Bool.fromString(expression)), m0)
    in
        if v1 then ModBlock(block1, m1)
        else
            ModBlock(block2, m1)
    end
    
fun ModWhile(expression, block, m0) =
    let
        val (v1, m1) = ExpUnaryComparison(expression, m0)
    in
        if v1 then
            let
                val m2 = ModBlock(block, m1)
            in
		ModWhile(expression, block, m2) 
            end
        else
            m1 
    end

fun ModForHelpe(id,expression,typ:string, m0) =
    let 
        val m1 =
        if typ = "preInc" orelse typ = "preDec" then
            if typ = "preInc" then ModPreInc(id, m0)
            else
                if typ = "preDec" then ModPreDec(id, m0)
            else m0
        else m0
        
        val (v1, m2) = ExpUnaryComparison(expression, m1) 
        val m3 = 
            if typ = "postInc" orelse typ = "postDec" then
                if typ = "postInc" then ModPostInc(id, m2)
                else
                    if typ = "postInc" then ModPostDec(id, m2)
                    else m2
            else m2
    in
            ModForHelpe(id,expression,typ:string, m3)
    end

fun ModFor(id, expression1, expression2, block, typ:string, m0) =
    let 
        val m1 =intInitialization(id, expression1, m0)
        val(v1, m2) = ExpUnaryComparison(expression2, m1) 
    in
        if v1 then ModForHelpe(id,expression2, typ , m0)
        else
            m2 
    end
	
(*  Start ignore  - model testing functions finished *)
	
(* Starts from Here *)
	
fun E(  itree(inode("expression",_), 
                [ 
                    logicalOr
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(logicalOr, m)
        in
            (v1, m1)
        end
| E(  itree(inode("logicalOr",_), 
                [ 
                    logicalOr,
                    itree(inode("||",_), [] ),
                    logicalAnd
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(logicalOr, m)
        in
            if v1 then
                (v1, m1)
            else
               E(logicalAnd, m) 
        end
| E(  itree(inode("logicalOr",_), 
                [ 
                    logicalAnd
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(logicalAnd, m)
        in
            (v1, m1)
 
        end
| E(  itree(inode("logicalAnd",_), 
                [ 
                    logicalAnd,
                    itree(inode("&&",_), [] ),
                    equality
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(logicalAnd, m)
        in
            if v1 then
                E(equality, m)
            else
                (v1, m1)
        end
| E(  itree(inode("logicalAnd",_), 
                [ 
                    equality
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(equality, m)
        in
            (v1, m1)
        end  
| E(  itree(inode("equality",_), 
                [ 
                    equality,
                    itree(inode("==",_), [] ),
                    relational
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(equality, m)
            val (v2, m2) = E(relational, m)
        in
            (v1 = v2, m2)
        end
| E(  itree(inode("equality",_), 
                [ 
                    equality,
                    itree(inode("!=",_), [] ),
                    relational
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(equality, m)
            val (v2, m2) = E(relational, m)
        in
            (v1 <> v2, m2)
        end
| E(  itree(inode("equality",_), 
                [ 
                    relational
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(relational, m)
        in
            (v1, m1)
        end
| E(  itree(inode("relational",_), 
                [ 
                    relational,
                    itree(inode(">",_), [] ),
                    additive
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(relational, m)
            val (v2, m2) = E(additive, m1)
        in
            (v1 > v2, m2)
        end
| E(  itree(inode("relational",_), 
                [ 
                    relational,
                    itree(inode("<",_), [] ),
                    additive
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(relational, m)
            val (v2, m2) = E(additive, m1)
        in
            (v1 < v2, m2)
        end
| E(  itree(inode("relational",_), 
                [ 
                    relational,
                    itree(inode(">=",_), [] ),
                    additive
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(relational, m)
            val (v2, m2) = E(additive, m1)
        in
            (v1 >= v2, m2)
        end
| E(  itree(inode("relational",_), 
                [ 
                    relational,
                    itree(inode("<=",_), [] ),
                    additive
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(relational, m)
            val (v2, m2) = E(additive, m1)
        in
            (v1 <= v2, m2)
        end
| E(  itree(inode("relational",_), 
                [ 
                    additive
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(additive,m)
        in
            (v1, m1)
        end
| E(  itree(inode("additive",_), 
                [ 
                    additive,
                    itree(inode("+",_), [] ),
                    multiplicative
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(additive, m)
            val (v2, m2) = E(multiplicative, m1)
        in
            (v1 + v2, m2)
        end 
| E(  itree(inode("additive",_), 
                [ 
                    additive,
                    itree(inode("-",_), [] ),
                    multiplicative
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(additive, m)
            val (v2, m2) = E(multiplicative, m1)
        in
            (v1 - v2, m2)
        end 
| E(  itree(inode("additive",_), 
                [ 
                    multiplicative
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(multiplicative,m)
        in
            (v1, m1)
        end
| E(  itree(inode("multiplicative",_), 
                [ 
                    multiplicative,
                    itree(inode("*",_), [] ),
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(multiplicative, m)
            val (v2, m2) = E(factor, m)
        in
            (v1 * v2, m2)
        end
    
| E(  itree(inode("multiplicative",_), 
                [ 
                    multiplicative,
                    itree(inode("/",_), [] ),
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(multiplicative, m)
            val (v2, m2) = E(factor, m)
        in
            (v1 div v2, m2)
        end
| E(  itree(inode("multiplicative",_), 
                [ 
                    multiplicative,
                    itree(inode("%",_), [] ),
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(multiplicative, m)
            val (v2, m2) = E(factor, m)
        in
            (v1 mod v2, m2)
        end

| E(  itree(inode("multiplicative",_), 
                [ 
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(factor,m)
        in
            (v1, m1)
        end
| E(  itree(inode("factor",_), 
                [ 
                    itree(inode("~",_), [] ),
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(factor, m)
        in
            (~v1, m1)
            
        end
| E(  itree(inode("factor",_), 
                [ 
                    itree(inode("!",_), [] ),
                    factor
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(factor, m)
        in
            (not v1, m1)
        end
| E(  itree(inode("factor",_), 
                [ 
                    exponent
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(exponent, m)
        in
            (v1, m1)
        end
| E(  itree(inode("exponent",_), 
                [ 
                    base,
                    itree(inode("^",_), [] ),
                    exponent
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(base, m)
            val (v2, m2) = E(exponent, m)
            fun comExp(b, e) =
            if e = 0 then 1
            else
                b * comExp(b, e-1)
                val v3 = TypeChecker.typeOf([[v1 ^ v2]], m)
        in
            (v3, m2)
        end
| E(  itree(inode("exponent",_), 
                [ 
                    base
                ] 
             ), 
        m
    ) = 
        let
            val (v1, m1) = E(base, m)
        in
            (v1, m1)
        end
| E(  itree(inode("base",_), 
                [ 
                    itree(inode("(",_), [] ),
                    expression,
                    itree(inode("(",_), [] )
                ] 
             ), 
        m
    ) = E (expression, m)
    
| E(  itree(inode("base",_), 
                [ 
                    itree(inode("|",_), [] ),
                    expression,
                    itree(inode("|",_), [] )
                ] 
             ), 
        m
    ) = 
        let
            val(v1, m1) = E (expression, m) 
            val v2 = 
            if v1 > 0 then v1 
            else ~v1 
        in
            (v2, m1)
        end
| E(  itree(inode("base",_), 
                [ 
                    modifiedId
                ] 
             ), 
        m
    ) = E(modifiedId, m)
| E(  itree(inode("base",_), 
                [ 
                    id
                ] 
             ), 
        m
    ) = 
        let
            val loc = getLoc(accessEnv(getLeaf(id), m)) 
            val v1 = accessStore(loc, m) 
        in
            (v1, m)
        end   
| E(  itree(inode("base",_), 
                [ 
                    integer
                ] 
             ), 
        m
    ) = (integer, m)
    
| E(  itree(inode("base",_), 
                [ 
                    boolean
                ] 
             ), 
        m
    ) = (getLeaf(boolean), m)
| E(  itree(inode("modifiedId",_), 
                [ 
                    prefix
                ] 
             ), 
        m
    ) = E(prefix,m)
| E(  itree(inode("modifiedId",_), 
                [ 
                    postfix
                ] 
             ), 
        m
    ) = E(postfix,m)
| E(  itree(inode("prefix",_), 
                [ 
                   itree(inode("++",_), [] ),
                   id
                ] 
             ), 
        m
    ) =
    let
        val loc = getLoc(accessEnv(getLeaf(id), m))
        val v1 = accessStore(loc, m)
        val v2 = valOf(Int.fromString(v1)) + 1
        val m1 = updateStore(loc, Int.toString(v2), m)
    in
        (v2, m1)
    end
| E(  itree(inode("prefix",_), 
                [ 
                   itree(inode("--",_), [] ),
                   id
                ] 
             ), 
        m
    ) =
    let
        val(v1, m1) = ExpGetValOfId(getLeaf(id), m)
        val v2 = valOf(Int.fromString(v1)) - 1
        val loc = getLoc(accessEnv(getLeaf(id), m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
    in
        (v2, m2)
    end
| E(  itree(inode("postfix",_), 
                [ 
                   id,
                   itree(inode("++",_), [] )
                ] 
             ), 
        m
    ) =
    let
        val(v1, m1) = ExpGetValOfId(getLeaf(id), m)
        val v2 = valOf(Int.fromString(v1)) + 1
        val loc = getLoc(accessEnv(getLeaf(id), m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
    in
        (v1, m2)
    end
| E(  itree(inode("postfix",_), 
                [ 
                   id,
                   itree(inode("--",_), [] )
                ] 
             ), 
        m
    ) =
    let
        val(v1, m1) = ExpGetValOfId(getLeaf(id), m)
        val v2 = valOf(Int.fromString(v1)) - 1
        val loc = getLoc(accessEnv(getLeaf(id), m1))
        val m2 = updateStore(loc, Int.toString(v2), m1)
    in
        (v1, m2)
    end   
  | E(  itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  
  | E _ = raise Fail("error in Semantics.M - this should never occur")
  
fun M(  itree(inode("prog",_), 
                [ 
                    statementList
                ] 
             ), 
        m
    ) = m
|   M(  itree(inode("statementList",_), 
                [ 
                    statement,
                    statementList
                ] 
             ), 
        m
    ) = m
|   M(  itree(inode("statementList",_), 
                [ 
                ] 
             ), 
        m
    ) = m
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("int",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = declaration("int", getLeaf(id), m )
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("bool",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = 
    let 
        val m1 = updateEnv(id, bool, new(), m0) 
    in 
     	m1 
    end 

|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("int",_), [] ),
                    id,
                    itree(inode("=",_), [] ),
                    expression,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = intInitialization(getLeaf(id), getLeaf(expression), m) 
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("bool",_), [] ),
                    id,
                    itree(inode("=",_), [] ),
                    expression,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = boolInitialization(getLeaf(id), expression, m)
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("if",_), [] ),
                    itree(inode("(",_), [] ),
                    expression,
                    itree(inode(")",_), [] ),
                    itree(inode("{",_), [] ),
                    statementList,
                    itree(inode("}",_), [] )
                ] 
             ), 
        m
    ) = ModIf( (getLeaf(expression)), M(statementList, m), m)
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("if",_), [] ),
                    itree(inode("(",_), [] ),
                    expression,
                    itree(inode(")",_), [] ),
                    itree(inode("{",_), [] ),
                    statementList1,
                    itree(inode("}",_), [] ),
                    itree(inode("else",_), [] ),
                    itree(inode("{",_), [] ),
                    statementList2,
                    itree(inode("}",_), [] )
                ] 
             ), 
        m
    ) = ModIfThenElse( (getLeaf(expression)), M(statementList1, m), M(statementList2, m), m)
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("while",_), [] ),
                    itree(inode("(",_), [] ),
                    expression,
                    itree(inode(")",_), [] ),
                    itree(inode("{",_), [] ),
                    statementList,
                    itree(inode("}",_), [] )
                ] 
             ), 
        m
    ) = ModWhile( (getLeaf(expression)), M(statementList, m), m)
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("++",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = ModPreInc(getLeaf(id), m)
|   M(  itree(inode("statement",_), 
                [ 
                    itree(inode("--",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = ModPreDec(getLeaf(id), m)
|   M(  itree(inode("statement",_), 
                [ 
                    id,
                    itree(inode("++",_), [] ),
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = ModPostInc(getLeaf(id), m)

|   M(  itree(inode("statement",_), 
                [ 
                    id,
                    itree(inode("--",_), [] ),
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) = ModPostDec(getLeaf(id), m)
 
  | M(  itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  
  | M _ = raise Fail("error in Semantics. M - this should never occur")

(* =========================================================================================================== *)
end (* struct *)
(* =========================================================================================================== *)