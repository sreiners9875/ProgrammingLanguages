(* =========================================================================================================== *) 
structure TypeChecker = 
struct  open Model 
        open CONCRETE_REPRESENTATION 
(* =========================================================================================================== *)
 (* Here is where your typeCheck and typeOf definitions go. 
 The primary challenge here is to translate the parse expression 
 notation we used in M2 to the actual SML tree patterns used in the TL System.
 See the comments in the semantics.sml file for a 
 more detailed discussion on this topic. *) 

 fun int typeOf(integer_value, m) = INT 
 fun bool typeOf(boolean_value, m) = BOOL 
 fun typeOf(id, (environ:env, str:store)) = 
    let 
        val (t1,l1)  = accessEnv(getLeaf(id), (environ:env, str:store))
        val t = getType(t1, l1, (environ:env, str:store))
    in
       t
    end
 
fun typeCheck(itree(inode("prog", _), [statementList]), m) = m 
 | typeCheck(itree(inode(x_root, _), children), _) = 
 raise General.Fail ("\n\nIn typeCheck root = " ^ x_root ^ "\n\n") 
 | typeCheck _ = raise Fail ("Error in Model.typeCheck - this should never occur")
 | typeCheck(itree(inode("statementList", _), [
             statement,
             statementList
         ]), m) =
            let
                val m1 = typeOf(statement, m0)
                val m2 = typeCheck(statementList, m1)
            in
                m2
            end
            
| typeCheck(itree(inode("statement", _), [
             itree(inode("int",_), [] ),
             id
         ]), m) = updateEnv(id, INT, ~1, m)
         
| typeCheck(itree(inode("statement", _), [
             itree(inode("bool",_), [] ),
             id
         ]), m) = updateEnv(id, BOOL, ~1, m)
         
| typeCheck(itree(inode("statement", _), [
            id, 
            itree(inode("=",_), [] ),
            expression
             
         ]), m) = updateEnv(id, INT, ~1, m)
            let
                val t1 = typeOf(expression, m)
                val t2 = getType(accessEnv(id, m))
            in
                if t1 = t2 then m
                else raise model_error
            end 
| typeCheck(itree(inode("statement", _), [
            itree(inode("int",_), [] ),
            id, 
            itree(inode("=",_), [] ),
            expression
             
         ]), m) = 
            let
                val m1 = updateEnv(id, INT, ~1, m)
                val t1 = typeOf(expression, m1)
                val t2 = getType(accessEnv(id, m1))
            in
                if t1 = t2 then m
                else raise model_error
            end            

| typeCheck(itree(inode("statement", _), [
            itree(inode("bool",_), [] ),
            id, 
            itree(inode("=",_), [] ),
            expression
             
         ]), m) = 
            let
                val m1 = updateEnv(id, BOOL, ~1, m)
                val t1 = typeOf(expression, m1)
                val t2 = getType(accessEnv(id, m1))
            in
                if t1 = t2 then m
                else raise model_error
            end
| typeCheck(itree(inode("statement", _), [
            itree(inode("{",_), [] ),
            statementList, 
            itree(inode("}",_), [] )
             
         ]), m) = 
            let
                val m1 = typeCheck(statementList, m)

            in
              m1
            end            
            
| typeCheck(itree(inode("statement", _), [
            itree(inode("for",_), [] ),
            itree(inode("(",_), [] ),
            initialization, 
            itree(inode(";",_), [] ),
            expression, 
            itree(inode(";",_), [] ),
            modifiedId, 
            itree(inode(";",_), [] ),
            itree(inode(")",_), [] ),
            block
         ]), m) = 
            let
                val m1 = typeCheck(statementList, m)

            in
              m1
            end             
 | typeCheck(itree(inode("statement", _), [
            itree(inode("while",_), [] ),
            itree(inode("(",_), [] ),
            expression, 
            itree(inode(")",_), [] ),
            block
         ]), m) = 
                let
                    val t1 = typeOf(expression1, m)
                in
                    if t1 = bool then typeCheck(block, m)
                    else raise model_error
                end
| typeCheck(itree(inode("statement", _), [
            itree(inode("if",_), [] ),
            itree(inode("(",_), [] ),
            expression, 
            itree(inode(")",_), [] ),
            block
         ]), m) = 
                let
                    val t1 = typeOf(expression, m)
                    val m1 = typeCheck(block, m)
                in
                    if t1 = bool then m
                    else raise model_error
                end              
| typeCheck(itree(inode("statement", _), [
            itree(inode("if",_), [] ),
            itree(inode("(",_), [] ),
            expression, 
            itree(inode(")",_), [] ),
            block1,
            itree(inode("else",_), [] ),
            block2
         ]), m) = 
                let
                    val t1 = typeOf(expression1, m)
                    val m1 = typeCheck(block1, m)
                    val m2 = typeCheck(block2, m1)
                in
                    if t1 = bool then m
                    else raise model_error
                end
                
| typeCheck(itree(inode("statement", _), [
            itree(inode("print",_), [] ),
            itree(inode("(",_), [] ),
            expression, 
            itree(inode(")",_), [] )
         ]), m) = 
                let
                    val t1 = typeOf(expression1, m0)
                in
                    if t1 = bool then m
                    else raise model_error
                end
                
| typeCheck(itree(inode("prefix",_), 
                [ 
                    itree(inode("++",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) =     
            let
                val t1 = typeOf(id, m)
            in
                if t1 = int then m
                else raise model_error
            end
| typeCheck(itree(inode("prefix",_), 
                [ 
                    itree(inode("--",_), [] ),
                    id,
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) =     
            let
                val t1 = typeOf(id, m)
            in
                if t1 = int then m
                else raise model_error
            end

| typeCheck(itree(inode("postfix",_), 
                [ 
                    id,
                    itree(inode("++",_), [] ),
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) =     
            let
                val t1 = typeOf(id, m)
            in
                if t1 = int then m
                else raise model_error
            end

| typeCheck(itree(inode("postfix",_), 
                [ 
                    id,
                    itree(inode("--",_), [] ),
                    itree(inode(";",_), [] )
                ] 
             ), 
        m
    ) =     
            let
                val t1 = typeOf(id, m)
            in
                if t1 = int then m
                else raise model_error
            end

                            
fun typeOf(itree(inode("expression", _), [logicalOr]), m) = typeOf(logicalOr, m)
 | typeOf(itree(inode(x_root, _), children), _) = 
 raise General.Fail ("\n\nIn typeCheck root = " ^ x_root ^ "\n\n") 
 | typeOf(itree(inode("logicalOr", _), 
				[
					logicalOr,
					itree(inode("||",_), [] ),
					logicalAnd
				]
				)
			, m) =
					let 
						val t1 = typeOf(logicalOr, m) 
						val t2 = typeOf(logicalAnd, m) 
					in 
						if t1 = t2 andalso t1 = BOOL then BOOL 
						else ERR 
					end
                                        

 | typeOf(itree(inode("logicalOr", _), 
				[
					logicalAnd
				]
				)
			, m) = typeOf(logicalAnd, m) 
  
 | typeOf(itree(inode("logicalAnd", _), 
				[
					logicalAnd,
					itree(inode("&&",_), [] ),
					equality
				])
			, m ) =
					let 
						val t1 = typeOf(logicalAnd, m) 
						val t2 = typeOf(equality, m) 
					in 
						if t1 = t2 andalso t1 = BOOL then BOOL 
						else ERR 

                                            end 
                                            
             
 | typeOf(itree(inode("logicalAnd", _), 
					[
						equality
					])
			, m ) = typeOf(equality, m)
                        
                        
 | typeOf(itree(inode("equality", _), 
				[
					equality,
					itree(inode("==",_), [] ),
					relational
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(equality, m) 
						val t2 = typeOf(relational, m) 
					 in 
						if t1 = BOOL andalso t2 = BOOL then BOOL 
						else if t1 = INT andalso t2 = INT then BOOL 
							else ERR
					 end
 
 | typeOf(itree(inode("equality", _), 
				[
					equality,
					itree(inode("!=",_), [] ),
					relational
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(equality, m) 
						val t2 = typeOf(relational, m) 
					 in 
						if t1 = BOOL andalso t2 = BOOL then BOOL 
						else if t1 = INT andalso t2 = INT then BOOL 
							else ERR 
					 end 
					 

| typeOf(itree(inode("equality", _), 
				[
					relational
				]
				)
			, m) = typeOf(relational, m)  
                        
| typeOf(itree(inode("relational", _), 
				[
					relational,
					itree(inode(">",_), [] ),
					additive
				]
				)
			, m) =
					let 
						val t1 = typeOf(relational, m) 
						val t2 = typeOf(additive, m) 
					in 
						if t1 = INT andalso t2 = INT then BOOL 
						else ERR 
					end 	

 | typeOf(itree(inode("relational", _), 
				[
					relational,
					itree(inode("<",_), [] ),
					additive
				]
				)
			, m) =
					let 
						val t1 = typeOf(relational, m) 
						val t2 = typeOf(additive, m) 
					in 
						if t1 = INT andalso t2 = INT then BOOL 
						else ERR 
					end 	


| typeOf(itree(inode("relational", _), 
				[
					relational,
					itree(inode(">=",_), [] ),
					additive
				]
				)
			, m) =
					let 
						val t1 = typeOf(relational, m) 
						val t2 = typeOf(additive, m) 
					in 
						if t1 = INT andalso t2 = INT then BOOL 
						else ERR 
					end 	

	
| typeOf(itree(inode("relational", _), 
				[
					relational,
					itree(inode("<=",_), [] ),
					additive
				]
				)
			, m) =
					let 
						val t1 = typeOf(relational, m) 
						val t2 = typeOf(additive, m) 
					in 
						if t1 = INT andalso t2 = INT then BOOL 
						else ERR 
					end 

| typeOf(itree(inode("relational", _), 
				[
					additive
				]
				)
			, m) = typeOf(additive, m)                       
                        
| typeOf(itree(inode("additive", _), 
				[
					additive,
					itree(inode("+",_), [] ),
					multiplicative
				]
				)
			, m) = 
					 let 
                                             val t1 = typeOf(additive, m) 
                                             val t2 = typeOf(multiplicative, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
					 end 
 
  | typeOf(itree(inode("additive", _), 
				[
					additive,
					itree(inode("-",_), [] ),
					multiplicative
				]
				)
			, m) = 
					 let 
                                             val t1 = typeOf(additive, m) 
                                             val t2 = typeOf(multiplicative, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
					 end 
 | typeOf(itree(inode("additive", _), 
				[
					multiplicative
				]
				)
			, m) =  typeOf(multiplicative, m)  
 
 | typeOf(itree(inode("multiplicative", _), 
				[
					multiplicative,
					itree(inode("*",_), [] ),
					factor
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(multiplicative, m) 
						val t2 = typeOf(factor, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
						end 
 
 | typeOf(itree(inode("multiplicative", _), 
				[
					multiplicative,
					itree(inode("/",_), [] ),
					factor
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(multiplicative, m) 
						val t2 = typeOf(factor, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
						end 
 
 | typeOf(itree(inode("multiplicative", _), 
				[
					multiplicative,
					itree(inode("%",_), [] ),
					factor
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(multiplicative, m) 
						val t2 = typeOf(factor, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
						end 
 

 
 | typeOf(itree(inode("exponent", _), 
				[
					base,
					itree(inode("^",_), [] ),
					exponent
				]
				)
			, m) = 
					 let 
						val t1 = typeOf(base, m) 
						val t2 = typeOf(exponent, m) 
					 in 
						if t1 = INT andalso t2 = INT then INT 
						else ERR 
					 end 
 
 (*
   | typeOf(itree(inode("expression", _), 
				[
					expression
				]
				)
			, m) = typeOf(expression, m)
                        
                        
  *)                      
  | typeOf(itree(inode("modifiedId", _), 
				[
					prefix
				]
				)
			, m) = typeOf(prefix, m)
  | typeOf(itree(inode("modifiedId", _), 
				[
					postfix
				]
				)
			, m) = typeOf(postfix, m)
                        
 | typeOf(itree(inode("multiplicative", _), 
				[
					factor
				]
				)
			, m) = typeOf(factor, m) 
                        
 
 
(*	
| typeOf(itree(inode("expression", _), 
				[
					logicalOr
				]
				)
			, m) = typeOf(logicalOr, m)
			
*)

			

| typeOf(itree(inode("factor", _), 
					[
						itree(inode("~",_), [] ),
						factor
					])
			, m ) = 
					let 
						val t1 = typeOf(factor, m) 
					in 
						if t1 = INT then INT 
						else ERR 
                                        end 
  
| typeOf(itree(inode("factor", _), 
					[
						itree(inode("!",_), [] ),
						factor
					])
			, m ) = 
					let 
						val t1 = typeOf(factor, m) 
					in 
						if t1 = BOOL then BOOL 
						else ERR 
                                        end 



| typeOf(itree(inode("exponent", _), 
					[
						base
					])
			, m ) =typeOf(base, m) 
 

| typeOf(itree(inode("base", _), 
					[
						itree(inode("|",_), [] ),
						expression,
						itree(inode("|",_), [] )
					])
			, m ) = 
					let 
						val t1 = typeOf(expression, m) 
					in 
						if t1 = INT then INT 
						else ERR 
					end
| typeOf(itree(inode("base", _), 
					[
						itree(inode("(",_), [] ),
						expression,
						itree(inode(")",_), [] )
					])
			, m ) = 
					let 
						val t1 = typeOf(expression, m) 
					in 
						if t1 = INT then INT
							else if t1 = BOOL then BOOL			
						else ERR 
					end

| typeOf(  itree(inode("base",_), 
                [ 
                   modifiedId
                ] 
             ), 
        m
    ) =	typeOf(modifiedId, m) 
| typeOf(  itree(inode("prefix",_), 
                [ 
                   itree(inode("++",_), [] ),
                   id
                ] 
             ), 
        m
    ) =
		let 
			val t1 = typeOf(id, m) 
		in 
			if t1 = INT then INT 
			else ERR 
		end
		
| typeOf(  itree(inode("prefix",_), 
                [ 
                   itree(inode("--",_), [] ),
                   id
                ] 
             ), 
        m
    ) =
		let 
			val t1 = typeOf(id, m) 
		in 
			if t1 = INT then INT 
			else ERR 
		end
		
| typeOf(  itree(inode("postfix",_), 
                [ 
                   id,
                   itree(inode("++",_), [] )
                ]
             ), 
        m
    ) =
		let 
			val t1 = typeOf(id, m) 
		in 
			if t1 = INT then INT 
			else ERR 
		end	
		
| typeOf(  itree(inode("postfix",_), 
                [ 
                   id,
                   itree(inode("--",_), [] )
                ] 
             ), 
        m
    ) =
		let 
			val t1 = typeOf(id, m) 
		in 
			if t1 = INT then INT 
			else ERR 
		end		
	
 | typeOf(_, _) = raise Fail "Unknown pattern encountered"  
 (* =========================================================================================================== *) 
 end (* struct *) 
 (* =========================================================================================================== *)
 
 
