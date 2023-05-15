(* =========================================================================================================== *) 
structure TypeChecker = 
struct
    
 open Model;
 
 open CONCRETE_REPRESENTATION; 
(* =========================================================================================================== *)
 (* Here is where your typeCheck and typeOf definitions go. 
 The primary challenge here is to translate the parse expression 
 notation we used in M2 to the actual SML tree patterns used in the TL System.
 See the comments in the semantics.sml file for a 
 more detailed discussion on this topic. *) 
 
 fun int typeOf(integer_value, m) = INT 
 fun bool typeOf(boolean_value, m) = BOOL 
 fun (id, m) = getType(accessEnv(id, m)) 
 fun typeCheck(itree(inode("prog", _), [stmt_list]), m) = m 
 | typeCheck(itree(inode(x_root, _), children), _) = 
 raise General.Fail ("\n\nIn typeCheck root = " ^ x_root ^ "\n\n") 
 | typeCheck _ = raise Fail ("Error in Model.typeCheck - this should never occur") 
 
 (* START TYPECHECK *)
    | typeCheck([INT id], m) =
        let
            val m1 = updateEnv(id, int, m)
        in
            m1
        end
    
    | typeCheck([BOOL id], m) = 
        let
            val m1 = updateEnv(id, bool, m)
        in
            m1
        end
    | typeCheck([id "=" expression], m) =
        let
            val t1 = typeOf(expression, m)
            val t2 = getType(accessEnv(id, m))
        in
            if t1 = t2 then m
            else ERROR
        end
    | typeCheck([INT id "=" expression], m) =
        let
            val m1 = typeCheck([INT id], m)
            val t1 = typeOf(expression, m)
        in
            if t1 = INT then typeCheck([id "=" expression], m1)
            else ERROR
        end
    | typeCheck([BOOL id "=" expression], m) =
        let
            val m1 = typeCheck([BOOL id], m)
            val t1 = typeOf(expression, m)
        in
            if t1 = BOOL then typeCheck([id "=" expression], m1)
            else ERROR
        end
    (* STATEMENTLIST??????
    | typeCheck([BOOL id "=" expression], m) =
        let
            val m1 = typeOf(id, m)
            val m2 = typeCheck([id "=" expression], m1)
        in
            m2
        end
    *)
    
    | typeCheck(["{"statementList"}"], m) =
        let
            val m1 = typeCheck(statementList, m)
        in
            m1
        end
    | typeCheck(["for" "(" initialization ";" expression ";" modifiedId ")" block], m) =
        let
            val m1 = typeCheck([initialization], m)
            val t1 = typeOf(expression, m)
            val t2 = typeOf(modifiedId, m2)
        in
            if t1 = BOOL andalso t2 = INT then
                typeCheck([block], m)
            else ERROR
        end
    
(* END TYPECHECK *)

 fun typeOf([[additive + multiplicative]], m) = 
 let 
	val t1 = typeOf(additive, m) 
	val t2 = typeOf(multiplicative, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
		else ERROR 
 end 
 
 | typeOf([[additive - multiplicative]], m) = 
 let val t1 = typeOf(additive, m) val t2 = typeOf(multiplicative, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
	else ERROR 
 end 
 
 | typeOf([[multiplicative]], m) = typeOf(multiplicative, m) 
 
 | typeOf([[multiplicative * factor]], m) = 
 
 let 
	val t1 = typeOf(multiplicative, m) 
	val t2 = typeOf(factor, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
	else ERROR 
 end 
 
 | typeOf([[multiplicative / factor]], m) = 
 let 
	val t1 = typeOf(multiplicative, m) 
	val t2 = typeOf(factor, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
	else ERROR 
 end
 
 | typeOf([[multiplicative % factor]], m) = 
 let 
	val t1 = typeOf(multiplicative, m) 
	val t2 = typeOf(factor, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
	else ERROR 
	end 
 
 | typeOf([[base ^ exponent]], m) = 
 let 
	val t1 = typeOf(base, m) 
	val t2 = typeOf(exponent, m) 
 in 
	if t1 = INT andalso t2 = INT then INT 
	else ERROR 
 end 
 
 | typeOf([[(expression)]], m) = typeOf(expression, m) 
 
 | typeOf([[modifiedId]], m) = typeOf(modifiedId, m) 
 
 | typeOf([[factor]], m) = typeOf(factor, m) 
 
 | typeOf([[equality == relational]], m) = 
 
 let 
	val t1 = typeOf(equality, m) 
	val t2 = typeOf(relational, m) 
 in 
	if t1 = BOOL andalso t2 = BOOL then BOOL 
	else if t1 = INT andalso t2 = INT then BOOL 
		else ERROR 
 end 
 
 | typeOf([[equality != relational]], m) = 
 let 
	val t1 = typeOf(equality, m) 
	val t2 = typeOf(relational, m) 
 in 
	if t1 = BOOL andalso t2 = BOOL then BOOL 
	else if t1 = INT andalso t2 = INT then BOOL
		else ERROR 
 end 
 
 | typeOf([[relational]], m) = typeOf(relational, m) 
 
 | typeOf([[relational > additive]], m) = 
 let 
	val t1 = typeOf(relational, m) 
	val t2 = typeOf(additive, m) 
 in 
	if t1 = INT andalso t2 = INT then BOOL 
	else ERROR 
 end 
 
 | typeOf([[relational < additive]], m) = 
 let 
	val t1 = typeOf(relational, m) 
		val t2 = typeOf(additive, m) 
 in 
	if t1 = INT andalso t2 = INT then BOOL 
	else ERROR 
 end 
 
 | typeOf([[relational >= additive]], m) = 
	let 
		val t1 = typeOf(relational, m) 
		val t2 = typeOf(additive, m) 
	in 
		if t1 = INT andalso t2 = INT then BOOL 
		else ERROR 
	end 
	
 | typeOf([[relational <= additive]], m) = 
	let 
		val t1 = typeOf(relational, m) 
		val t2 = typeOf(additive, m) 
	in 
		if t1 = INT andalso t2 = INT then BOOL 
		else ERROR 
	end 
 | typeOf([[additive]], m) = typeOf(additive, m) 
 
 
 

	(*****  M2 ****)
			
 | typeOf([[logicalOr || logicalAnd]], m) = 
	let 
		val t1 = typeOf(logicalOr, m) 
		val t2 = typeOf(logicalAnd, m) 
	in 
		if t1 = t2 andalso t1 = BOOL then BOOL 
		else ERROR 
	end 
 | typeOf([[logicalAnd]], m) = typeOf(logicalAnd, m)
 
 
	(*****  M4 ****)
	
   fun typeOf(itree(inode("expression", _), 
				[
					logicalOr
				]
				)
			, m) = typeOf(logicalOr, m)
			

    fun typeOf(itree(inode("logicalOr", _), 
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
						else ERROR 
					end
 | typeOf(itree(inode("logicalOr", _), 
				[
					logicalAnd
				]
				)
			, m) = typeOf(logicalAnd, m)
 
 (********** M2 **************)
 
 | typeOf([[logicalAnd && equality]], m) = 
	let 
		val t1 = typeOf(logicalAnd, m) 
		val t2 = typeOf(equality, m) 
	in 
		if t1 = t2 andalso t1 = BOOL then BOOL 
		else ERROR 
	end 
 | typeOf([[equality]], m) = typeOf(equality, m) 
 
 
  (********** M4 **************)
  
 | typeOf(itree(inode("logicalAnd", _), 
				[
					logicalAnd,
					itree(inode("&&",_), [] ),
					equality
				]
			, m )) =
					let 
						val t1 = typeOf(logicalAnd, m) 
						val t2 = typeOf(equality, m) 
					in 
						if t1 = t2 andalso t1 = BOOL then BOOL 
						else ERROR 
					end 
 | typeOf(itree(inode("logicalAnd", _), 
					[
						equality
					]
			, m )) = typeOf(equality, m)
			
			
 (********** M2 **************)
  (*  - does not match our grammar we must either chane it in grammar to - or here to ~    *)
  
 | typeOf([[!factor]], m) = 
	let 
		val t1 = typeOf(factor, m) 
	in 
	if t1 = BOOL then BOOL 
	else ERROR end 

  
 | typeOf([[-factor]], m) = 
	let 
		val t1 = typeOf(factor, m)
	in 
		if t1 = INT then INT 
		else ERROR 
	end 

  (********** M4 **************)
(*  - does not match our grammar we must either chane it in grammar to - or here to ~    *)

| typeOf(itree(inode("factor", _), 
					[
						itree(inode("-",_), [] ),
						factor
					]
			, m )) = 
					let 
						val t1 = typeOf(factor, m) 
					in 
						if t1 = INT then INT 
						else ERROR end 
  
| typeOf(itree(inode("factor", _), 
					[
						itree(inode("!",_), [] ),
						factor
					]
			, m )) = 
					let 
						val t1 = typeOf(factor, m) 
					in 
						if t1 = BOOL then BOOL 
						else ERROR end 


 
	
 | typeOf([[exponent]], m) = typeOf(exponent, m) 
 
 | typeOf([itree(inode("|", _), [itree(inode("expression", _), [expression])])], m) =
	let 
		val t1 = typeOf(expression, m) 
	in 
		if t1 = INT then INT 
		else ERROR 
	end
	
 | typeOf([[++id]], m) = 
	let 
		val t1 = typeOf(id, m) 
	in 
		if t1 = INT then INT 
		else ERROR 
	end 
	
 | typeOf([[id++]], m) = 
	let 
		val t1 = typeOf(id, m) 
	in 
		if t1 = INT then INT 
		else ERROR 
	end 
	
 | typeOf([[--id]], m) = 
	let 
		val t1 = typeOf(id, m) 
	in 
		if t1 = INT then INT 
		else ERROR 
	end 
	
 | typeOf([[id--]], m) = 
	let 
		val t1 = typeOf(id, m) 
	in 
		if t1 = INT then INT 
		else ERROR 
	end 
 | typeOf(_, _) = raise Fail "Unknown pattern encountered" 
 (* =========================================================================================================== *) 
 end (* struct *) 
 (* =========================================================================================================== *)
 
 