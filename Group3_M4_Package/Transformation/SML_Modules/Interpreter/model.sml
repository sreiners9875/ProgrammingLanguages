(* =========================================================================================================== *)
structure Model =

struct 

(* =========================================================================================================== *)
(* This function may be useful to get the leaf node of a tree -- which is always a string (even for integers).
   It is up to the interpreter to translate values accordingly (e.g., string to integer and string to boolean).
   
   Consult (i.e., open Int and open Bool) the SML structures Int and Bool for functions that can help with 
   this translation. 
*)

 fun getLeaf( term ) = CONCRETE.leavesToStringRaw term 

(* For your typeChecker you may want to have a datatype that defines the types 
  (i.e., integer, boolean and error) in your language. *)

datatype types = INT | BOOL | ERROR;

(* It is recommended that your model store integers and Booleans in an internal form (i.e., as terms belonging to
   a userdefined datatype (e.g., denotable_value). If this is done, the store can be modeled as a list of such values.
*)
datatype denotable_value =  Boolean of bool
                          | Integer of int;
                          
type loc   = int
type env   = (string * types * loc) list
type store = (loc * denotable_value) list
type counter = int


(* The model defined here is a triple consisting of an environment, an address counter, and a store. The environment
   and the store are lists similar to what we have used in class. The address counter serves as an implementation of
   new(). Note that, depending on your implementation, this counter either contains the address of (1) the
   next available memory location, or (2) the last used memory location -- it all depends on when the counter is 
   incremented. *)

val initialModel = ( []:env, []:store, 0:counter )
val error = print("Error! Location not found")

fun accessEnv(id1: string, (environ: env, s: store, c: counter )) =
    let
        val msg = "Error: accessEnv " ^ id1 ^ " not found.";
        
        fun aux [] = error msg
          | aux ((id, t, loc)::env) =
                if id1 = id then (t, loc)
                else aux env;
    in
        aux env
    end;

fun getLoc(t: types, l:loc) = l

fun getType(t: types, l:loc) = t

fun getEnv(e: env, s: store, c: counter) = e

fun getStore(e: env, s: store, c: counter) = s

fun getCounter(e: env, s: store, c: counter) = c

fun updateEnv(id: string, t: types, loc0: loc, (environ: env, stored: store, c: counter)) =
    let
        val msg = "Error: updateEnv failed.";
        val idMsg = "Error: updateEnv failed[ " ^ id ^ " already declared].";
        
        fun aux (id1, t1, loc1, []) = [(id1, t1, loc1)]
          | aux (id1, t1, loc1, (eid, et, eloc)::env) =
            if id1 = eid then
                error idMsg
            else
                (eid, et, eloc)::aux(id1, t1, loc1, env)
    in
        if c == loc0
            (aux(id, t, loc0, environ), stored, c+1 )
        else
            error msg
    end
             
fun updateStore(loc0: loc, newValue: denotable_value, (environ: env, stored: store, c: counter)) =
    let
        val msg = "Error: updateStore failed.";
        
        fun aux (1oc1, nv1, []) = [(loc1, nv1)]
          | aux (loc1, nv1, (sloc, sdnv)::store) =
            if loc1 = sloc then
               (loc1, nv1)::store
            else
                (sloc, sdnv)::aux(loc1, nv1, store)
    in
        (environ, aux(loc, newValue, stored), c)
    end

fun accessStore(loc0: loc, (environ : env, stored: store, c: counter)) =
    let
        val msg = "Error: accessStore " ^ loc1 ^ " not found.";
        
        fun aux [] = error msg
            | aux ((loc1, denotable_value)::store) =
                if loc0 = loc1 then denotable_value
                else aux store;
    in
        aux store
    end;
(* =========================================================================================================== *)
(* For printing the model *)
(* =========================================================================================================== *)
fun typeToString BOOL   = "bool"
 |  typeToString INT    = "int"
 |  typeToString ERROR  = "error";
 
fun error msg = (print(msg), raise runtime_error);

fun t_error msg = (print(msg), raise model_error);

exception runtime_error
 
exception model_error
 
fun dnvToString (Integer x) = Int.toString x
 |  dnvToString (Boolean x) = Bool.toString x
 
fun dnvToBool (Boolean x) = x
 |  dnvToBool _ = raise Fail("Error dnvToBool.");

fun dnvToInt (Integer x) = x
 |  dnvToInt _ = raise Fail("Error dnvToInt.");
 
fun envEntryToString (id, t, loc) =
    "(" ^ id ^ "," ^ typeToString t ^ "," ^ Int.toString loc ^ ")";
    
fun storeEntryToString (loc, dnv) =
    "(" ^ Int.toString loc ^ "," ^ dnvToString dnv ^ ")";
    
fun showEnv [] = print "\n"
 |  showEnv (entry::env) = (
                                print("\n" ^ envEntryToString entry);
                                showEnv env
                            );
                            
fun showStr [] = print "\n"
 |  showStr (stored::store) = (
                                print("\n" ^ storeEntryToString stored);
                                showStr stored
                            );

fun printM (env, s, c) = (showEnv(env); showStr(s); print(Int.toString c);

(* =========================================================================================================== *)
end; (* struct *) 
(* =========================================================================================================== *)

val error = "Error! Location not found";