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

  (* datatype types = INT | BOOL | ERROR; *)

val INT = "int"
val BOOL = "bool"
val ERR = "error"
(* It is recommended that your model store integers and Booleans in an internal form (i.e., as terms belonging to
   a userdefined datatype (e.g., denotable_value). If this is done, the store can be modeled as a list of such values.
*)
(* datatype denotable_value =  Boolean of bool
                          | Integer of int;

*)
type loc   = int
type env   = (string * string * loc) list
type store = (loc * string) list


(* The model defined here is a triple consisting of an environment, an address counter, and a store. The environment
   and the store are lists similar to what we have used in class. The address counter serves as an implementation of
   new(). Note that, depending on your implementation, this counter either contains the address of (1) the
   next available memory location, or (2) the last used memory location -- it all depends on when the counter is 
   incremented. *)
   
val menv = []:env
val mloc = 0:loc
val mstore = []:store

val initialModel = ( []:env, []:store ) 
val error = print("Error! Location not found")


fun accessEnv(name : string, (environ: env, str: store)) : (string * loc)= 
    let 
        val result = List.find (fn (n, _, _) => n = name) environ 
    in
        case result of NONE => (ERR, ~1)
        | SOME(_,typ,loc) => (typ, loc)
    end


fun getLoc((t: string, l:loc)): loc = l

fun getType(typ: string, location:loc, (environ: env, str: store)): string = 
    let 
        val result = List.find (fn (_, t, l) => t = typ andalso location = l) environ 
    in
        case result of NONE => ERR
        | SOME(tp,loc) =>  tp
    end
    
fun updateEnv(name : string, datatyp: string, LocationOrRequest: loc,(environ: env, str: store))=
    if LocationOrRequest = ~1 then
        let
            val result = List.find (fn (n, _, _) => n = name) environ
            val locToget = length(str)+1
        in
            case result of NONE =>  (environ@[(name, datatyp, locToget)], str)
            | SOME(_,_,l) => (environ,  str)
        end
    else
        let
            val result = List.find (fn (n, _, l) => n = name andalso LocationOrRequest = l) environ
        in
            case result of NONE =>  (environ@[(name, datatyp, LocationOrRequest)], str)
            | SOME(_,_,l) => (environ, str)
        end



fun updateStore( loc1: loc, newValue: string, (environ: env, str: store)) =
    
    let val updatedSTR =
        let
            fun update ((location, value) : (loc * string)) =
                if location = loc1 then
                    (location, newValue)
                else
                   (location, value) 
        in
            List.map update str
        end
    in 
        (environ: env, updatedSTR)
    end
        

 fun accessStore(loc1: loc, (environ : env, str: store)) =
            let 
                val result = List.find (fn (l, _) => l = loc1) str
            in 
            case result of NONE => ERR
            | SOME (_, value) =>  value 
            end

fun showEnv(envron: env) = 
    let 
        fun formatVar((name, t, loc)) = 
        if t = ERR then
            "Error! at location :" ^ Int.toString(loc)
            else  name ^ " : " ^ t ^ " : " ^ Int.toString(loc)
        val formattedEnv = List.map formatVar envron 
    in 
        print("var Name : var type :  var mem location \n" ^ String.concatWith "\n" formattedEnv ^ "\n")
    end

fun showStr(str: store) = 
    let 
        fun formatVar((loc, value)) = 
        if value = ERR then
            "Error! at location :" ^ Int.toString(loc)
            else  Int.toString(loc) ^ " : " ^ value
        val formattedEnv = List.map formatVar str 
    in 
        print("mem id : value \n" ^ String.concatWith "\n" formattedEnv ^ "\n")
    end

(* =========================================================================================================== *)
end; (* struct *) 
(* =========================================================================================================== *)

val error = "Error! Location not found";
