open Batteries_uni
open Std

module List = struct

  include List
  
  let rev_mape f lst =
    let _, res = List.fold_left
      (fun (env,cont) x -> let y, env = f env x in (env, y::cont)) 
      ([], []) 
      lst 
    in
    res

  let mape f lst =
    List.rev (rev_mape f lst)    

end

module B = struct

  type t = 
    | Int of Int64.t
    | Char of char
    | List of t list (* 型を制限できてない... *)
        
  let print_sep = function
    | Int _ -> Printf.printf " "
    | Char _ -> ()
    | List _ -> Printf.printf "\n"

  let rec print = function
    | Int x -> Printf.printf "%Ld" x
    | Char c -> Printf.printf "%c" c
    | List lst -> List.iter (fun x -> print x; print_sep x) lst

end

module A = struct

  type ident = string
  type _t = Type.t * ident list
  type t = ident * _t

  let rec eval_random' env (ident, (typ, deps)) =
    match typ, deps with
      | Type.Int (inf, sup), [] -> 
        let open Int64_op in
            let value = inf + Random.int64 (sup - inf + 1L) in
            B.Int value, (ident, Int64.to_int value)::env
      | Type.Char charlst, [] ->
        B.Char (charlst.[Random.int (String.length charlst - 1)]), env
      | typ, d::ds ->
        let len = List.assoc d env in
        B.List 
          (List.init 
             len 
             (fun i -> fst (eval_random' env (Printf.sprintf "%s[%d]" ident i, (typ, ds))))),
        env

  let eval_random lst =
    List.mape eval_random' lst

end

module Z = struct (* 構文木をtypeを解決しつつAに渡す形式にする *)

  let resolve_type' type_env = function
    | Syntax.Typedef (ident, typ) ->
      None, (ident, typ) :: type_env
    | Syntax.Term (ident, (Type.Id x, deps)) ->
      Some (ident, (List.assoc x type_env, deps)), type_env
    | Syntax.Term (ident, (Type.T x, deps)) ->
      Some (ident, (x, deps)), type_env

  let resolve_type lst =
    List.filter_map
      identity
      (List.mape resolve_type' lst)

end

let () =
  Random.self_init ()

let small_latten_letters = "abcdefghijklmnopqrstuvwxyz"
let big_latten_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digits = "0123456789"

let get_input0 () =
  let lexbuf = Lexing.from_channel stdin in
  let res = ref [] in
  begin
    try
      while true do
        res := (Parser.topp Lexer.token lexbuf) :: !res
      done
    with End_of_file -> ()
  end;
  List.rev !res

let get_input1 () = 
  let open Syntax in
  let open Type in
      [ Typedef ("a", int 1L 15L);
        Typedef ("b", int 0L 100L);
        Typedef ("c", char small_latten_letters);
        Term ("n", (Id "a", [])); 
        Term ("a", (Id "c", ["n"])) ]

let () =
  let hoge =
    Z.resolve_type (get_input0 ())
  in
  List.iter (fun x -> B.print x; Printf.printf "\n") (A.eval_random hoge)

(** sample input

type a = [1,10)
type b = [0,100)
n:a
a:b[n]

*)



















