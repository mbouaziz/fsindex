
open GenSpec
open More

type t =
  | Commands : (string * ('z, 'o, 'l) spec * 'b) list -> t
  | Dir
  | File
  | FunAll of (string list -> unit)
  | Nothing
  | Or of t * t
  | Then of t * t

and ('z, 'o, 'l) spec = ('z, 'o, 'l, t) genSpec

let completeCommand cmd arg =
  cmd |> List.map fst3 |> List.filter (stringStartsWith arg) |> List.iter print_endline
let completeFile arg = Sys.command (Printf.sprintf "bash -c \"compgen -f -- '%s'\"" arg) |> ignore
let completeDir arg = Sys.command (Printf.sprintf "bash -c \"compgen -d -- '%s'\"" arg) |> ignore

let rec completeT t args arg =
  match t with
  | Commands cmd -> completeCommand cmd arg
  | Dir -> completeDir arg
  | File -> completeFile arg
  | FunAll f -> f args
  | Nothing -> ()
  | Or (t1, t2) -> completeT t1 args arg; completeT t2 args arg
  | Then (t1, t2) -> completeT t1 args arg

let rec matchFirst t a =
  match t with
  | Commands cmd ->
    (match List.find (fst3 @> ((=) a)) cmd with
    | (_, Zero _, _) -> Some Nothing
    | (_, One (_, t'), _) -> Some t'
    | (_, List (_, t'), _) -> let rec t'' = Then (t', t'') in Some t''
    | exception Not_found -> None)
  | Dir | File | FunAll _ -> Some Nothing
  | Nothing -> None
  | Or (t1, t2) ->
    (match matchFirst t1 a, matchFirst t2 a with
    | None, None -> None
    | None, Some t | Some t, None -> Some t
    | Some t1, Some t2 -> Some (Or (t1, t2)) )
  | Then (t1, t2) ->
    match matchFirst t1 a with
    | None -> None
    | Some Nothing -> Some t2
    | Some t1 -> Some (Then (t1, t2))

let doComplete t allArgs =
  let rec aux t args =
    let first a = completeT t allArgs a in
    match args with
    | [] -> first ""
    | [a] -> first a
    | a0::args ->
      match matchFirst t a0 with
      | None -> ()
      | Some t' -> aux t' args in
  aux t allArgs
