
open GenSpec
open More

type t = File | Dir | Nothing | FunAll of (string list -> unit)

let completeCommand cmd arg =
  cmd |> List.map fst3 |> List.filter (stringStartsWith arg) |> List.iter print_endline
let completeFile arg = Sys.command (Printf.sprintf "bash -c \"compgen -f -- '%s'\"" arg) |> ignore
let completeDir arg = Sys.command (Printf.sprintf "bash -c \"compgen -d -- '%s'\"" arg) |> ignore

let completeT t args arg =
  match t with
  | File -> completeFile arg
  | Dir -> completeDir arg
  | Nothing -> ()
  | FunAll f -> f args

let completeSpec spec args =
  match spec with
  | Zero _ -> ()
  | One (_, t) ->
    let first a = completeT t args a in
    begin match args with
    | [] -> first ""
    | [a] -> first a
    | _ -> () end
  | List (_, t) ->
    completeT t args (try listLast args with Not_found -> "")

let genCommand cmd more default args =
  let first a = completeCommand cmd a; more a in
  match args with
  | [] -> first ""
  | [a] -> first a
  | a0::args ->
    match List.find (fst3 @> ((=) a0)) cmd with
    | (_, spec, _) -> completeSpec spec args
    | exception Not_found -> default args

let command cmd args = genCommand cmd ignore ignore args

let commandOrFile cmd following args = genCommand cmd completeFile following args
