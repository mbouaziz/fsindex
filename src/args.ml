
open ArgSpec
open More

type 'a t = 'a argSpec

type wrapped = Wrapped : 'a t -> wrapped

let completeCommand cmd arg =
  cmd |> List.map fst3 |> List.filter (stringStartsWith arg) |> List.iter print_endline
let completeFile arg = Sys.command (Printf.sprintf "bash -c \"compgen -f -- '%s'\"" arg) |> ignore
let completeDir arg = Sys.command (Printf.sprintf "bash -c \"compgen -d -- '%s'\"" arg) |> ignore

let rec canBeEmpty : type a. a t -> bool = function
  | Apply (_, t) -> canBeEmpty t
  | Commands _ -> false
  | Dir -> false
  | File -> false
  | List _ -> true
  | Nothing -> true
  | Or (t1, t2) -> canBeEmpty t1 || canBeEmpty t2
  | Then (t1, t2) -> canBeEmpty t1 && canBeEmpty t2
  | Value _ -> true

let rec completeT : type a. a t -> string -> unit = fun t arg ->
  match t with
  | Apply (_, t') -> completeT t' arg
  | Commands cmd -> completeCommand cmd arg
  | Dir -> completeDir arg
  | File -> completeFile arg
  | List t' -> completeT t' arg
  | Nothing -> ()
  | Or (t1, t2) -> completeT t1 arg; completeT t2 arg
  | Then (t1, t2) ->
    completeT t1 arg;
    if canBeEmpty t1 then completeT t2 arg
  | Value _ -> ()

let rec matchFirst : type a. a t -> string -> wrapped = fun t0 a ->
  match t0 with
  | Apply (_, t') -> matchFirst t' a
  | Commands cmd ->
    Wrapped (snd3 (List.find (fst3 @> ((=) a)) cmd))
  | Dir -> Wrapped Nothing
  | File -> Wrapped Nothing
  | List t ->
    (match matchFirst t a with
    | Wrapped Nothing -> Wrapped (List t)
    | Wrapped t' -> Wrapped (Then (t', t0)))
  | Nothing -> raise Not_found
  | Or (t1, t2) ->
    (match matchFirst t1 a with
    | exception Not_found -> matchFirst t2 a
    | Wrapped Nothing -> matchFirst t2 a
    | Wrapped t1 -> Wrapped t1)
  | Then (t1, t2) ->
    (match matchFirst t1 a with
    | Wrapped Nothing -> Wrapped t2
    | Wrapped t1 -> Wrapped (Then (t1, t2)))
  | Value _ -> raise Not_found

let rec doComplete : type a. a t -> string list -> unit = fun t args ->
  match args with
  | [] -> completeT t ""
  | [a] -> completeT t a
  | a0::args ->
    match matchFirst t a0 with
    | exception Not_found -> ()
    | Wrapped t' -> doComplete t' args

let compute : type a. a t -> string list -> a = fun t args ->
  let rec aux : type a. a t -> bool -> string list -> a Lazy.t * string list = fun t full args ->
    match t, args with
    | Apply (f, t'), _ ->
      let res, rem = aux t' full args in
      lazy (f (Lazy.force res)), rem
    | Commands _, [] -> raise (Arg.Bad "Missing command")
    | Commands cmd, command::args ->
      (match List.find (fst3 @> ((=) command)) cmd with
      | _, t, _ -> aux t full args
      | exception Not_found -> raise (Arg.Bad "Unknown command"))
    | Dir, [] -> raise (Arg.Bad "Missing directory name")
    | File, [] -> raise (Arg.Bad "Missing file name")
    | Dir, arg::args ->
      if full && args <> [] then raise (Arg.Bad "Too many arguments");
      lazy arg, args
    | File, arg::args ->
      if full && args <> [] then raise (Arg.Bad "Too many arguments");
      lazy arg, args
    | List _, [] -> lazy [], []
    | List t', args ->
      let res, rem = match aux t' false args with
      | exception (Arg.Bad _) -> lazy [], args
      | res, rem ->
        let res', rem = aux t full rem in
        lazy ((Lazy.force res)::(Lazy.force res')), rem in
      if full && rem <> [] then raise (Arg.Bad "Too many arguments");
      res, rem
    | Nothing, args ->
      if full && args <> [] then raise (Arg.Bad "Too many arguments");
      lazy (), args
    | Or (t1, t2), args ->
      (try aux t1 full args with
      | Arg.Bad _ -> aux t2 full args)
    | Then (t1, t2), args ->
      let res1, rem1 = aux t1 false args in
      let res2, rem = aux t2 full rem1 in
      lazy (Lazy.force res1, Lazy.force res2), rem
    | Value v, args ->
      if full && args <> [] then raise (Arg.Bad "Too many arguments");
      lazy v, args
  in
  match aux t true args with
  | v, [] -> Lazy.force v
  | _, rem -> raise (Arg.Bad "Too many arguments") (* should not happen *)

let usage commands =
  let padFor =
    let maxLength = List.fold_left (fun m (k, _, _) -> max m (String.length k)) 0 commands in
    fun s -> String.make (maxLength + 3 - String.length s) ' '
  in
  let commandUsage (k, _, doc) = Printf.printf " %s%s%s\n" k (padFor k) doc in
  List.iter commandUsage commands
