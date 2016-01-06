
type _ argSpec =
  | Apply : ('a -> 'b) * 'a argSpec -> 'b argSpec
  | Commands : (string * 'a argSpec * _) list -> 'a argSpec
  | Dir : string argSpec
  | File : string argSpec
  | List : 'a argSpec -> 'a list argSpec
  | NonEmptyList : 'a argSpec -> 'a list argSpec
  | Nothing : unit argSpec
  | Or : 'a argSpec * 'a argSpec -> 'a argSpec
  | Then : 'a argSpec * 'b argSpec -> ('a * 'b) argSpec
  | Value : 'a -> 'a argSpec

(*

  (missing) Any c
  match any token
  complete c()
  return the token

  Apply(f, t)
  match t
  complete t
  return f [| t |]

  Commands [s1, t1, _; s2, t2, _; ...]
  (s1, s2, ... must be all different and non-empty)
  match s1 followed by t1 or s2 followed by t2 or ...
  complete s1 and s2 and ...
  return [| t1 |] if first token is s1, [| t2 |] if second token is s2, ...
  is like Or(Apply(snd, Then(Exact s1, t1)), Or(Apply(snd, Then(Exact s2, t2)), ...))
  is like Switch ([...], Fail)

  (missing) Exact (s, v)
  match s
  complete s
  return v
  or just Exact s with v = ()

  Dir
  match any token
  complete as directory
  return the token
  is like Any dir

  (missing) Fail
  match nothing
  complete nothing
  return exception

  File
  match any token
  complete as file
  return the token
  is like Any file

  List t
  (t must be non empty otherwise it can be matched infinitely many times)
  match nothing or t or t t or t t t or ...
  complete as t
  return the list of [| t |]
  is like Or(Value [], NonEmptyList t)
  or like Or(Value [], Apply(List.cons, Then(t, List t)))

  NonEmptyList t
  (t must be non empty)
  match t or t t or t t t or ...
  complete as t
  return the list of [| t |]
  is like Apply(List.cons, Then(t, List t))

  Nothing
  match empty (always succeed)
  complete nothing
  return ()
  is like Value ()
  
  (missing) Option t
  match t or empty
  complete as t
  return Some [| t |] if matched, None otherwise
  is like Or(Apply((fun x -> Some x), t), Value None)

  Or (t1, t2)
  (t1 must be non-empty otherwise it is always matched)
  match t1 or t2
  complete as t1 and t2
  return [| t1 |] if matched, [| t2 |] otherwise
  could be replaced with Switch, so as to ensure LR(1)
  
  (missing) Switch ([s1, t1, _; s2, t2, _; ...], td)
  (s1, s2, ... must be all different)
  match s1 followed by t1 or s2 followed by t2 or ... or td
  complete s1, s2, ... and as td
  return [| t1 |] if first token is s1, ..., [| td |] otherwise
  is like Or(Commands [...], td)

  Then (t1, t2)
  match t1 followed by t2
  complete as t1, or t2 if t1 is empty
  return ([| t1 |], [| t2 |])
  when t1 is empty, could be replaced by Apply((fun x -> [| t1 |], x), t2)
  
  Value v
  match empty (always succeed)
  complete nothing
  return v
  is like Apply((fun () -> v), Nothing)
*)
