
type _ argSpec =
  | Apply : ('a -> 'b) * 'a argSpec -> 'b argSpec
  | Commands : (string * 'a argSpec * _) list -> 'a argSpec
  | Dir : string argSpec
  | File : string argSpec
  | List : 'a argSpec -> 'a list argSpec
  | Nothing : unit argSpec
  | Or : 'a argSpec * 'a argSpec -> 'a argSpec
  | Then : 'a argSpec * 'b argSpec -> ('a * 'b) argSpec
