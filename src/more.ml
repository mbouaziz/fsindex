
let (@>) f g = fun x -> g (f x)
let fst3 (a, _, _) = a
let snd3 (_, b, _) = b

let listRevSplit l =
  let rec aux ((rx, ry) as acc) = function
  | [] -> acc
  | (x, y)::l -> aux (x::rx, y::ry) l
  in
  aux ([], []) l

let rec listKeep n l = match l with (* /!\ not tailrec *)
| [] -> l
| _ when n <= 0 -> []
| hd::tl -> hd::(listKeep (n-1) tl)

let listKeepLast n l =
  if n <= 0 then [] else
    let rec aux all l = match l with
    | [] -> List.nth all (n-1)
    | _::tl -> aux (l::all) tl
    in
    try aux [] l with _ -> l

let rec listLast = function
| [] -> raise Not_found
| [x] -> x
| _::l -> listLast l

let stringStartsWith pref s =
  String.length s >= String.length pref &&
    String.sub s 0 (String.length pref) = pref

let stringChopPrefix s pref =
  String.sub s (String.length pref) (String.length s - String.length pref)

let stringIndexListFrom s i cList =
  let l = String.length s in
  let r = List.fold_left (fun r c -> min r (try String.index_from s i c with Not_found -> l)) l cList in
  if r = l then raise Not_found
  else r

let stringSplitKeepLastEmpty s cList =
  let l = String.length s in
  if l = 0 then []
  else
    let rec aux i =
      if i >= l then [""]
      else match stringIndexListFrom s i cList with
      | j when j = i -> aux (j+1)
      | j -> (String.sub s i (j-i))::(aux (j+1))
      | exception Not_found -> [String.sub s i (l-i)]
    in
    aux 0

let commaSeparatedString s =
  let l = String.length s in
  if l <= 3 then s
  else
    let l' = l + (l-1)/3 in
    let g k =
      let k' = l' - k - 1 in
      if k' mod 4 = 3 then ','
      else s.[l - k' + k'/4 - 1]
    in
    String.init l' g

let commaSeparatedInt64 i = commaSeparatedString (Int64.to_string i)
let commaSeparatedInt i = commaSeparatedString (string_of_int i)


module H = struct
  include Digest

  let combine list =
    let list = List.sort Pervasives.compare list in
    string (String.concat "" (List.rev_map to_hex list))

  let empty = string ""

end

module Int64Map = Map.Make(Int64)
module HMap = Map.Make(H)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module StringUnsortedPair = struct
  type t = string * string
  let compare = Pervasives.compare
  let make s1 s2 =
    if s1 < s2 then s1, s2 else s2, s1
end
module SUP = StringUnsortedPair
module SUPMap = Map.Make(SUP)
