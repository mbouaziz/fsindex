
open More

type t = { l: string list;
           s: string }
  
let empty = { l = [];
              s = "/" } (* Unix only!!! *)

let concat path name = { l = name::path.l;
                         s = Filename.concat path.s name }

let toString path = path.s

let baseName path = match path.l with
| [] -> ""
| hd::_ -> hd

let filenameDirOpt filename =
  let d = Filename.dirname filename in
  if d = filename then None
  else Some d

let rec filenameLength filename =
  match filenameDirOpt filename with
  | None -> 1
  | Some d -> 1 + (filenameLength d)

let dirSepLength = String.length Filename.dir_sep

let filenameIsParent f1 f2 =
  let l1 = String.length f1 in
  let l2 = String.length f2 in
  l1 + dirSepLength < l2 && String.sub f2 l1 dirSepLength = Filename.dir_sep && String.sub f2 0 l1 = f1

let ofString path =
  let rec lOfString path =
    let b = Filename.basename path in
    match filenameDirOpt path with
    | None -> []
    | Some d -> b::(lOfString d)
  in
  { l = lOfString path;
    s = path }

let currentDirPrefix = Filename.concat Filename.current_dir_name ""
let parentDirPrefix = Filename.concat Filename.parent_dir_name ""

let rec smartConcat cwd filename =
  if not (Filename.is_relative filename) then filename
  else if filename = Filename.current_dir_name then cwd
  else if filename = Filename.parent_dir_name then Filename.dirname cwd
  else if Filename.is_implicit filename then Filename.concat cwd filename
  else if stringStartsWith currentDirPrefix filename then smartConcat cwd (stringChopPrefix filename currentDirPrefix)
  else if stringStartsWith parentDirPrefix filename then smartConcat (Filename.dirname cwd) (stringChopPrefix filename parentDirPrefix)
  else begin
    Printf.printf "Don't know how to smartConcat '%S' with '%S'\n" cwd filename;
    filename
  end

let cwd = Unix.getcwd ()

let makeAbsolute filename =
  smartConcat cwd filename
