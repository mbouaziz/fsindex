
open More


let formatSize = commaSeparatedInt64
let formatInt = commaSeparatedInt

let formatPercent f =
  Printf.sprintf "%2.2f" (f *. 100.)

module FsNode = struct

  type node = { nbFiles: int; size: int64 }
  type leaf = int64 * float * H.t

  let emptyNode = { nbFiles = 0; size = 0L }

  let nodeOfLeaf (size, _, _) = { nbFiles = 1; size = size }

  let replace fromNode toNode inNode =
    { nbFiles = inNode.nbFiles - fromNode.nbFiles + toNode.nbFiles;
      size = Int64.add (Int64.sub inNode.size fromNode.size) toNode.size }
end

module FsTree = PathTree.Make(FsNode)

module DirHashNode = struct

  type leaf = int64 * H.t
  type node = leaf * leaf list

  let emptyNode = (0L, H.empty), []

  let nodeOfLeaf x = x, [x]

  let replace _fromNode _toNode _inNode = assert false

  let makeNode leaves =
    let sizes, hashes = listRevSplit leaves in
    let size = List.fold_left Int64.add 0L sizes in
    let hash = H.combine hashes in
    (size, hash), leaves

end

module DHTree = PathTree.Make(DirHashNode)

type 'tree genIndex = {
  hashes: StringSet.t HMap.t Int64Map.t;
  tree: 'tree;
}

type index = FsTree.t genIndex

module Index = struct

  let empty = { hashes = Int64Map.empty;
                tree = FsTree.empty }

  let fromFile filename =
    let ic = open_in_bin filename in
    let index = Marshal.from_channel ic in
    close_in ic;
    (index : index)

  let fromFileOrEmpty filename =
    try fromFile filename with _ -> empty

  let toFile filename (index : index) =
    let oc = open_out_bin filename in
    Marshal.to_channel oc index [];
    close_out oc

  let mkSub f index path =
    let subTree, mk = f index.tree path in
    let mk index = { index with tree = mk index.tree } in
    { index with tree = subTree }, mk

  let subPath = mkSub FsTree.subPath
  let subElt = mkSub FsTree.subElt

  let switchFold onLeaf onNodeElt index =
    FsTree.switchFold onLeaf onNodeElt index.tree index

end

module Exclusion = struct

  module ExclNode = struct
    type node = unit
    type leaf = unit
    let emptyNode = ()
    let nodeOfLeaf unit = unit
    let replace _from _to _in = assert false
  end

  module ExclTree = PathTree.Make(ExclNode)

  let add f tree =
    let f = Path.makeAbsolute f in
    let path = Path.ofString f in
    let rpath = List.rev path.Path.l in
    let rec aux tree rpath =
      match rpath with
      | [] -> ExclTree.leaf ()
      | name::rpath ->
        let onLeaf () = tree in
        let onNode () m =
          let subTree = try StringMap.find name m with Not_found -> ExclTree.empty in
          ExclTree.node () (StringMap.add name (aux subTree rpath) m)
        in
        ExclTree.switch onLeaf onNode tree
    in
    aux tree rpath

  let read filename =
    match open_in filename with
    | ic ->
      let rec aux tree =
        match String.trim (input_line ic) with
        | "" -> aux tree
        | f when f.[0] = '#' -> aux tree
        | f -> aux (add f tree)
        | exception End_of_file ->
          close_in ic;
          tree
      in
      aux ExclTree.empty
    | exception _ -> ExclTree.empty

  let filename indexFile =
    indexFile ^ ".excl"

  let isExcluded tree =
    ExclTree.switch (fun _ -> true) (fun _ _ -> false) tree

  let subElt tree filename =
    fst (ExclTree.subElt tree filename)

end

let rmFileFromHashes index size h filename = 
  let hm = Int64Map.find size index.hashes in
  let fs = HMap.find h hm in
  let fs = StringSet.remove filename fs in
  let hm = if StringSet.is_empty fs then
      HMap.remove h hm
    else
      HMap.add h fs hm in
  let hashes = if HMap.is_empty hm then
      Int64Map.remove size index.hashes
    else
      Int64Map.add size hm index.hashes
  in
  { index with hashes = hashes }

let isFileInHashes index size h filename =
  match Int64Map.find size index.hashes with
  | hm ->
    begin match HMap.find h hm with
    | fs -> StringSet.mem filename fs
    | exception Not_found -> false
    end
  | exception Not_found -> false

let addFileToHashes index size h filename =
  let hm = match Int64Map.find size index.hashes with
  | hm ->
    let fs = match HMap.find h hm with
    | fs -> StringSet.add filename fs
    | exception Not_found -> StringSet.singleton filename
    in
    HMap.add h fs hm
  | exception Not_found -> HMap.singleton h (StringSet.singleton filename)
  in
  { index with hashes = Int64Map.add size hm index.hashes }

let rec rmAllFiles index path =
  let onLeaf (sz, _, h) index = rmFileFromHashes index sz h (Path.toString path) in
  let onNodeElt name tree index = rmAllFiles { index with tree = tree } (Path.concat path name) in
  let index = Index.switchFold onLeaf onNodeElt index in
  { index with tree = FsTree.empty }

let rmRemovedFiles path files index =
  let onLeaf _ index = index in
  let onNodeElt name t index =
    if StringSet.mem name files then index
    else
      let index, mk = Index.subElt index name in
      mk (rmAllFiles index (Path.concat path name))
  in
  Index.switchFold onLeaf onNodeElt index

let addRegFileToIndex index path stats =
  let size = stats.Unix.LargeFile.st_size in
  let filename = Path.toString path in
  Printf.printf "File %s (%s) " filename (formatSize size);
  let toAdd, toRm = match FsTree.getLeaf index.tree with
  | sz, _, h when sz <> size ->
    Printf.printf "changed (size was %s)\n" (formatSize sz);
    true, Some (sz, h)
  | sz, tm, h when tm < stats.Unix.LargeFile.st_mtime ->
    Printf.printf "changed (time)\n";
    true, Some (sz, h)
  | _ ->
    Printf.printf "unchanged\n";
    false, None
  | exception Not_found ->
    Printf.printf "new\n";
    true, None
  in
  let index = match toRm with
  | Some (sz, h) -> rmFileFromHashes index sz h filename
  | None -> index
  in
  if toAdd then
    match H.file filename with
    | h ->
      let index = addFileToHashes index size h filename in
      { index with tree = FsTree.leaf (size, Unix.gettimeofday (), h) }
    | exception _ ->
      Printf.printf "Failed to hash file %s\n" filename;
      index
  else
    index

let rec addFileToIndex excl index path =
  if Exclusion.isExcluded excl then begin
    Printf.printf "Excluded file %s\n" (Path.toString path);
    index
  end else
    match Unix.LargeFile.lstat (Path.toString path) with
    | stats ->
      let index = match stats.Unix.LargeFile.st_kind with
      | Unix.S_REG -> addRegFileToIndex index path stats
      | Unix.S_DIR -> addDirToIndex excl index path
      | _ ->
        Printf.printf "Ignore file %s\n" (Path.toString path);
        index
      in
      index
    | exception _ ->
      Printf.printf "Failed to lstat file %s\n" (Path.toString path);
      index

and addDirToIndex excl index path =
  Printf.printf "In %s\n" (Path.toString path);
  let rec aux dh files index =
    match Unix.readdir dh with
    | filename when filename = Filename.current_dir_name
               || filename = Filename.parent_dir_name ->
      aux dh files index
    | filename ->
      let index, mk = Index.subElt index filename in
      let index = addFileToIndex (Exclusion.subElt excl filename) index (Path.concat path filename) in
      aux dh (StringSet.add filename files) (mk index)
    | exception End_of_file ->
      Unix.closedir dh;
      rmRemovedFiles path files index
  in
  match Unix.opendir (Path.toString path) with
  | dh -> aux dh StringSet.empty index
  | exception _ ->
    Printf.printf "Failed!\n";
    index

let addOneToIndex excl index filename =
  let path = Path.ofString filename in
  let index, mk = Index.subPath index path in
  let index = addFileToIndex excl index path in
  mk index

let printSummaryDiff node0 node1 =
  if node1.FsNode.nbFiles < node0.FsNode.nbFiles then
    Printf.printf "%s file(s) removed\n" (formatInt (node0.FsNode.nbFiles - node1.FsNode.nbFiles))
  else
    Printf.printf "%s file(s) added\n" (formatInt (node1.FsNode.nbFiles - node0.FsNode.nbFiles));
  if node1.FsNode.size < node0.FsNode.size then
    Printf.printf "%s byte(s) removed\n" (formatSize (Int64.sub node0.FsNode.size node1.FsNode.size))
  else
    Printf.printf "%s byte(s) added\n" (formatSize (Int64.sub node1.FsNode.size node0.FsNode.size))

let addToSavedIndex dirl excl index =
  let node0 = FsTree.nodeOfTree index.tree in
  let index = List.fold_left (addOneToIndex excl) index (List.map Path.makeAbsolute dirl) in
  let node1 = FsTree.nodeOfTree index.tree in
  printSummaryDiff node0 node1;
  index

let rmOneFromIndex index filename =
  let path = Path.ofString filename in
  let index, mk = Index.subPath index path in
  let index = rmAllFiles index path in
  mk index

let rmFromSavedIndex dirl index =
  let node0 = FsTree.nodeOfTree index.tree in
  let index = List.fold_left rmOneFromIndex index (List.map Path.makeAbsolute dirl) in
  let node1 = FsTree.nodeOfTree index.tree in
  printSummaryDiff node0 node1;
  index

let listDup index =
  let forHash size _h set l =
    if StringSet.is_empty set || (StringSet.min_elt set == StringSet.max_elt set) then l
    else (size, StringSet.elements set)::l
  in
  let forSize size hm l = HMap.fold (forHash size) hm l in
  Int64Map.fold forSize index.hashes []

let listFiles index =
  let forHash size _h set l = (size, StringSet.elements set)::l in
  let forSize size hm l = HMap.fold (forHash size) hm l in
  Int64Map.fold forSize index.hashes []

let noFilter x = x

let printFileList l =
  let printOneFile filename =
    Printf.printf " %s\n" filename
  in
  let printOne (size, list) =
    Printf.printf "Size %s\n" (formatSize size);
    List.iter printOneFile list
  in
  List.iter printOne l

let printSizeLost fileList =
  let totalSize = List.fold_left (fun tot (size, list) -> Int64.add tot (Int64.mul (Int64.of_int ((List.length list) - 1)) size)) 0L fileList in
  Printf.printf "Total size lost: %s\n" (formatSize totalSize)

let printDup filter final () index =
  let ldup = listDup index in
  let ldup = filter ldup in
  let ldup = List.sort Pervasives.compare ldup in
  printFileList ldup;
  final ldup

type simInfo = {
  simIndex: float;
  fileSim: float;
  sizeSim: float;
  simFiles: int;
  simSize: int64;
}
  
type sim = simInfo SUPMap.t

let dirHashIndex index =
  let rec aux path hashes tree =
    let onLeaf (sz, _, h) = { hashes = hashes; tree = DHTree.leaf (sz, h) } in
    let onNode _ m =
      let f name tree (m, hashes) =
        let index = aux (Path.concat path name) hashes tree in
        let m = StringMap.add name index.tree m in
        m, index.hashes
      in
      let m, hashes = StringMap.fold f m (StringMap.empty, hashes) in
      let dhl = List.map (snd @> DHTree.nodeOfTree @> snd) (StringMap.bindings m) in
      let dh = List.flatten dhl in
      let dh = List.sort Pervasives.compare dh in
      let ((size, hash), dh) as node = DirHashNode.makeNode dh in
      let index = { hashes = hashes; tree = DHTree.node node m } in
      addFileToHashes index size hash (Path.toString path)
    in
    FsTree.switch onLeaf onNode tree
  in
  aux Path.empty Int64Map.empty index.tree

let printDDup () index =
  let rec self_or_parent_in path set =
    if StringSet.mem path set then true
    else match Path.filenameDirOpt path with
    | Some path -> self_or_parent_in path set
    | None -> false
  in
  let rec filter set acc list =
    match list with
    | [] -> acc
    | (size, _)::list when size <= 1L -> filter set acc list
    | (size, l)::list ->
      let rec aux set acc l =
        match l with
        | [] -> set, acc
        | f::l when self_or_parent_in f set -> aux set acc l
        | f::l -> aux (StringSet.add f set) (f::acc) l
      in
      let set', l = aux set [] l in
      match l with
      | [] | [_] -> filter set acc list
      | l -> filter set' ((size, l)::acc) list
  in
  printDup (filter StringSet.empty []) printSizeLost () (dirHashIndex index)

let listSim maxToShow () index =
  let dirHashIndex = dirHashIndex index in
  let dirHashTree = dirHashIndex.tree in
  let getDirHashes dir = DHTree.nodeOfSubPath dirHashTree (Path.ofString dir) in
  let compSim (dir1, dir2) =
    Printf.printf "Comparing %s and %s\n" dir1 dir2;
    let ((s1, _) as dH1), dh1 = getDirHashes dir1 in
    let ((s2, _) as dH2), dh2 = getDirHashes dir2 in
    let n1 = List.length dh1 in
    let n2 = List.length dh2 in
    let simF, simS =
      if dH1 = dH2 then min n1 n2, s1
      else
        let rec aux dh1 dh2 ((simF, simS) as simInf) = match dh1, dh2 with
        | _, [] | [], _ -> simInf
        | f1::dh1, f2::dh2 when f1 = f2 -> aux dh1 dh2 (simF + 1, Int64.add simS s1)
        | f1::dh1, f2::_ when f1 < f2 -> aux dh1 dh2 (simF, simS)
        | _, _::dh2 -> aux dh1 dh2 (simF, simS)
        in
        aux dh1 dh2 (0, 0L)
    in
    let maxN = max n1 n2 in
    let maxS = max s1 s2 in
    let fileSim = if maxN = 0 then 0. else (float_of_int simF) /. (float_of_int maxN) in
    let sizeSim = if maxS = 0L then 0. else (Int64.to_float simS) /. (Int64.to_float maxS) in
    let simIndex = max fileSim sizeSim in
    { simIndex = simIndex;
      fileSim = fileSim;
      sizeSim = sizeSim;
      simFiles = simF;
      simSize = simS }
  in
  let rec trySim dir1 dir2 sim =
    if dir1 = dir2 || Path.filenameIsParent dir1 dir2 || Path.filenameIsParent dir2 dir1 then sim
    else
      let dirp = SUP.make dir1 dir2 in
      if SUPMap.mem dirp sim then sim
      else
        let si = compSim dirp in
        let sim = SUPMap.add dirp si sim in
        match si, Path.filenameDirOpt dir1, Path.filenameDirOpt dir2 with
        | si, Some dir1, Some dir2 when si.simIndex > 0.1 -> trySim dir1 dir2 sim
        | _ -> sim
  in
  let rec tryPairsWith dir1 files sim = match files with
  | [] -> sim
  | file2::files ->
    let sim = trySim dir1 (Filename.dirname file2) sim in
    tryPairsWith dir1 files sim
  in
  let rec tryPairs files sim = match files with
  | [] | [_] -> sim
  | file1::files ->
    let sim = tryPairsWith (Filename.dirname file1) files sim in
    tryPairs files sim
  in
  let forHash _h set sim =
    tryPairs (StringSet.elements set) sim
  in
  let forSize size hm sim =
    if size >= 10L then
      HMap.fold forHash hm sim
    else
      sim
  in
  let sim = Int64Map.fold forSize index.hashes SUPMap.empty in
  let siml = SUPMap.bindings sim in
  let simEltVal (_, si) = si.simIndex, si.sizeSim, si.simSize, si.simFiles in
  let simEltCmp e1 e2 = Pervasives.compare (simEltVal e1) (simEltVal e2) in
  let siml = List.sort simEltCmp siml in
  let siml = listKeepLast maxToShow siml in
  let printSim ((d1, d2), si) =
    Printf.printf "Similarity %s%%: files %s%% (%s), size %s%% (%s)\n"
      (formatPercent si.simIndex)
      (formatPercent si.fileSim) (formatInt si.simFiles)
      (formatPercent si.sizeSim) (formatSize si.simSize);
    Printf.printf " Path %s\n" d1;
    Printf.printf "  and %s\n\n" d2;
  in
  List.iter printSim siml

let printStats () index =
  Printf.printf "Sizes: %s different\n" (formatInt (Int64Map.cardinal index.hashes));
  Printf.printf "       from %s\n" (formatSize (fst (Int64Map.min_binding index.hashes)));
  Printf.printf "       to %s\n\n" (formatSize (fst (Int64Map.max_binding index.hashes)));
  let count, sizeLost = Int64Map.fold (
    fun sz h (count, sizeLost) ->
      let card = HMap.cardinal h in
      count + card, Int64.add sizeLost (Int64.mul sz (Int64.of_int (card - 1)))
  ) index.hashes (0, 0L) in
  Printf.printf "Hashes: %s\n\n" (formatInt count);
  let node = FsTree.nodeOfTree index.tree in
  Printf.printf "Files: %s\n" (formatInt node.FsNode.nbFiles);
  Printf.printf "Total size: %s\n" (formatSize node.FsNode.size);
  Printf.printf "Total size lost: %s\n" (formatSize sizeLost)

let collectAllFiles tree =
  let nodes = FsTree.getNodes tree in
  let nodeV (_, n) = n.FsNode.size in
  let cmpNode n1 n2 = Pervasives.compare (nodeV n1) (nodeV n2) in
  List.sort cmpNode nodes

let diskUsage maxToShow filename index =
  let filename = Path.makeAbsolute filename in
  let path = Path.ofString filename in
  let tree, _ = FsTree.subPath index.tree path in
  let root = FsTree.nodeOfTree tree in
  if root.FsNode.size <= 0L then
    Printf.printf "Empty!\n"
  else
    let files = collectAllFiles tree in
    let files = listKeepLast maxToShow files in
    let printFile (path, n) =
      Printf.printf "%s%% (%s) %s (%s file(s))\n"
        (formatPercent ((Int64.to_float n.FsNode.size) /. (Int64.to_float root.FsNode.size)))
        (formatSize n.FsNode.size) (Path.toString path)
        (formatInt n.FsNode.nbFiles);
    in
    List.iter printFile files

let diskUsageTree maxToShow filename index =
  let path = Path.ofString filename in
  let tree, _ = FsTree.subPath index.tree path in
  let root = FsTree.nodeOfTree tree in
  if root.FsNode.size <= 0L then
    Printf.printf "Empty!\n"
  else
    let files = collectAllFiles tree in
    let files = listKeepLast maxToShow files in
    let addToSet s (path, _) = StringSet.add (Path.toString path) s in
    let bigFiles = List.fold_left addToSet StringSet.empty files in
    let printFile margin path n isFile =
      Printf.printf "%s%s%% (%s) %s%s\n" margin
        (formatPercent ((Int64.to_float n.FsNode.size) /. (Int64.to_float root.FsNode.size)))
        (formatSize n.FsNode.size) (Path.toString path)
        (if isFile then "" else Printf.sprintf " (%s file(s))" (formatInt n.FsNode.nbFiles))
    in
    let rec walkPath margin path tree =
      if StringSet.mem (Path.toString path) bigFiles then begin
        let onFile leaf = printFile margin path (FsNode.nodeOfLeaf leaf) true in
        let onNode node m =
          printFile margin path node false;
          let l = StringMap.bindings m in
          let nodeV (_, tree) = (FsTree.nodeOfTree tree).FsNode.size in
          let cmpNode n1 n2 = Pervasives.compare (nodeV n1) (nodeV n2) in
          let l = List.sort cmpNode l in
          List.iter (walk (margin ^ "  ") path) l in
        FsTree.switch onFile onNode tree
      end
    and walk margin parent (name, tree) =
      walkPath margin (Path.concat parent name) tree
    in
    walkPath "" Path.empty tree

let printHashes () index =
  printFileList (listFiles index)  

let printDirHashes () index =
  printHashes () (dirHashIndex index)

let checkIndex () index =
  let rec checkTree path index =
    let onLeaf (sz, _, h) index =
      if isFileInHashes index sz h (Path.toString path) then index
      else begin
        Printf.printf "%s (hash missing)\n" (Path.toString path);
        { index with tree = FsTree.empty }
      end
    in
    let onNodeElt name t index =
      let index, mk = Index.subElt index name in
      mk (checkTree (Path.concat path name) index)
    in
    Index.switchFold onLeaf onNodeElt index
  in
  let checkHashes index =
    let forFile size h filename index =
      let path = Path.ofString filename in
      let tree, mk = FsTree.subPath index.tree path in
      match FsTree.getLeaf tree with
      | (sz, _, _) when size <> sz ->
        Printf.printf "%s (size mismatch)\n" filename;
        let index = rmFileFromHashes index size h filename in
        { index with tree = mk FsTree.empty }
      | (_, _, h') when h <> h' ->
        Printf.printf "%s (hash mismatch)\n" filename;
        let index = rmFileFromHashes index size h filename in
        { index with tree = mk FsTree.empty }
      | _ -> index
      | exception Not_found ->
        Printf.printf "%s (missing tree leaf)\n" filename; (* todo: diff empty/non-empty dir *)
        rmFileFromHashes index size h filename
    in
    let forHash size h set index = StringSet.fold (forFile size h) set index in
    let forSize size hm index = HMap.fold (forHash size) hm index in
    Int64Map.fold forSize index.hashes index
  in
  let index = checkTree Path.empty index in
  checkHashes index

type doRmConfig = {
  verbosity : int;
  force : bool;
}

let doRmDefaultConfig = {
  verbosity = 0;
  force = false;
}

let doRm _ index =
  failwith "Not implemented"

let withIndexFileRO f indexFile =
  let index = Index.fromFileOrEmpty indexFile in
  f index

let withIndexFileRW f indexFile =
  let index = Index.fromFileOrEmpty indexFile in
  let index = f index in
  Index.toFile indexFile index

let withIndexAndExclFilesRW f indexFile =
  let index = Index.fromFileOrEmpty indexFile in
  let excl = Exclusion.read (Exclusion.filename indexFile) in
  let index = f excl index in
  Index.toFile indexFile index


let maxToShow = 10000
let maxToShowSmall = 200
let myName = "fsindex"


module DoRmCmd = struct
  open ArgSpec

  let incrVerb cfg = { cfg with verbosity = cfg.verbosity + 1 }
  let decrVerb cfg = { cfg with verbosity = cfg.verbosity - 1 }
  let setForce cfg = { cfg with force = true }

  let commands0 = [
    "--verbose", Value incrVerb, "increase verbosity";
    "--quiet", Value decrVerb, "decrease verbosity";
    "--force", Value setForce, "really remove files";
  ]
  let commands1 = [
    "--collect", Then (Value (true, false), NonEmptyList Dir), "collect hashes in these directories";
    "--collectrm", Then (Value (true, true), NonEmptyList Dir), "collect hashes and remove files in these directories";
    "--rm", Then (Value (false, true), NonEmptyList Dir), "remove files in these directories";
  ]

  let args = Then (List (Commands commands0), NonEmptyList (Commands commands1))
end

module IndexCmd = struct
  open ArgSpec

  let rwexcl f arg = withIndexAndExclFilesRW (f arg)
  let rw f arg = withIndexFileRW (f arg)
  let ro f arg = withIndexFileRO (f arg)

  let commands = [
    "add", Apply (rwexcl addToSavedIndex, NonEmptyList File), "add/update files/directories to index";
    "rm", Apply (rw rmFromSavedIndex, NonEmptyList File), "remove files/directories from index";
    "ldup", Apply (ro (printDup noFilter printSizeLost), Nothing), "list duplicate files";
    "ddup", Apply (ro printDDup, Nothing), "list duplicate directories";
    "lsim", Apply (ro (listSim maxToShow), Nothing), "list similar directories";
    "stats", Apply (ro printStats, Nothing), "print stats";
    "du", Apply (ro (diskUsage maxToShow), Dir), "print biggest files/directories in some directory";
    "dut", Apply (ro (diskUsageTree maxToShowSmall), Dir), "print biggest files/directories in some directory (shown as a tree)";
    "phashes", Apply (ro printHashes, Nothing), "print all files in index";
    "pdhashes", Apply (ro printDirHashes, Nothing), "print all directories in index";
    "check", Apply (rw checkIndex, Nothing), "check index and remove partial entries";
    "dorm", Apply (ro doRm, DoRmCmd.args), "remove duplicates";
  ]

  let args = Commands commands
end

module MainCmd = struct
  open ArgSpec  

  let help () = raise (Arg.Help "Help requested")

  let bashCompletion () =
    Printf.printf "# Bash completion script for %s\n" myName;
    Printf.printf "# Put this script in /etc/bash_completion.d/\n\n";
    Printf.printf "complete -C %s %s\n" myName myName

  let commands = [
    "--help", Apply (help, Nothing), "print this help";
    "--bashcompletion", Apply (bashCompletion, Nothing), "print completion script";
  ]

  let default =
    let apply (index, f) = f index in
    Apply (apply, Then (File, IndexCmd.args))

  let args = Or (Commands commands, default)
end

let main () =
  let usage () =
    Printf.printf "Usage (1): %s <command> [<command args>]\n\n" myName;
    Printf.printf "where <command> can be:\n";
    Args.usage MainCmd.commands;
    Printf.printf "\n";
    Printf.printf "Usage (2): %s <index file> <command> [<command args>]\n\n" myName;
    Printf.printf "where <command> can be:\n";
    Args.usage IndexCmd.commands;
    Printf.printf "\n";
    Printf.printf "Usage (3): %s <index file> dorm [<option>+] [<command> <dirs>]+\n\n" myName;
    Printf.printf "where <option> can be:\n";
    Args.usage DoRmCmd.commands0;
    Printf.printf "\nand <command> can be:\n";
    Args.usage DoRmCmd.commands1;
    Printf.printf "\n"
  in
  let help msg =
    Printf.printf "%s\n\n" msg;
    usage ()
  in
  try
    Args.compute MainCmd.args (List.tl (Array.to_list Sys.argv))
  with
  | Arg.Help msg -> help msg
  | Arg.Bad msg -> help ("Error: " ^ msg)

let complete line =
  let wordBreaks = [' ';'\t';'\r';'\n'] in
  let point = try int_of_string (Sys.getenv "COMP_POINT") with Not_found -> String.length line in
  let line = if point < String.length line then String.sub line 0 point else line in
  let args = stringSplitKeepLastEmpty line wordBreaks in
  match args with
  | [] -> ()
  | _::args -> Args.doComplete MainCmd.args args

let mainOrComplete () =
  match Sys.getenv "COMP_LINE" with
  | line -> complete line
  | exception Not_found -> main ()

let _ =
  Args.sanityCheck MainCmd.args;
  Printexc.record_backtrace true;
  try mainOrComplete () with
  | exn ->
    Printf.printf "Exception:\n %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout
