
open More

module type NodeAndLeaf = sig

  type node
  type leaf
  val emptyNode: node
  val isEmptyNode: node -> bool
  val nodeOfLeaf: leaf -> node
  val replace: (* from *) node -> (* to *) node -> (* in *) node -> node

end

module Make(NAL : NodeAndLeaf) = struct

  type t =
  | Node of (NAL.node * t StringMap.t)
  | Leaf of NAL.leaf

  let nodeEmpty = NAL.emptyNode, StringMap.empty
  let empty = Node nodeEmpty

  let isEmpty = function
  | Leaf _ -> false
  | Node (n, m) -> NAL.isEmptyNode n && StringMap.is_empty m

  let nodeOfTree = function
  | Leaf l -> NAL.nodeOfLeaf l
  | Node (n, _) -> n

  let nodeReplace pathElt ot t (n, m) =
    let n = NAL.replace (nodeOfTree ot) (nodeOfTree t) n in
    let m = if isEmpty t then StringMap.remove pathElt m
      else StringMap.add pathElt t m in
    n, m

  let getSubElt (tree, mk) pathElt =
    let (_, m) as nm = match tree with
    | Leaf _ -> nodeEmpty
    | Node nm -> nm
    in
    let subTree = try StringMap.find pathElt m with Not_found -> empty in
    let mk t =
      if t = subTree then mk tree
      else mk (Node (nodeReplace pathElt subTree t nm)) in
    subTree, mk

  let subElt tree pathElt = getSubElt (tree, fun x -> x) pathElt

  let subPath tree path =
    let rpath = List.rev path.Path.l in
    List.fold_left getSubElt (tree, fun x -> x) rpath

  let nodeOfSubPath tree path =
    let subTree, _ = subPath tree path in
    nodeOfTree subTree

  let getLeaf tree = match tree with
  | Leaf x -> x
  | Node _ -> raise Not_found (* TODO: make diff between empty/non-empty dir *)

  let getLeaves tree =
    let rec aux _ tree acc = match tree with
    | Leaf x -> x::acc
    | Node (_, m) -> StringMap.fold aux m acc
    in
    aux "" tree []

  let getNodes tree =
    let rec auxOfTree path tree acc =
      match tree with
      | Leaf x -> (path, NAL.nodeOfLeaf x)::acc
      | Node (n, m) -> StringMap.fold (aux path) m ((path, n)::acc)
    and aux path name tree acc =
      let path = Path.concat path name in
      auxOfTree path tree acc
    in
    auxOfTree Path.empty tree []

  let switch onLeaf onNode tree = match tree with
  | Leaf x -> onLeaf x
  | Node (n, t) -> onNode n t

  let switchFold onLeaf onNodeElt tree acc = match tree with
  | Leaf x -> onLeaf x acc
  | Node (_, m) -> StringMap.fold onNodeElt m acc

  let leaf x = Leaf x

  let node n m = Node (n, m)

end
