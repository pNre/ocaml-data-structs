module Make_heap(C : sig type t;; val compare : t -> t -> bool end) = struct
  type t =
    | Leaf
    | Node of C.t * int * t * t (* val, height, left subtree, right subtree *)
  
  let rec heapify node =
    match node with
    | Node (v, h, Node (lv, lh, ll, lr), Leaf) when C.compare lv v ->
      Node (lv, h, heapify (Node (v, lh, ll, lr)), Leaf)
    | Node (v, h, Leaf, Node (rv, rh, rl, rr)) when C.compare rv v ->
      Node (rv, h, Leaf, heapify (Node (v, rh, rl, rr)))
    | Node (v, h, Node (lv, lh, ll, lr), Node (rv, rh, rl, rr)) when C.compare lv v && C.compare lv rv ->
      Node (lv, h, heapify (Node (v, lh, ll, lr)), Node (rv, rh, rl, rr))
    | Node (v, h, Node (lv, lh, ll, lr), Node (rv, rh, rl, rr)) when C.compare rv v ->
      Node (rv, h, Node (lv, lh, ll, lr), heapify (Node (v, rh, rl, rr)))
    | _ ->
      node

  let height heap =
    match heap with
    | Leaf -> 0
    | Node (_, h, _, _) -> h

  let rec insert x heap =
    match heap with
    | Leaf ->
      Node (x, 0, Leaf, Leaf)
    | Node (y, h, l, r) ->
      let (top, bottom) = if C.compare x y then (x, y) else (y, x) in
      match l, r with
      | Leaf, Leaf -> 
        Node (top, h + 1, Node (bottom, h, Leaf, Leaf), Leaf)
      | _, Leaf -> 
        Node (top, h, l, Node (bottom, 0, Leaf, Leaf))
      | Leaf, _ ->
        Node (top, h, Node (bottom, 0, Leaf, Leaf), r)
      | Node (_, hl, _, _), Node (_, hr, _, _) when hl <= hr ->
        let left_branch = insert bottom l in
        Node (top, (height left_branch) + 1, left_branch, r)
      | _, Node (_, hr, _, _) ->
        let right_branch = insert bottom r in
        Node (top, (height right_branch) + 1, l, right_branch)

  let root heap =
    match heap with
    | Leaf -> None
    | Node (x, _, _, _) -> Some x

  let rec remove_lowest_leaf heap =
    match heap with
    | Leaf -> failwith "Empty heap"
    | Node (v, _, Leaf, Leaf) -> Leaf, v
    | Node (v, _, l, Leaf) ->
      let subtree, n = remove_lowest_leaf l in
      Node (v, height subtree, subtree, Leaf), n
    | Node (v, _, Leaf, r) ->
      let subtree, n = remove_lowest_leaf r in
      Node (v, height subtree, Leaf, subtree), n
    | Node (v, h, l, r) when height l <= height r ->
      let subtree, n = remove_lowest_leaf r in
      Node (v, max (height l) (height subtree) + 1, l, subtree), n
    | Node (v, h, l, r) ->
      let subtree, n = remove_lowest_leaf l in
      Node (v, max (height r) (height subtree) + 1, subtree, r), n

  let rec remove_root heap =
    match remove_lowest_leaf heap with
    | Leaf, _ -> Leaf
    | Node (_, h, l, r), v -> heapify (Node (v, h, l, r))

  let from_list list =
    List.fold_left (fun heap x -> insert x heap) Leaf list

  let sort_list list =
    let rec sort heap =
      match heap with
      | Leaf -> []
      | Node (v, _, _, _) -> v :: (sort (remove_root heap))
    in sort (from_list list)
end

module Max_heap = Make_heap(struct 
  type t = int
  let compare = ( > ) 
end)

module Min_heap = Make_heap(struct 
  type t = int
  let compare = ( < ) end
)
