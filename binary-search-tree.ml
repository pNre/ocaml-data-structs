module type Comparable = sig
  type t
  val compare: t -> t -> bool
  val to_string: t -> string
end

module Make(Bst: Comparable) = struct
  type t =
    | Empty
    | Node of Bst.t * t * t

  let init = Empty

  let rec in_order_walk tree f =
    match tree with
    | Empty -> ()
    | Node (v, left, right) ->
      in_order_walk left f;
      f v;
      in_order_walk right f;;

  let dump tree =
    let rec dump' tree depth =
      match tree with
      | Empty -> ()
      | Node (v, left, right) ->
        Printf.printf "%s%s\n" (String.init depth (fun _ -> ' ')) (Bst.to_string v);
        dump' left (depth + 1);
        dump' right (depth + 1) in
    dump' tree 0

  let rec search tree x =
    match tree with
    | Empty -> None
    | Node (v, _, _) when v = x -> Some x
    | Node (v, _, right) when Bst.compare x v ->
      search right x
    | Node (v, left, _) ->
      search left x

  let rec insert tree node =
    match tree with
    | Empty -> Node (node, Empty, Empty)
    | Node (v, left, right) when Bst.compare node v -> Node (v, left, insert right node)
    | Node (v, left, right) -> Node (v, insert left node, right)

  let rec leftmost_node tree =
    match tree with
    | Empty -> Empty
    | Node (_, Empty, Empty) -> tree
    | Node (_, Empty, right) -> leftmost_node right
    | Node (_, left, _) -> leftmost_node left

  let rec leftmost_value tree =
    match leftmost_node tree with
    | Empty -> None
    | Node (v, _, _) -> Some v

  let rec rightmost_value tree =
    match tree with
    | Empty -> None
    | Node (v, Empty, Empty) -> Some v
    | Node (_, left, Empty) -> rightmost_value left
    | Node (_, _, right) -> rightmost_value right

  (* smallest node > root (if compare = (>)) *)
  let rec successor tree =
    match tree with
    | Empty -> None
    | Node (_, _, right) -> leftmost_value right

  let rec remove tree node =
    match tree with
    | Empty -> Empty
    | Node (v, left, right) when Bst.compare node v ->
      Node (v, left, remove right node)
    | Node (v, left, right) when Bst.compare v node ->
      Node (v, remove left node, right)
    | Node (v, left, right) ->
      match successor tree with
      | None -> 
        (* the right subtree is empty *)
        left
      | Some s -> 
        (* replace the root with the successor and delete the successor *)
        Node (s, left, remove right s)
end

module Int_bst = Make(struct
  type t = int
  let compare = ( > )
  let to_string = string_of_int
end)
