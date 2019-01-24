module type RBTreeEl = sig 
  type t
  val compare: t -> t -> bool
end

module Make(E: RBTreeEl) = struct
  type color =
    | Red
    | Black

  type t = 
    | Leaf
    | Node of color * E.t * t * t

  let init =
    Leaf

  let rec balance = function
    | (Black, z, Node (Red, y, Node (Red, x, a, b), c), d)
    | (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d)
    | (Black, x, a, Node (Red, z, Node (Red, y, b, c), d))
    | (Black, x, a, Node (Red, y, b, Node (Red, z, c, d))) -> 
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | (color, x, a, b) -> 
      Node (color, x, a, b)

  let insert tree value =
    let rec insert' tree =
      match tree with
      | Leaf -> 
        Node (Red, value, Leaf, Leaf)
      | Node (color, nv, l, r) when E.compare nv value ->
        balance (color, nv, (insert' l), r)
      | Node (color, nv, l, r) when E.compare value nv ->
        balance (color, nv, l, (insert' r))
      | _ -> 
        tree in 
    match insert' tree with
    | Node (_, x, l, r) -> Node (Black, x, l, r)
    | _ -> failwith "Invalid tree"

  let rec mem tree value =
    match tree with
    | Leaf -> false
    | Node (_, x, a, _) when E.compare x value ->
      mem a value
    | Node (_, x, _, b) when E.compare value x ->
      mem b value
    | _ ->
      true
end

module Int_tree = Make(struct
  type t = int
  let compare = (>)
end)
