type 'a t =
  | Empty
  | Node of 'a * 'a t

module type List = sig
  val insert: 'a t -> 'a -> 'a t
end

module Make(L: List) = struct
  let rec insert = L.insert
  
  let delete list item =
    match list with
    | Empty -> Empty
    | Node (n, next) -> next

  let rec search list item =
    match list with
    | Empty -> None
    | Node (n, _) when n = item -> Some list
    | Node (_, next) -> search next item

  let rec iter list f =
    match list with
    | Empty -> Empty
    | Node (n, next) -> 
      f n;
      Node (n, iter next f)

  let rec map list f =
    match list with
    | Empty -> Empty
    | Node (n, next) -> Node (f n, map next f)

  let rec filter list f =
    match list with
    | Empty -> Empty
    | Node (n, next) when f n -> Node (n, filter next f)
    | Node (_, next) -> filter next f
end

module Stack = Make(struct 
  let insert list item =
    Node (item, list)
end)

module Queue = Make(struct 
  let rec insert list item =
    match list with
    | Empty -> Node (item, Empty)
    | Node (n, next) -> Node (n, insert next item)
end)
