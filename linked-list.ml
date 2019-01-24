module LinkedList = struct
  type 'a t =
    | Empty
    | Node of 'a * 'a t

  let rec append list item =
    match list with
    | Empty -> Node (item, Empty)
    | Node (n, Empty) -> Node (n, Node (item, Empty))
    | Node (n, next) -> Node (n, append next item)

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

  let rec delete list item =
    filter list (fun x -> x != item)
end
