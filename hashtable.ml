module type Hashtable = sig
  type k
  type v
  val hash: k -> int
  val compare: k -> k -> bool
end

module Make(H: Hashtable) = struct
  type entry =
    | Empty
    | Item of H.k * H.v * entry

  type t = {
    table: entry array;
    size: int;
  }

  let init size =
    { table = Array.init size (fun _ -> Empty)
    ; size = size }

  let rec insert table key value =
    match table with
    | Empty -> Item (key, value, Empty)
    | Item (k, v, next) -> Item (k, v, insert next key value)

  let rec find table key =
    match table with
    | Empty -> None
    | Item (k, v, _) when H.compare k key -> Some v
    | Item (_, _, next) -> find next key

  let insert ht key value =
    let bucket = H.hash key mod ht.size in
    ht.table.(bucket) <- insert ht.table.(bucket) key value

  let find ht key =
    let bucket = H.hash key mod ht.size in
    find ht.table.(bucket) key
end

module String_to_int_hashtable = Make(struct 
  type k = string
  type v = int

  let compare = ( = )

  (* adler32 *)
  let hash s = 
    let (s0, s1) = s
      |> String.to_seq
      |> Seq.fold_left (fun (s0, s1) char -> 
        let s0 = (s0 + Char.code char) mod 65521 in
        let s1 = (s1 + s0) mod 65521 in
        (s0, s1)
      ) (1, 0) in
    (s1 lsl 16) lor s0
end)
