module type Trie = sig
  type t
  val to_string: t -> string
end

module Make(T: Trie) = struct
  type t =
    | Empty
    | Node of T.t option * t array

  let array_size = Char.code 'z' - Char.code '0' + 1

  let init = 
    Empty
  
  let node value = 
    Node (value, (Array.init array_size (fun _ -> Empty)))

  let index_of_char c =
    Char.code c - Char.code '0'

  let char_of_index i =
    Char.chr (Char.code '0' + i)

  let chars_list_of_string s =
    (List.init (String.length s) (String.get s))

  let insert trie key value = 
    let rec insert' trie chars =
      match trie with
      | Empty -> 
        insert' (node None) chars
      | Node (_, children) ->
        match chars with
        | [] -> node (Some value)
        | char :: rest ->
          children.(index_of_char char) <- (insert' (children.(index_of_char char)) rest);
          trie
    in
    insert' trie (chars_list_of_string key)

  let find trie key =
    let rec find' trie chars =
      match trie with
      | Empty -> None
      | Node (value, children) ->
        match chars with
        | [] -> value
        | char :: rest ->
          find' children.(index_of_char char) rest
    in
    find' trie (chars_list_of_string key)

  let dump trie =
    let rec print' trie prefix =
      match trie with
      | Empty -> ()
      | Node (value, children) ->
        (match value with
        | None -> ()
        | Some v -> Printf.printf "%s = %s\n" prefix (T.to_string v));
        Array.iteri (fun i child -> print' child (prefix ^ (String.make 1 (char_of_index i)))) children
    in
    print' trie ""
end

module Int_trie = Make(struct 
  type t = int 
  let to_string = string_of_int
end)
