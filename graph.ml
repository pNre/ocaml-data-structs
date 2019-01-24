module type Representation = sig 
  type graph
  type vertex

  val init: unit -> graph
  val adj: vertex -> graph -> vertex list
  val vertices: graph -> vertex list 

  val add_vertex: graph -> graph
  val add_edge: vertex -> vertex -> graph -> graph

  val string_of_vertex: vertex -> string

  val distance: vertex -> graph -> int
  val set_distance: vertex -> int -> graph -> graph
  val visited: vertex -> graph -> bool
  val set_visited: vertex -> bool -> graph -> graph
  val predecessor: vertex -> graph -> vertex option
  val set_predecessor: vertex -> vertex option -> graph -> graph
end

module type Graph = sig
  include Representation
  val bfs: vertex -> (vertex -> unit) -> graph -> unit
  val dfs: (vertex -> unit) -> graph -> unit
end

module Make(R: Representation): Graph = struct
  type graph = R.graph
  type vertex = R.vertex

  let init = R.init
  let adj = R.adj
  let vertices = R.vertices

  let add_vertex = R.add_vertex
  let add_edge = R.add_edge

  let string_of_vertex = R.string_of_vertex

  let distance = R.distance
  let visited = R.visited
  let predecessor = R.predecessor
  let set_distance = R.set_distance
  let set_visited = R.set_visited
  let set_predecessor = R.set_predecessor

  let reset_attributes graph =
    graph
    |> vertices
    |> List.fold_left (fun graph x -> 
      graph
      |> set_visited x false
      |> set_distance x 0
      |> set_predecessor x None) graph

  let bfs vertex f graph =
    let rec bfs' vertexs f graph =
      match vertexs with
      | [] -> ()
      | x :: xs when visited x graph ->
        f x;
        let adjs = List.filter (fun n -> not (visited n graph)) (adj x graph) in
        let graph = List.fold_left (fun attrs n ->
          graph
          |> set_visited n true
          |> set_distance n ((distance x graph) + 1)
          |> set_predecessor n (Some x)) graph adjs in
        bfs' (List.append xs adjs) f graph
      | _ :: xs ->
        bfs' xs f graph
      in
    let graph = graph
      |> reset_attributes
      |> set_visited vertex true
      |> set_distance vertex 0
      |> set_predecessor vertex None in
    bfs' [vertex] f graph

  let dfs f graph =
    let rec dfs' vertex pred f graph =
      f vertex;
      let graph = set_visited vertex true graph in
      let adjs = List.filter (fun n -> not (visited n graph)) (adj vertex graph) in
      List.fold_left (visit f (Some vertex)) graph adjs
    and visit f pred graph n =
      if visited n graph then
        graph
      else
        graph
        |> set_predecessor n pred
        |> dfs' n pred f
    in
    let graph = reset_attributes graph in
    ignore @@ List.fold_left (visit f None) graph (vertices graph)
end

module Matrix = Make(struct 
  type vertex = int
  type graph =
    { attributes: (int * bool * int option) array
    ; adj: bool array array }

  let init () =
    { attributes = Array.make 10 (0, false, None)
    ; adj = Array.make_matrix 10 10 false }

  let adj vertex graph =
    graph.adj.(vertex)
    |> Array.to_list
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (_, x) -> x)
    |> List.map (fun (i, _) -> i)

  let add_vertex graph =
    graph

  let add_edge a b graph =
    graph.adj.(a).(b) <- true;
    graph.adj.(b).(a) <- true;
    graph

  let vertices graph =
    List.init (Array.length graph.adj) (fun x -> x)

  let string_of_vertex vertex =
    Printf.sprintf "(%d)" vertex

  let distance vertex graph = 
    let (d, _, _) = graph.attributes.(vertex) in
    d
  
  let visited vertex graph = 
    let (_, c, _) = graph.attributes.(vertex) in
    c
  
  let predecessor vertex graph =
    let (_, _, p) = graph.attributes.(vertex) in
    p

  let set_distance vertex dist graph =
    let (_, c, p) = graph.attributes.(vertex) in
    graph.attributes.(vertex) <- (dist, c, p);
    graph

  let set_visited vertex visited graph =
    let (d, _, p) = graph.attributes.(vertex) in
    graph.attributes.(vertex) <- (d, visited, p);
    graph
  
  let set_predecessor vertex pred graph =
    let (d, c, _) = graph.attributes.(vertex) in
    graph.attributes.(vertex) <- (d, c, pred);
    graph
end)

module Lst = Make(struct
  type vertex = int
  type attribs = { distance: int; visited: bool; predecessor: vertex option }
  type graph = ((vertex list) * attribs) array

  let init_vertex  =
    ([], { distance = 0; visited = false; predecessor = None })

  let init () =
    Array.make 0 init_vertex

  let adj vertex graph =
    let (adj, _) = graph.(vertex) in
    adj

  let add_vertex graph =
    [init_vertex]
    |> Array.of_list
    |> Array.append graph

  let add_edge a b graph =
    let (a_adj, a_attribs) = graph.(a) in
    let (b_adj, b_attribs) = graph.(b) in
    graph.(a) <- (b :: a_adj, a_attribs);
    graph.(b) <- (a :: b_adj, b_attribs);
    graph

  let vertices graph =
    graph
    |> Array.to_list
    |> List.mapi (fun i _ -> i)

  let string_of_vertex vertex =
    Printf.sprintf "(%d)" vertex

  let distance vertex graph = 
    let (_, attribs) = graph.(vertex) in
    attribs.distance
  
  let visited vertex graph = 
    let (_, attribs) = graph.(vertex) in
    attribs.visited
  
  let predecessor vertex graph =
    let (_, attribs) = graph.(vertex) in
    attribs.predecessor

  let set_distance vertex dist graph =
    let (adj, attribs) = graph.(vertex) in
    graph.(vertex) <- (adj, {attribs with distance = dist});
    graph

  let set_visited vertex visited graph =
    let (adj, attribs) = graph.(vertex) in
    graph.(vertex) <- (adj, {attribs with visited = visited});
    graph
  
  let set_predecessor vertex pred graph =
    let (adj, attribs) = graph.(vertex) in
    graph.(vertex) <- (adj, {attribs with predecessor = pred});
    graph
end)
