open Core;;

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

let filename = "input.txt"
let r file = In_channel.read_lines filename


let f1 () =
  let data = r filename in

  let parse_rolls s =
    (
      String.split_on_chars ~on: [','] s
      |> List.filter ~f:(fun g -> g <> "")
      |> List.map ~f:(fun g -> int_of_string g)
    ) 
  in

  let parse_row s =
    String.split_on_chars ~on: [' '] s
      |> List.filter ~f:(fun g -> g <> "")
      |> List.map ~f:(fun g -> int_of_string g)
      |> List.to_array
  in

  let rec parse_boards s =
    match s with
    | [] -> []
    | l1::l2::l3::l4::l5::_::rest -> [|parse_row(l1); parse_row(l2); parse_row(l3); parse_row(l4); parse_row(l5)|] :: parse_boards (rest)
    | _ -> []
  in

  let rec sum_board b =
    let row_sums = Array.map (b) ~f:(fun r -> Array.fold(r) ~init:0 ~f:(fun e g -> e+g)) in
    Array.fold (row_sums) ~init:0 ~f:(fun e g -> e+g)
  in

  let rec mark_boards bs r =
    match bs with
    | [] -> None
    | b::rest ->
    for i = 0 to 4 do
      for j = 0 to 4 do
        if b.(i).(j) = r then (b.(i).(j) <- -1) else ();
      done;
    done;
    let cols_solved = 
      let folded_arr = Array.fold (b) ~init:[|true; true; true; true; true;|] 
      ~f:(
        fun a c -> 
          let mapped_rows = Array.map (c) ~f:(fun cc -> if cc = -1 then true else false) in
          Array.map2_exn (a) (mapped_rows) ~f:(fun g1 g2 -> g1 && g2)
      )
        in
      Array.fold (folded_arr) ~init:false ~f:(fun a g -> a || g)
  in
    let rows_solved = Array.fold (b) ~init:false 
    ~f:(
      fun a c -> Array.fold (c) ~init:true ~f:(fun aa cc -> if cc = -1 then aa else false) || a
    ) in
    if cols_solved || rows_solved then Some (b) else mark_boards (rest) (r)

  in
  match data with
  | first_line::_::last_lines ->
    let rolls = parse_rolls (first_line) in
    let boards = parse_boards (last_lines) in
    let rec loop rs = 
      match rs with
        | [] -> (0, [|[||]|])
        | r :: rr -> 
          let found_board = mark_boards (boards) (r) in
          match found_board with
          | None -> 
            loop (rr) 
          | Some b -> (r, b)
    in
    let num, winner = loop (rolls) in
    let filtered_board = Array.map (winner) ~f:(fun r -> Array.filter (r) ~f:(fun e -> e <> -1)) in
    let s = sum_board (filtered_board) in
    s*num
  
  
  | _ -> -1


let f2 () =
  let data = r filename in

  let parse_rolls s =
    (
      String.split_on_chars ~on: [','] s
      |> List.filter ~f:(fun g -> g <> "")
      |> List.map ~f:(fun g -> int_of_string g)
    ) 
  in

  let parse_row s =
    String.split_on_chars ~on: [' '] s
      |> List.filter ~f:(fun g -> g <> "")
      |> List.map ~f:(fun g -> int_of_string g)
      |> List.to_array
  in

  let rec parse_boards s =
    match s with
    | [] -> []
    | l1::l2::l3::l4::l5::_::rest -> [|parse_row(l1); parse_row(l2); parse_row(l3); parse_row(l4); parse_row(l5)|] :: parse_boards (rest)
    | _ -> []
  in

  let rec sum_board b =
    let row_sums = Array.map (b) ~f:(fun r -> Array.fold(r) ~init:0 ~f:(fun e g -> e+g)) in
    Array.fold (row_sums) ~init:0 ~f:(fun e g -> e+g)
  in

  let rec mark_boards bs r =
    match bs with
    | [] -> None
    | b::rest ->
    for i = 0 to 4 do
      for j = 0 to 4 do
        if b.(i).(j) = r then (b.(i).(j) <- -1) else ();
      done;
    done;
    let cols_solved = 
      let folded_arr = Array.fold (b) ~init:[|true; true; true; true; true;|] 
      ~f:(
        fun a c -> 
          let mapped_rows = Array.map (c) ~f:(fun cc -> if cc = -1 then true else false) in
          Array.map2_exn (a) (mapped_rows) ~f:(fun g1 g2 -> g1 && g2)
      )
        in
      Array.fold (folded_arr) ~init:false ~f:(fun a g -> a || g)
  in
    let rows_solved = Array.fold (b) ~init:false 
    ~f:(
      fun a c -> Array.fold (c) ~init:true ~f:(fun aa cc -> if cc = -1 then aa else false) || a
    ) in
    if cols_solved || rows_solved then Some (b) else mark_boards (rest) (r)

  in
  match data with
  | first_line::_::last_lines ->
    let rolls = parse_rolls (first_line) in
    let boards = parse_boards (last_lines) in
    let rec loop rs bs = 
      match rs with
        | [] -> (0, [|[||]|], [])
        | r :: rr -> 
          let found_board = mark_boards (bs) (r) in
          match found_board with
          | None -> 
            loop (rr) (bs)
          | Some b -> 
            let new_boards = List.filter (bs) ~f:(fun v -> v <> b) in
            (r, b, new_boards)
    in
    let rec find_last rs bb =
      let n, w, new_boards = loop (rolls) (bb) in
      if List.length(new_boards) = 0 then (n, w) else
        find_last (rs) (new_boards)
      in
    let num, winner = find_last (rolls) (boards) in
    let filtered_board = Array.map (winner) ~f:(fun r -> Array.filter (r) ~f:(fun e -> e <> -1)) in
    let s = sum_board (filtered_board) in
    s*num
  
  
  | _ -> -1