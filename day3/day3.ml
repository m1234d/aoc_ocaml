open Core;;

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

let filename = "input3.txt"
let r file = In_channel.read_lines filename

let f1 () =
  let data = r filename in

  let line_parse s =
    let cl = String.to_list s in
    let rec loop sl =
      match sl with
      | [] -> []
      | a :: x -> if a = '0' then -1 :: loop (x) else 1 :: loop (x)
    in
    loop (cl)
  in

  let rec process_data l bit_counts = 
    match l with
    | [] -> bit_counts
    | a :: x -> 
      let bits = line_parse (a) in
      let new_counts = match bit_counts with
      | [] -> bits
      | _ -> List.map2_exn bits bit_counts ~f:(fun b1 b2 -> b1+b2)
      in
      process_data (x) new_counts
  in

  let counts = process_data data [] in

  let rec make_list s =
    match s with
    | [] -> []
    | a :: x -> if a > 0 then '1' :: make_list (x) else '0' :: make_list(x)
  in

  let clist = make_list (counts) in
  let alt = List.map (clist) ~f:(fun c -> if c = '1' then '0' else '1') in
  let str = String.of_char_list ('0' :: 'b' :: clist) in
  let altstr = String.of_char_list ('0' :: 'b' :: alt) in

  let gamma = int_of_string str in
  let epsilon = int_of_string altstr in

  gamma*epsilon

  
let f2 () =
  let data = r filename in

  let rec find_common l i count = 
    match l with
    | [] -> count
    | a :: x -> 
      let bit = a.[i] in
      if bit = '1' then find_common(x)(i)(count+1) else find_common(x)(i)(count-1)
  in

  let rec process_data l i inv =
    if List.length(l) = 1 then l else
    let most_common = find_common (l) (i) (0) in
    let filter_func = if inv then 
      (if most_common < 0 then (fun v -> v.[i] = '1') else (fun v -> v.[i] = '0'))
    else (if most_common >= 0 then (fun v -> v.[i] = '1') else (fun v -> v.[i] = '0')) in
    let new_data = List.filter ~f:(filter_func) (l) in
    process_data (new_data) (i+1) (inv)
    
  in
  let oxygen = match process_data (data) (0) (true) with
  | [] -> 0
  | fs :: _ ->
    int_of_string ("0b" ^ fs)
  in
  let co =  match process_data (data) (0) (false) with
  | [] -> 0
  | fs :: _ ->
    int_of_string ("0b" ^ fs)
  in

  oxygen*co

