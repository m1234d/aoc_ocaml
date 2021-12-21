open Core;;

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

module Hashtbl = Stdlib.Hashtbl;;

let filename = "input.txt"
let r file = In_channel.read_lines filename

let parse_line l =
  (String.split_on_chars l ~on:[',']
  |> List.filter ~f:(fun g -> g <> ""))
  |> List.map ~f:(fun g -> int_of_string(g))
  
let remove_dups l =
  let table = Hashtbl.create 4096 in
  let rec helper ls = 
    match ls with
    | [] -> []
    | a::rest -> if Hashtbl.mem(table)(a) then helper(rest) else
      (Hashtbl.add (table) (a) (1); a::helper(rest))
  in
  helper (l)

let f1 () = 
  let data = r filename in
  let table = Hashtbl.create 4096 in

  let rec process_data da = 
    match da with
    | [] -> []
    | l::rest ->
      let fishes = parse_line l in
      let _ = List.map (fishes) ~f:(fun g ->
        if Hashtbl.mem (table) (g) then Hashtbl.add (table) (g) (1 + Hashtbl.find (table) (g))
        else Hashtbl.add (table) (g) (1))
      in
      (* let new_fishes = remove_dups(fishes) in *)
      let final_fishes = ref fishes in
      let rec loop fs new_fishes =
        match fs with
        | [] -> new_fishes
        | f :: rest ->
          if f = 0 then
            let new_fish = 8 in
            let old_fish = 6 in
            
            loop(rest) (old_fish::new_fish::new_fishes)
          else
            loop(rest) ((f-1)::new_fishes)
      in
      for i = 0 to 79 do
        final_fishes := loop (!final_fishes) ([])
      done;
      !final_fishes
    in
    List.length (process_data (data))