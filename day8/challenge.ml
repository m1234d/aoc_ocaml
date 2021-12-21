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

let read_data data =
  List.hd_exn data 
  |> String.split_on_chars ~on:[',']
  |> List.filter ~f:(fun g -> g <> "")
  |> List.map ~f:(fun g -> int_of_string(g))

let f1 () = 
  let data = r filename in
  let table = Hashtbl.create 4096 in

  let crabs = read_data data in
  let max_val = List.fold (crabs) ~init:0 ~f:(fun a b -> if a > b then a else b) in

  let fuel_costs = List.init max_val (fun i -> List.fold (crabs) ~init:0 ~f:(fun a b -> 
    let n = abs(b - i)
    in a + n*(n+1)/2
    )) in
    
  let fuel_costs = List.filter (fuel_costs) ~f:(fun a -> a >= 0) in
  List.fold (fuel_costs) ~init:(-1) ~f:(fun a b -> if a = -1 || b < a then b else a)
