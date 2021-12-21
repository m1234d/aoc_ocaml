open Core;;

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

let filename = "input2.txt"
let r file = In_channel.read_lines filename

let f1 () =
  let data = r filename in

  let line_parse s =
    match (
      String.split_on_chars ~on: [' '] s
      |> List.filter ~f:(fun g -> g <> "")
    ) 
    with
    | a::b::[] -> (a, b)
    | _ -> ("", "")
  in

  let rec process_data l vert horiz = 
    match l with
    | [] -> vert*horiz
    | a :: x -> 
      let a1, a2 = line_parse (a) in
      let v2 = int_of_string a2 in
      match a1 with
      | "forward" -> process_data (x) (vert) (horiz+v2)
      | "down" -> process_data (x) (vert+v2) (horiz)
      | "up" -> process_data (x) (vert-v2) (horiz)
      | _ -> process_data (x) (vert) horiz

  in
  process_data data 0 0


let f2 () =
  let data = r filename in

  let line_parse s =
    match (
      String.split_on_chars ~on: [' '] s
      |> List.filter ~f:(fun g -> g <> "")
    ) 
    with
    | a::b::[] -> (a, b)
    | _ -> ("", "")
  in

  let rec process_data l vert horiz aim = 
    match l with
    | [] -> vert*horiz
    | a :: x -> 
      let a1, a2 = line_parse (a) in
      let v2 = int_of_string a2 in
      match a1 with
      | "forward" -> process_data x (vert + aim*v2) (horiz+v2) (aim)
      | "down" -> process_data x vert horiz (aim+v2)
      | "up" -> process_data x vert horiz (aim-v2)
      | _ -> -1

  in
  process_data data 0 0 0