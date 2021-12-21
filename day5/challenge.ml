open Core;;

let (=) = Poly.(=);;
let (<>) = Poly.(<>);;

module Hashtbl = Stdlib.Hashtbl;;

let filename = "input.txt"
let r file = In_channel.read_lines filename

let parse_line l =
  match (String.split_on_chars l ~on:[','; ' '; '-'; '>']
  |> List.filter ~f:(fun g -> g <> "")) with
  | a1::a2::a3::a4::[] -> int_of_string a1, int_of_string a2, int_of_string a3, int_of_string a4
  | _ -> (-1, -1, -1, -1)
  

let f1 () = 
  let data = r filename in
  let table = Hashtbl.create 4096 in

  let rec process_data da = 
    match da with
    | [] -> ()
    | l::rest ->
      let x1,y1,x2,y2 = parse_line l in
      
      let _ = if x1 = x2 then
        let new_y1 = if y1 < y2 then y1 else y2 in
        let new_y2 = if y1 < y2 then y2 else y1 in
        let y1 = new_y1 in
        let y2 = new_y2 in
        (for i = y1 to y2 do
          if Hashtbl.mem (table) (x1, i) then
            Hashtbl.replace (table) (x1, i) (1 + Hashtbl.find (table) (x1, i))
          else
            Hashtbl.add (table) (x1, i) (1)
        done;)
      else if y1 = y2 then
        let new_x1 = if x1 < x2 then x1 else x2 in
        let new_x2 = if x1 < x2 then x2 else x1 in
        let x1 = new_x1 in
        let x2 = new_x2 in
        (for i = x1 to x2 do
          if Hashtbl.mem (table) (i, y1) then
            Hashtbl.replace (table) (i, y1) (1 + Hashtbl.find (table) (i, y1))
          else
            Hashtbl.add (table) (i, y1) (1)
        done;)
      else 
        let invx = if x2 > x1 then 1 else -1 in
        let invy = if y2 > y1 then 1 else -1 in
        let count = ref 0 in
        (for ii = invx*x1 to invx*x2 do
          let i = invx*ii in
          let j = invy*(invy*y1 + !count) in
          count := !count + 1;
          if Hashtbl.mem (table) (i, j) then
            Hashtbl.replace (table) (i, j) (1 + Hashtbl.find (table) (i, j))
          else
            Hashtbl.add (table) (i, j) (1)
        done;)
      in
      process_data (rest)

    in
    let _ = process_data (data) in
    Hashtbl.fold (fun (k0,k1) d1 acc -> if d1 > 1 then (acc+1) else acc) (table) (0)