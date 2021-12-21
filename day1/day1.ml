open Core;;

let filename = "input1.txt"
let r file = In_channel.read_lines filename

let f () =
  let data = r filename in
  let rec process_data l v1 v2 v3 = 
    match l with
    | [] -> 0
    | a :: x -> 
      let v = int_of_string (a) in
      match (v1, v2, v3) with
      | _, _, None -> process_data (x) (Some (v)) (v1) (v2)
      | Some vv1, Some vv2, Some vv3 ->
        let s = vv1 + vv2 + vv3 in
        let new_s = v + vv1 + vv2 in
        if new_s > s then 1 + process_data (x) (Some (v)) (v1) (v2)
        else process_data (x) (Some (v)) v1 v2
      | _, _, _-> -1
  in
  process_data data None None None