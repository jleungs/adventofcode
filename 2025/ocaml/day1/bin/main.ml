let modulus x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y
;;

let line_to_int (str : string) =
  match str.[0] with
  | 'R' ->
        int_of_string (String.sub str 1 (String.length str - 1))
  | 'L' ->
        - int_of_string (String.sub str 1 (String.length str - 1))
  | _ -> 0
;;

let rec move_dial (cur : int) (cnt : int) (lst : string list) =
  match lst with
  | [] -> cnt
  | head :: tail -> 
        let delta = line_to_int head in
        let new_cur = modulus (cur + delta) 100 in
        if new_cur = 0 then move_dial new_cur (cnt+1) tail
        else move_dial new_cur cnt tail
;;

let add_rotations (cur : int) (delta : int) =
  let c =
    if cur = 0 then
	  0
	else
	  match delta with
	  | d when d > 0 -> cur
	  | d when d < 0 -> 100 - cur
	  | _ -> 0
  in
  (c + abs delta) / 100
;;
	
let rec move_dial_two (cur : int) (cnt : int) (lst : string list) =
  match lst with
  | [] -> cnt 
  | head :: tail -> 
        let delta = line_to_int head in
        let new_cur = modulus (cur + delta) 100 in
        let new_cnt = cnt + (add_rotations cur delta) in
        move_dial_two new_cur new_cnt tail
;;

let () =
  let lines = In_channel.with_open_text "../../inputs/1.input" In_channel.input_lines in
  (*let lines = In_channel.with_open_text "1.input" In_channel.input_lines in*)
  let res1 = move_dial 50 0 lines in
  Printf.printf "Result: %d\n" res1;
  let res2 = move_dial_two 50 0 lines in
  Printf.printf "Result: %d\n" res2;
;;
