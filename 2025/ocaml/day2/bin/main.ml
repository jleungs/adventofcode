let read_file file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char ',' contents
;;

let split_range str =
    let split = String.split_on_char '-' str in
    let s = int_of_string (List.nth split 0) in
    let e = int_of_string (List.nth split 1) in
    (s, e)
;;

let num_digits n =
    String.length (string_of_int n)
;;

let rec pow x n =
    if n = 0 then 1
    else x * pow x (n-1)
;;

let rec process_range (cur : int) (last : int) =
    if cur > last then 0
    else
        let h = pow 10 ((num_digits cur)/2) in
        if (cur / h) = (cur mod h) then
            cur + process_range (cur+1) last
        else 
            process_range (cur+1) last
;;

let rec process_input (lst : string list) =
    match lst with
    | [] -> 0
    | head :: tail ->
        let h = String.trim head in
        if h = "" then
            process_input tail
        else
            let (start_range, end_range) = split_range h in
            let sum = process_range start_range end_range in
            sum + process_input tail
;;

let rec repeat (s : string) (n : int) =
    match n with
    | 0 -> s
    | _ -> s ^ repeat s (n-1)
;;

let rec process_number_two (num : string) (n : int) =
    if n = String.length num then false
    else
        let numsub = String.sub num 0 n in
        let t = (String.length num) / n - 1 in
        if (repeat numsub t) = num then true
        else process_number_two num (n+1)
 ;;   

let rec process_range_two (cur : int) (last : int) =
    if cur > last then 0
    else
        let scur = string_of_int cur in
        if process_number_two scur 1 then
            cur + process_range_two (cur+1) last
        else
            process_range_two (cur+1) last
;;
                  
let rec process_input_two (lst : string list) =
    match lst with
    | [] -> 0
    | head :: tail ->
        let h = String.trim head in
        if h = "" then
            process_input_two tail
        else
            let (start_range, end_range) = split_range h in
            let sum = process_range_two start_range end_range in
            sum + process_input_two tail
;;

let () =
    let input = read_file "../../inputs/2.input" in
    (*let input = read_file "2.input" in*)
    let out1 = process_input input in
    Printf.printf "%d\n" out1;
    let out2 = process_input_two input in
    Printf.printf "%d\n" out2;
;;
