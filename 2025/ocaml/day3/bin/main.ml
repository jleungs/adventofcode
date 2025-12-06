let read_file file =
    In_channel.with_open_text file In_channel.input_lines
;;

let rec find_largest str i len max =
    if i >= len then
        max
    else
        let cur = int_of_char str.[i] in
        if cur > (int_of_char str.[max]) then
            find_largest str (i+1) len i
        else
            find_largest str (i+1) len max
;;

let rec process_input_one lst =
    match lst with
    | [] -> 0
    | head :: tail ->
        let len = String.length head in
        let idx1 = find_largest head 0 (len-1) 0 in
        let idx2 = find_largest head (idx1+1) len (idx1+1) in
        let sum_str = (String.make 1 head.[idx1]) ^ (String.make 1 head.[idx2]) in
        let sum = int_of_string sum_str in
        sum + process_input_one tail
;;

let rec process_input_two lst =
    match lst with
    | [] -> 0
    | head :: tail ->
        let len = String.length head in
        let b = Buffer.create 12 in
        let j = ref 0 in
        for i = 0 to 11 do
            let r = 12 - i in
            let m = len - r + 1 in
            let idx = find_largest head !j m !j in
            Buffer.add_char b head.[idx];
            j := idx + 1
        done;
        (int_of_string (Buffer.contents b)) + process_input_two tail
;;

let () =
    let input = read_file "../../inputs/3.input" in
    let out1 = process_input_one input in
    Printf.printf "%d\n" out1;
    let out2 = process_input_two input in
    Printf.printf "%d\n" out2;
;;
