open Lib

(* Deallocate *)
let deallocate_mem pid =
  let flag = ref false in
  let () = 
    for i = 0 to (Array.length !heap_f - 1) do
      if (!heap_f.(i) = pid) then begin !heap_f.(i) <- (-1); flag := true end;
      if (!heap_n.(i) = pid) then begin !heap_n.(i) <- (-1); flag := true end;
      if (!heap_b.(i) = pid) then begin !heap_b.(i) <- (-1); flag := true end;
      if (!heap_w.(i) = pid) then begin !heap_w.(i) <- (-1); flag := true end
    done in
  if !flag then 1 else (-1)

let fragment_count heap =
  let hole_size = ref 0 in
  let n_holes = ref 0 in
  let () =
    for i = 0 to (Array.length heap - 1) do
        if (heap.(i) = -1) then hole_size := !hole_size + 1
        else 
          (if (!hole_size = 1 || !hole_size = 2) then n_holes := !n_holes + 1;
          hole_size := 0)
    done in !n_holes

let rec has_memory_available i n heap =
  if n = 0 then true
  else if (i + n > Array.length heap) then false
  else
    if (heap.(i) = -1) then has_memory_available (i + 1) (n - 1) heap
    else false

(* First-fit *)
  let first_allocate n pid =
    let flag = ref true in
    let i = ref 0 in
    let () = while !flag do
      if (has_memory_available (!i) (n) (!heap_f)) then  (* enc *)
        begin
        for j = !i to (n - 1 + !i) do
          !heap_f.(j) <- pid
        done;
        flag := false;
        i := !i + 1
        end       
      else 
        if (!i = Array.length !heap_f) then begin flag := false; i := (-1) end
        else
          i := !i + 1
    done in !i

(* Next-fit *)
let next_fit_index = ref 0

let next_allocate n pid =
  let flag = ref true in
  let i = ref 0 in
  let () = while !flag do
    if (has_memory_available (!next_fit_index) (n) (!heap_n)) then  (* enc *)
      begin
      for j = !next_fit_index to (n - 1 + !next_fit_index) do
        !heap_n.(j) <- pid;
        next_fit_index := !next_fit_index + 1
      done;
      flag := false;
      i := !i + 1
      end       
    else 
      if (!next_fit_index = Array.length !heap_n) then begin flag := false; i := (-1) end
      else
        (i := !i + 1;
        next_fit_index := !next_fit_index + 1)
  done in !i
  
(* Best-Fit *)
let best_allocate n pid =
  let aux = ref 0 in
  let ind = ref (-1) in
  let min = ref (max_int) in
  let count = ref 0 in
  let () = 
    for i = 0 to (Array.length !heap_b) - 1 do
      (if !heap_b.(i) = -1 then (count := !count + 1)
      else (if !count >= n && !min > !count then (min := !count; ind := i - !count; count := 0; aux := i)
      else (count := 0; aux := i)));
      (if i = (Array.length !heap_b - 1) && !min >= !count && !count >= n then (ind := i - !count + 1; min := !count));
    done;
    if !ind <> (-1) then
      for i = !ind to !ind + n - 1 do
        !heap_b.(i) <- pid
      done
    else () in if !ind = (-1) then !ind else (Array.length !heap_b)

  (* Worst Fit *)
let worst_allocate n pid = 
  let countIndex = ref (-1) in
  let count = ref 0 in
  let max = ref 0 in 
  let maxIndex = ref (-1) in
  let () = begin
    for i=0 to ((Array.length !heap_w) - 1) do
      if(!heap_w.(i) <> -1)
      then ((if !count > !max then (max := !count; maxIndex := !countIndex;));count := 0;countIndex := -1)
      else ((if !countIndex = -1 then countIndex := i); count := !count +1;)
    done;
    if !count > !max then (max := !count; maxIndex := !countIndex;);
    if (has_memory_available (!maxIndex) (n) (!heap_w))
    then 
      for i = !maxIndex to (!maxIndex + n - 1) do
        !heap_w.(i) <- pid
      done;
  end in 128

let n_generator () =
  if (Queue.length readyQ < 10) then 2
  else let () = Random.init 42 in ((Random.int 7) + 3)

let solicitate_allocation pid =
  let () = Printf.printf "OLA\n" in
  let n = n_generator () in
  let aux = ref 0 in
  begin
    aux := first_allocate n pid;
    if !aux = -1 then success_list_f := !success_list_f@[1]
    else (time_list_f := !time_list_f@[!aux]; success_list_f := !success_list_f@[0]);
    aux := next_allocate n pid;
    if !aux = -1 then success_list_n := !success_list_n@[1]
    else (time_list_n := !time_list_n@[!aux]; success_list_n := !success_list_n@[0]);
    aux := best_allocate n pid;
    if !aux = -1 then success_list_b := !success_list_b@[1]
    else (time_list_b := !time_list_b@[!aux]; success_list_b := !success_list_b@[0]);
    aux := worst_allocate n pid;
    if !aux = -1 then success_list_w := !success_list_w@[1]
    else (time_list_w := !time_list_w@[!aux]; success_list_w := !success_list_w@[0]);
  end
