open Lib

(* Deallocate *)
let deallocate_mem pid =
  let flag = ref false in
  let () = 
    for i = 0 to (Array.length !heap - 1) do
      if (!heap.(i) = pid) then begin !heap.(i) <- (-1); flag := true end
    done in
  if !flag then 1 else (-1)

let fragment_count () =
  let hole_size = ref 0 in
  let n_holes = ref 0 in
  let () =
    for i = 0 to (Array.length !heap - 1) do
        if (!heap.(i) = -1) then hole_size := !hole_size + 1
        else 
          (if (!hole_size = 1 || !hole_size = 2) then n_holes := !n_holes + 1;
          hole_size := 0)
    done in !n_holes

let rec has_memory_available i n =
  if n = 0 then true
  else if (i + n > Array.length !heap) then false
  else
    if (!heap.(i) = -1) then has_memory_available (i + 1) (n - 1)
    else false

(* First-fit *)
  let first_allocate n pid =
    let flag = ref true in
    let i = ref 0 in
    let () = while !flag do
      if (has_memory_available (!i) (n)) then  (* enc *)
        begin
        for j = !i to (n - 1 + !i) do
          !heap.(j) <- pid
        done;
        flag := false;
        i := !i + 1
        end       
      else 
        if (!i = Array.length !heap) then begin flag := false; i := (-1) end
        else
          i := !i + 1
    done in !i

(* Next-fit *)
let next_fit_index = ref 0

let next_allocate n pid =
  let flag = ref true in
  let i = ref 0 in
  let () = while !flag do
    if (has_memory_available (!next_fit_index) (n)) then  (* enc *)
      begin
      for j = !next_fit_index to (n - 1 + !next_fit_index) do
        !heap.(j) <- pid;
        next_fit_index := !next_fit_index + 1
      done;
      flag := false;
      i := !i + 1
      end       
    else 
      if (!next_fit_index = Array.length !heap) then begin flag := false; i := (-1) end
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
    for i = 0 to (Array.length !heap) - 1 do
      if i = (Array.length !heap - 1) && !min >= !count && !count >= n then ind := !aux + 1; 
      if !heap.(i) = -1 then count := !count + 1
      else if !count >= n && !min > !count then (min := !count; ind := i - !count; count := 0; aux := i)
      else (count := 0; aux := i);
    done;
    if !ind <> (-1) then
      for i = !ind to !ind + n - 1 do
        !heap.(i) <- pid
      done
    else () in !ind

(* Worst Fit *)
let worst_allocate n pid = 
  let countIndex = ref (-1) in
  let count = ref 0 in
  let max = ref 0 in 
  let maxIndex = ref (-1) in
  begin 
    Printf.printf "f1\n";
    for i=0 to ((Array.length !heap) - 1) do
      if(!heap.(i) <> -1)
      then ((if !count > !max then (max := !count; maxIndex := !countIndex;));count := 0;countIndex := -1)
      else ((if !countIndex = -1 then countIndex := i); count := !count +1;)
    done;
    if !count > !max then (max := !count; maxIndex := !countIndex;);
    if (has_memory_available (!maxIndex) (n))
    then 
      for i = !maxIndex to (!maxIndex + n - 1) do
        !heap.(i) <- pid
      done;
  end

let allocate_mem op n pid =
  match op with
  |1 -> first_allocate n pid
  |_ ->first_allocate n pid