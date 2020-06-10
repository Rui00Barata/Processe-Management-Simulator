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

(*  *)

let allocate_mem op n pid =
  match op with
  |1 -> first_allocate n pid
  |_ ->first_allocate n pid