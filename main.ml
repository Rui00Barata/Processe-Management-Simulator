open Lib

(*let () = Sim.openfile "prog"
let aux = (List.nth !pcb_table 1)
let () = Printf.printf "variable = %d\n" aux.variable
let () = Sim.read_instr (List.nth !pcb_table 1)
let aux = (List.nth !pcb_table 1)
let () = Printf.printf "variable = %d\n" aux.variable*)

(*CLOCK*)
(*while true do
  short
  instr
  tempo++;
done*)


(* match (List.nth (!pcb_table) ind).status with
    | 1 -> Queue.push temp readyQ
    | _ -> () *)

let clock_flag = ref true

let executing_flag = ref false

let rem_time = ref 0

let clock = 
  while( !clock_flag) do
    controller

  done;

let () = 
  begin

    menu
    
  end