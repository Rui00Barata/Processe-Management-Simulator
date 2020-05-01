open Lib

let () = Sim.abrir "prog"
let aux = (List.nth !pcb_table 1)
let () = Printf.printf "Variavel = %d\n" aux.variavel 
let () = Sim.read_instr (List.nth !pcb_table 1)
let aux = (List.nth !pcb_table 1)
let () = Printf.printf "Variavel = %d\n" aux.variavel 

(*let menu op =
  let flag = ref true in
  while !flag do
    let () = printf "Menu\n1 - Escalonamento Preemptivo"
    match op with
    |
  done;*)
