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

let scheduling_menu () =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Escalonamento\n\n1 - FCFS\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> begin 
      controller false;
      flag := false
    end
    | 0 -> exit 0
    | _ -> ()
done

let options_menu () =
  let flag = ref true in
  while !flag do
    Printf.printf "Menu de Opções\n\n1 - Escalonamento Não Preemptivo\n2 - Escalonamento Preemptivo\n\n0 - Sair\n";
    let op = read_int () in
    match op with
    | 1 -> begin 
      scheduling_menu ();
      flag := false
    end
    | 2 -> ()
    | 0 -> exit 0
    | _ -> ()
  done

let menu op =
  let flag = ref true in
  let () = read_plan () in
  while !flag do
    Printf.printf "Menu\n\n1 - Iniciar\n2 - Opções\n\n0 - Sair\n";
    match op with
    | 1 -> controller true      
    | 2 -> ()
    | 0 -> exit 0 
    | _ -> ()
  done