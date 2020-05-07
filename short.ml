(*Função de Escalonamento a Curto Prazo*)
open Lib 

let rec findProcInd queue pid count=
  match queue with
  | head::body -> if (head.pid = pid) then count else findProcInd (body) pid (count+1)
  | [] -> -1


let select_scheduller = ref 1

(* 1 -> fcfs *)

let short_sched =
  begin
    match !select_scheduller with
    | 1 -> fcfs
    | _ -> fcfs
    Main.executing_flag := true;
    Main.rem_time := time_quantum
  end

let fcfs =
  let temp = Queue.pop readyQ in
  let ind = (findProcInd !pcb_table temp.pid 0) in
  if (ind < 0) then print_endline "ERRO! O Processo nao existe na tabela" 
  else
  begin
    running_proc := {ind = ind; pid = p.pid; pc = p.pc};
    temp.status <- 1;
   
  end 
