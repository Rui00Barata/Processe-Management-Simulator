(*Função de Escalonamento a Curto Prazo*)
open Lib 

let rec findProcInd (queue : pcb list) pid count =
  match queue with
  | head::body -> if (head.pid = pid) then count else findProcInd (body) pid (count+1)
  | [] -> -1

let selected_scheduller = ref 1

(*  match (List.nth (!pcb_table) ind).status with
    | 1 -> Queue.push temp readyQ
    | _ -> () *)

(* 1 -> fcfs *)

let fcfs () =
  if running_proc.ind = -1 then
  let temp = Queue.pop readyQ in
  let ind = (findProcInd !pcb_table temp.pid 0) in
  if (ind < 0) then print_endline "ERRO! O Processo nao existe na tabela" 
  else
    begin
      (* Printf.printf "FCFS!!!"; *)
      running_proc.ind <- ind;
      running_proc.pid <- temp.pid;
      running_proc.pc <- temp.pc;
      temp.status <- 1;
    end

let short_sched () =
  begin
    (match !selected_scheduller with
    |1 -> fcfs ()
    |_ -> fcfs ());
    Printf.printf "ihateyou";
    executing_flag := true;
    rem_time := !time_quantum;
    Printf.printf "ihateyou2";
  end