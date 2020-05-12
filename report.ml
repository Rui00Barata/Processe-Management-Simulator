(*Função de Reportagem*)
open Lib

let print_queue process_queue = 
  Queue.iter (fun x -> Printf.printf "ppid = %d, pid = %d, prioridade = %d, 
  valor da variável = %d, tempo de início = %d, tempo de fim = %d, tempo usado do CPU = %d\n\n" x.ppid x.pid x.priority x.variable x.arrival_time x.finish x.time) process_queue

  let report () =
  begin
    Printf.printf "\nTEMPO ATUAL: %d\n\nPROCESSO EM EXECUÇÃO:\n" !time;
    if running_proc.ind > 0 then
    (let x = List.nth !pcb_table running_proc.ind in Printf.printf "ppid = %d, pid = %d, prioridade = %d, 
    valor da variável = %d, tempo de início = %d, tempo usado do CPU = %d\n\n" x.ppid x.pid x.priority x.variable x.arrival_time x.time)
    else Printf.printf "Nenhum processo em execução!\n\n";
    Printf.printf "PROCESSOS BLOQUEADOS:\n";
    if Queue.is_empty blockedQ then Printf.printf "Nenhum processo bloqueado!\n\n"
    else print_queue blockedQ;
    Printf.printf "PROCESSOS PRONTOS A EXECUTAR:\n";
    if Queue.is_empty readyQ then Printf.printf "Nenhum processo pronto a executar!\n\n"
    else  print_queue readyQ;
    Printf.printf "PROCESSOS TERMINADOS:\n";
    if Queue.is_empty terminatedQ then Printf.printf "Nenhum processo terminado!\n\n"
    else print_queue terminatedQ
  end

let turnaround () =
  let sum = ref 0. in
  let n = ref 0. in
  (Queue.iter (fun x -> sum := !sum +. ((float_of_int x.finish) -. (float_of_int x.arrival_time)); n := !n +. 1.) terminatedQ; !sum /. !n)

let tme () =
  let sum = ref 0. in
  let n = ref 0. in
  (Queue.iter (fun x -> sum := !sum +. (((float_of_int x.finish) -. (float_of_int x.arrival_time)) -. (float_of_int x.time)); n := !n +. 1.) terminatedQ; !sum /. !n)

let burst_time () =
  let sum = ref 0. in
  let n = ref 0. in
  (List.iter (fun (x : pcb) -> sum := !sum +. (float_of_int x.time); n := !n +. 1.) !pcb_table; !sum /. !n)

let global_report () =
  Printf.printf "Turnaround médio: %0.02f\n" (turnaround ()) ;
  Printf.printf "Tempo médio de espera: %0.02f\n" (tme ());
  Printf.printf "Burst Time médio: %0.02f\n\n" (burst_time ())


