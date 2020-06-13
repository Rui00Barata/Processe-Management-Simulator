(*Função de Reportagem*)
open Lib

let print_queue process_queue = 
  Queue.iter (fun x -> (Printf.printf "|\tPID:    %-16d Prioridade:   %-16d Arrival Time:   %d\n|\tPPID:   %-16d Burst Time:   %-16d Finish Time:    %d\n|\tNome:   %-16s Varíavel:     %-16d Time in CPU:    %d\n|\n" x.pid x.priority x.arrival_time x.ppid x.burst_time x.finish x.name x.variable x.time)) process_queue

let report () =
  begin
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tTempo atual: %d\n|\n|\tProcesso em execução:\n|\n" !time;
    if running_proc.ind >= 0 then
    (let x = List.nth !pcb_table running_proc.ind in (Printf.printf "|\tPID:    %-16d Prioridade:   %-16d Arrival Time:   %d\n|\tPPID:   %-16d Burst Time:   %-16d Finish Time:    %d\n|\tNome:   %-16s Varíavel:     %-16d Time in CPU:    %d\n|\n" x.pid x.priority x.arrival_time x.ppid x.burst_time x.finish x.name x.variable x.time))
    else Printf.printf "|\tNenhum processo em execução!\n|\n";
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tFila de processos prontos para executar:\n|\n";
    if Queue.is_empty readyQ then Printf.printf "|\tNenhum processo pronto para executar!\n|\n"
    else print_queue readyQ;
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tFila de processos bloqueados:\n|\n";
    if Queue.is_empty blockedQ then Printf.printf "|\tNenhum processo bloqueado!\n|\n"
    else print_queue blockedQ;
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tFila de processos terminados:\n|\n";
    if Queue.is_empty terminatedQ then Printf.printf "|\tNenhum processo terminado!\n|\n"
    else print_queue terminatedQ;
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n";
  end

let turnaround () =
  let sum = ref 0. in
  let n = ref 0. in
  (Queue.iter (fun x -> sum := !sum +. ((float_of_int x.finish) -. (float_of_int x.arrival_time)); n := !n +. 1.) terminatedQ; !sum /. !n)

let tme () =
  let sum = ref 0. in
  let n = ref 0. in
  (Queue.iter (fun x -> sum := !sum +. (((float_of_int x.finish) -. (float_of_int x.arrival_time)) -. (float_of_int x.burst_time)); n := !n +. 1.) terminatedQ; !sum /. !n)

let burst_time () =
  let sum = ref 0. in
  let n = ref 0. in
  (List.iter (fun (x : pcb) -> sum := !sum +. (float_of_int x.burst_time); n := !n +. 1.) !pcb_table; !sum /. !n)

let global_report () =
  begin
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tEstatísticas de Escalonamento\n|\n";
    Printf.printf "|\tTurnaround médio: %0.02f\n" (turnaround ());
    Printf.printf "|\tTempo médio de espera: %0.02f\n" (tme ());
    Printf.printf "|\tBurst Time médio: %0.02f\n|\n" (burst_time ());
    for i = 0 to 90 do print_char '-' done;
    Printf.printf "\n|\n|\tEstatísticas de Gestão de Memória\n|\n";
    Printf.printf "|\tFIRST-FIT: Número de fragmentos externos: %d\n" (Memory.fragment_count !heap_f);
    Printf.printf "|\tFIRST-FIT: Tempo médio de alocação: %.2f\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !time_list_f) in (!sum/.(float_of_int (List.length !time_list_f))));
    Printf.printf "|\tFIRST-FIT: Percentagem de erros de alocação: %.2f%%\n|\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !success_list_f) in ((!sum/.(float_of_int (List.length !success_list_f)))*.100.));
    Printf.printf "|\tNEXT-FIT:  Número de fragmentos externos: %d\n" (Memory.fragment_count !heap_n);
    Printf.printf "|\tNEXT-FIT:  Tempo médio de alocação: %.2f\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !time_list_n) in (!sum/.(float_of_int (List.length !time_list_n))));
    Printf.printf "|\tNEXT-FIT:  Percentagem de erros de alocação: %.2f%%\n|\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !success_list_n) in ((!sum/.(float_of_int (List.length !success_list_n)))*.100.));
    Printf.printf "|\tBEST-FIT:  Número de fragmentos externos: %d\n" (Memory.fragment_count !heap_b);
    Printf.printf "|\tBEST-FIT:  Tempo médio de alocação: %.2f\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !time_list_b) in (!sum/.(float_of_int (List.length !time_list_b))));
    Printf.printf "|\tBEST-FIT:  Percentagem de erros de alocação: %.2f%%\n|\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !success_list_b) in ((!sum/.(float_of_int (List.length !success_list_b)))*.100.));
    Printf.printf "|\tWORST-FIT: Número de fragmentos externos: %d\n" (Memory.fragment_count !heap_w);
    Printf.printf "|\tWORST-FIT: Tempo médio de alocação: %.2f\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !time_list_w) in (!sum/.(float_of_int (List.length !time_list_w))));
    Printf.printf "|\tWORST-FIT: Percentagem de erros de alocação: %.2f%%\n|\n" (let sum = ref 0. in let () = (List.iter (fun x -> sum := !sum +. (float_of_int x)) !success_list_w) in ((!sum/.(float_of_int (List.length !success_list_w)))*.100.));
    for i = 0 to 90 do print_char '-' done;
  end