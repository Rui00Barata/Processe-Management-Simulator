(*Função de Reportagem*)
open Lib

let print_queue process_queue = 
  Queue.iter (fun x -> Printf.printf "ppid = %d, pid = %d, prioridade = %d, 
  valor da variável = %d, tempo de início = %d, tempo usado do CPU = %d\n\n" x.ppid x.pid x.priority x.variable x.time x.pc) process_queue
let report () =
  begin
    Printf.printf "TEMPO ATUAL: %d\n\nPROCESSO EM EXECUÇÃO: " !time;
    if running_proc.ind > 0 then
    (let x = List.nth !pcb_table running_proc.ind in Printf.printf "ppid = %d, pid = %d, prioridade = %d, 
    valor da variável = %d, tempo de início = %d, tempo usado do CPU = %d\n\n" x.ppid x.pid x.priority x.variable x.time x.pc);
    Printf.printf "PROCESSOS BLOQUEADOS:\n";
    if Queue.is_empty blockedQ then Printf.printf "Nenhum processo bloqueado!\n"
    else print_queue blockedQ;
    Printf.printf "\nPROCESSOS PRONTOS A EXECUTAR:\n";
    if Queue.is_empty readyQ then Printf.printf "Nenhum processo pronto a executar!\n"
    else  print_queue readyQ;
    Printf.printf "PROCESSOS TERMINADOS:\n";
    if Queue.is_empty terminatedQ then Printf.printf "Nehum processo terminado!\n"
    else print_queue terminatedQ
  end