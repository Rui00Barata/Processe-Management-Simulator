(*Função de Escalonamento a Curto Prazo*)
open Lib 

let rec findProcInd queue pid count=
  match queue with
  | head::body -> if (head.pid = pid) then count else findProcInd (body) pid (count+1)
  | [] -> -1


let fcfs =
  let temp = Queue.pop readyQ in
  let ind = (findProcInd !pcb_table temp.pid 0) in
  begin
    if (ind < 0) then print_endline "ERRO! O Processo nao existe na tabela" else execute !time_quantum ind;
    match (List.nth (!pcb_table) ind).status with
    | 1 -> Queue.push temp readyQ
    | _ -> ()
  end  
