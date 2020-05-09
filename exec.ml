(*Função de execução*)
open Lib

let execute i =
  begin 
    let p = List.nth !pcb_table i in
    Sim.read_instr p;
    if p.status = 2 then Queue.push p blockedQ;
    if p.status = 3 then Queue.push p terminatedQ;
    rem_time := !rem_time - 1
  end