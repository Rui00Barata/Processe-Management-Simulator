(*Função de execução*)
open Lib

let execute i =
  begin 
    let p = List.nth !pcb_table i in
    Sim.read_instr p;
    rem_time := !rem_time - 1;
    p.time <- p.time + 1;
    if p.status = 2 then (time_flag := true; Queue.push p  blockedQ);
    if p.status = 3 then (time_flag := true; Queue.push p terminatedQ);
  end