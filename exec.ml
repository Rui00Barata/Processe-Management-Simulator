(*Função de execução*)
open Lib

let execute i =
  begin 
    let p = List.nth !pcb_table i in
      Sim.read_instr p;
      rem_time := !rem_time - 1;
      Printf.printf "STATUS: %d\t%d\t\n" p.status !rem_time;
      (* if (!rem_time = 0 && p.status = 1) then (Printf.printf "READYQQQQQ!!\n"; Queue.push p readyQ); *)
      if p.status = 2 then Queue.push p  blockedQ;
      if p.status = 3 then (Printf.printf "TERMINADOS!!\n";Queue.push p terminatedQ);
  end