(*Função de execução*)
open Lib

let execute t i =
  if t = 0 then ()
  else 
    begin 
      let p = List.nth !pcb_table i in
      running_proc := {ind = i; pid = p.pid; pc = p.pc};
      Sim.read_instr p;
      if p.status = 2 then push p blockedQ;
      if p.status = 3 then push p terminatedQ;
    end