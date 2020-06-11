(*Função de Escalonamento a Curto Prazo*)
open Lib 

let rec findProcInd (queue : pcb list) pid count =
  match queue with
  | head::body -> if (head.pid = pid) then count else findProcInd (body) pid (count+1)
  | [] -> -1

let findHighestPriority () =
  let proc = ref {name = ""; start = (-1); variable = (-1); pid = (-1); ppid = (-1); priority = (max_int); arrival_time = (-1); time = (-1); pc = (-1); status = (-1); finish = (-1)} in
  let () = begin
    for i = 0 to (Queue.length readyQ) do
      let p = Queue.pop readyQ in
      (if (p.priority < !proc.priority) then (proc := p);
      Queue.push p readyQ)
    done;
    for i = 0 to (Queue.length readyQ) do
      let p = Queue.pop readyQ in
      if (p.pid = !proc.pid) then ()
      else Queue.push p readyQ
    done
  end in !proc

let selected_scheduller = ref 1

(* First Come First Serve*)

let fcfs () =
  if running_proc.ind = -1 then
  let temp = Queue.pop readyQ in
  let ind = (findProcInd !pcb_table temp.pid 0) in
  if (ind < 0) then print_endline "ERRO! O Processo nao existe na tabela"
  else
    begin
      running_proc.ind <- ind;
      running_proc.pid <- temp.pid;
      running_proc.pc <- temp.pc;
      temp.status <- 1;
    end

(* Priority Schedulling *)

let priority_p () =
  if !preempt_flag then
    let p = findHighestPriority () in
    begin
      let temp = (List.nth !pcb_table running_proc.ind) in
      if temp.status = 1 then temp.status <- 0;
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

let priority () =
  let p = findHighestPriority () in
  let ind = findProcInd !pcb_table p.pid 0 in
  if ind < 0 then print_endline "ERRO! O Processo nao existe na tabela"
  else
    begin
      running_proc.ind <- ind;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

let short_sched () =
  match !selected_scheduller with
  |1 -> fcfs ()                                                             (* First Come First Serve *)
  |2 -> priority ()                                                         (* Priority w/ no preemption *)
  |3 -> priority_p ()                                                       (* Priority w/ preemption *)
  |_ -> fcfs ()