(*Função de Escalonamento a Curto Prazo*)
open Lib 

let rec findProcInd (queue : pcb list) pid count =
  match queue with
  | head::body -> if (head.pid = pid) then count else findProcInd (body) pid (count+1)
  | [] -> -1

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

(* Auxiliary Function *)

let findHighestPriority () =
  let proc = ref {name = ""; start = (-1); variable = (-1); pid = (-1); ppid = (-1); priority = (max_int); arrival_time = (-1); burst_time = (-1); time = (-1); pc = (-1); status = (-1); finish = (-1)} in
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

let priority_p () =                      (* W/ Preemption *)
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


let priority () =                        (* Wo/ Preemption *)
  let p = findHighestPriority () in
  begin
    running_proc.ind <- findProcInd !pcb_table p.pid 0;
    running_proc.pid <- p.pid;
    running_proc.pc <- p.pc;
    p.status <- 1;
  end

(* Round Robin *)

let round_robin () = 
  let temp = Queue.pop readyQ in
  let ind = (findProcInd !pcb_table temp.pid 0) in
  if (ind < 0) then print_endline "ERRO! O Processo nao existe na tabela"
  else
    begin
      if runnning_proc.ind <> -1 then ((List.nth !pcb_table running_proc.ind).status <- 0; Queue.push (List.nth !pcb_table running_proc.ind) readyQ);
      running_proc.ind <- ind;
      running_proc.pid <- temp.pid;
      running_proc.pc <- temp.pc;
      temp.status <- 1;
    end

(* Shortest Job First *)

let findShortestJob () = 
  let proc = ref {name = ""; start = (-1); variable = (-1); pid = (-1); ppid = (-1); priority = (-1); arrival_time = (-1); burst_time = (max_int); time = (-1); pc = (-1); status = (-1); finish = (-1)} in
  let () = begin
    for i = 0 to (Queue.length readyQ) do
      let p = Queue.pop readyQ in
      (if (p.burst_time < !proc.burst_time) then (proc := p)
      else if (p.burst_time = !proc.burst_time) then if (p.arrival_time < !proc.arrival_time) then (proc := p);
      Queue.push p readyQ)
    done;
    for i = 0 to (Queue.length readyQ) do
      let p = Queue.pop readyQ in
      if (p.pid = !proc.pid) then ()
      else Queue.push p readyQ
    done
  end in !proc

let sjf_p () =                           (* W/ Preemption *)
  if !preempt_flag then
    let p = findShortestJob () in
    begin
      let temp = (List.nth !pcb_table running_proc.ind) in
      if temp.status = 1 then temp.status <- 0;
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

let sjf () =
  let p = findShortestJob () in
  begin
    running_proc.ind <- findProcInd !pcb_table p.pid 0;
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