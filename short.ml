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

let findHighestPriority pri =
  let proc = ref {name = ""; start = (-1); variable = (-1); pid = (-1); ppid = (-1); priority = (pri); arrival_time = (-1); burst_time = (-1); time = (-1); pc = (-1); status = (-1); finish = (-1)} in
  let () = begin
    for i = 0 to (Queue.length readyQ - 1) do
      let p = Queue.pop readyQ in
      (if (p.priority < !proc.priority) then (proc := p);
      Queue.push p readyQ)
    done;
    for i = 0 to (Queue.length readyQ - 1) do
      let p = Queue.pop readyQ in
      if (p.pid = !proc.pid) then ()
      else Queue.push p readyQ
    done
  end in if (!proc.priority = pri) then (List.nth !pcb_table running_proc.ind) else !proc

let priority_p () =                      (* W/ Preemption *)
  if !preempt_flag then
    if (running_proc.ind <> (-1)) then
      let p = findHighestPriority ((List.nth !pcb_table running_proc.ind).priority) in
      begin
        if ((p <> (List.nth !pcb_table running_proc.ind))) then 
          let temp = (List.nth !pcb_table running_proc.ind) in 
          (temp.status <- 0; Queue.push temp readyQ);
        running_proc.ind <- findProcInd !pcb_table p.pid 0;
        running_proc.pid <- p.pid;
        running_proc.pc <- p.pc;
        p.status <- 1;
      end
    else
      let p = findHighestPriority (max_int) in
      begin
        running_proc.ind <- findProcInd !pcb_table p.pid 0;
        running_proc.pid <- p.pid;
        running_proc.pc <- p.pc;
        p.status <- 1;
      end

let priority () =                        (* Wo/ Preemption *)
  if (running_proc.ind = (-1)) then
    let p = findHighestPriority (max_int) in
    begin
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

(* Round Robin *)

let round_robin () =
  if (!rem_time = 0) then 
    let temp = Queue.pop readyQ in
    let ind = (findProcInd !pcb_table temp.pid 0) in
    if (ind < 0) then Printf.printf "ERRO! O Processo nao existe na tabela\n"
    else
      begin
        if running_proc.ind <> -1 then ((List.nth !pcb_table running_proc.ind).status <- 0; Queue.push (List.nth !pcb_table running_proc.ind) readyQ);
        running_proc.ind <- ind;
        running_proc.pid <- temp.pid;
        running_proc.pc <- temp.pc;
        temp.status <- 1;
        preempt_flag := false;
      end

(* Shortest Job First *)

let findShortestJob burst = 
  let proc = ref {name = ""; start = (-1); variable = (-1); pid = (-1); ppid = (-1); priority = (-1); arrival_time = (-1); burst_time = (burst); time = (-1); pc = (0); status = (-1); finish = (-1)} in
  let () = begin
    for i = 0 to (Queue.length readyQ - 1) do
      let p = Queue.pop readyQ in
      (if ((p.burst_time - p.pc) < (!proc.burst_time - !proc.pc)) then (proc := p)
      else if (p.burst_time = !proc.burst_time) then if (p.arrival_time < !proc.arrival_time) then (proc := p);
      Queue.push p readyQ)
    done;
    for i = 0 to (Queue.length readyQ - 1) do
      let p = Queue.pop readyQ in
      if (p.pid = !proc.pid) then ()
      else Queue.push p readyQ
    done
  end in if (!proc.burst_time = burst) then (List.nth !pcb_table running_proc.ind) else !proc

let sjf_p () =                           (* W/ Preemption *)
  if !preempt_flag then
  if (running_proc.ind <> (-1)) then
    let p = findShortestJob ((List.nth !pcb_table running_proc.ind).burst_time - (List.nth !pcb_table running_proc.ind).pc) in
    begin
      if ((p <> (List.nth !pcb_table running_proc.ind))) then 
        let temp = (List.nth !pcb_table running_proc.ind) in 
        (temp.status <- 0; Queue.push temp readyQ);
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end
  else
    let p = findShortestJob (max_int) in
    begin
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

let sjf () =                             (* Wo/ Preemption *)
  if (running_proc.ind = (-1)) then
    let p = findShortestJob (max_int) in
    begin
      running_proc.ind <- findProcInd !pcb_table p.pid 0;
      running_proc.pid <- p.pid;
      running_proc.pc <- p.pc;
      p.status <- 1;
    end

let short_sched () =
  ((match !selected_scheduller with
  |1 -> fcfs ()                                                             (* First Come First Serve *)
  |2 -> priority ()                                                         (* Priority wo/ preemption *)
  |3 -> priority_p ()                                                       (* Priority w/ preemption *)
  |4 -> sjf ()                                                              (* Shortest Job First wo/ preemption *)
  |5 -> sjf_p ()                                                            (* Shortest Job First w/ preemption *)
  |6 -> round_robin ()                                                      (* Round Robin w/ preemption *)
  |_ -> fcfs ());
  if (!selected_scheduller <> 6) then preempt_flag := false;)