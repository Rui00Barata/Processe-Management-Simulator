open Lib

let line_to_instr line =
  let x = String.split_on_char ' ' (remove_CR line) in
  match x with
  |[] -> {ins = char_of_int 0; n = 0; name = ""}
  |[hd] -> {ins = hd.[0]; n = 0; name = ""}
  |h::t -> 
    if h.[0] <> 'L'  then {ins = h.[0];n = int_of_string (List.hd t); name = ""}
    else if h.[0] = 'L' then {ins = h.[0];n = 0;name = (List.hd t)}
    else {ins = 'E';n = 0;name = ""}


let openfile newP =
  let process = {name = String.sub newP.name 0 (String.length newP.name - 4); start = !next_memory_index; variable = 0; pid = !next_pid; ppid = 0; priority = newP.priority; time = newP.time; pc = 0; status = 0; finish = 0} in
  let fi = open_in (newP.name) in
  let flag = ref true in
  let line = ref "" in
  let instr = ref {ins = 'N'; n = 0; name = ""} in
  while (!flag) do
      try 
      begin 
        line := input_line fi;
        instr := line_to_instr !line;
        memory.(!next_memory_index) <- !instr;
        next_memory_index := !next_memory_index + 1;
      end
      with End_of_file -> begin close_in fi; flag := false; pcb_table := !pcb_table @ [process]; Queue.push process readyQ; next_pid := !next_pid + 1 end
  done
  
let read_instr process =
  let instr = memory.(process.start + process.pc) in
  begin
    (match instr.ins with
    | 'M' -> (process.variable <- instr.n; process.pc<-(process.pc+1))
    | 'A' -> (process.variable <- (process.variable + instr.n); process.pc<-(process.pc+1)) 
    | 'S' -> (process.variable <- (process.variable - instr.n); process.pc<-(process.pc+1)) 
    | 'B' -> (process.status <- 2; process.pc<-(process.pc+1); executing_flag := false; rem_time := 0; running_proc.ind <- (Short.findProcInd !pcb_table process.pid 0); running_proc.pid <- process.pid; running_proc.pc <- process.pc)
    | 'T' -> (process.status <- 3; process.pc<-(process.pc+1); executing_flag := false; rem_time := 0; running_proc.ind <- -1; running_proc.pid <- -1; running_proc.pc <- -1; process.finish <- !time)
    | 'C' -> let proc = process in
            begin
              process.pc <- (process.pc + instr.n);
              proc.pid <- !next_pid;
              proc.ppid <- process.pid; 
              next_pid := !next_pid + 1;
              pcb_table := !pcb_table @ [proc]
            end
    | 'L' -> begin
              (List.nth !pcb_table (List.length !pcb_table - 1)).name <- instr.name;
              (*openfile (List.nth !pcb_table (List.length !pcb_table - 1)).name;*)
              process.pc<-(process.pc+1)
            end
    | _ -> Printf.fprintf stderr "Instrução inválida\n");
    running_proc.pc <- process.pc
  end

