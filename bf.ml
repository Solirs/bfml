

(* Our pointer in the array*)
let dtp : int ref = ref 0;;

(* Our array *)
let array = Array.make 30000 0;;
let rec bf_eval (i: int ref) (program: string) (brc: int ref) = (match program.[!i] with 
  | '>' -> dtp:= !dtp+1
  | '<' -> dtp := !dtp-1
  | '.' ->  Char.chr array.(!dtp) |> Printf.printf "%c%!"
  | '+' -> array.(!dtp) <- array.(!dtp)+1
  | '-' -> array.(!dtp) <- array.(!dtp)-1
  | ',' -> Scanf.bscanf  Scanf.Scanning.stdin "%c" (fun x-> array.(!dtp) <- int_of_char x) 

  (* Could have used a stack for this part but i refuse to fund big FILO *)
  | '[' -> brc := 1;(match array.(!dtp) with
              | 0 -> (while !brc != 0 do
                  i := !i+1;
                  (match String.get program !i with 
                  | ']' ->  brc := !brc-1
                  | '[' -> brc := !brc+1
                  | _ -> ());

                done)
              | _ -> ()
            )
  | ']' ->  brc := 1;(match array.(!dtp) with
            | 0 -> ()
            | _ -> (while !brc != 0 do
              i := !i-1;              
              (match String.get program !i with 
              | '[' ->  brc := !brc-1
              | ']' -> brc := !brc+1
              | _ -> ());

            done)
          )
  | _  -> ());


  i := !i+1;
  if !i < String.length program then bf_eval i program brc;;


(*Initial pos in the string*)
let x : int ref = ref 0;;
(* Just a pointer for computing matching brackets*)
let brc : int ref = ref 0;;

(* Your glorious program *)
let code : string = Sys.argv.(1);;

bf_eval x code brc;;
