
(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit -> 'a

(**[trim_command str] takes in a string [str] and trims the whitespace off of
   either side and makes the string lowercase.*)
val trim_command : string -> string

(**[read_command ()] reads a command from the terminal and creates a string.
   If the command is 'quit', [read_command ()] exits the program.*)
val read_command : unit -> string

(** [check_rc input] takes in a string [input] and determines if it is an 
    integer between 1 and 10. If it is not, the function prompts the user for 
    another input, and performs the same check. This repeats until the user 
    satisfies the condition.*)
val check_rc : string -> int

(** [check_to_win input] takes in a string [input] and determines if it is an 
    integer less than or equal to the greater number of [row] or [col]. If it 
    is not, the function prompts the user for another input, and performs the 
    same check. This repeats until the user satisfies the condition.*)
val check_to_win : string -> int -> int -> int

(** [check_play_type input] takes in a string [input] and determines if it is an 
    it is either 'ai' or 'player'. If it is not, the function prompts the user 
    for another input, and performs the same check. This repeats until the user 
    satisfies the condition.*)
val check_play_type : string -> string

(** [check_start_menu input] takes in a string [input] and determines if it is 
    an it is either 'start' or 'tutorial'. If it is not, the function prompts 
    the user for another input, and performs the same check. This repeats until  
    the user satisfies the condition.*)
val check_start_menu : string -> string

(** [check_endscreen input] takes in a string [input] and determines if it is 
    an it is 'menu'. If it is not, the function prompts the user for another 
    input, and performs the same check. This repeats until the user satisfies 
    the condition.*)
val check_endscreen : string -> string

(**[print_title] prints the title screen after clearing the terminal *)
val print_title : unit -> unit

(**[run_tutorial ()] runs the tutorial for the game.*)
val run_tutorial : unit -> unit

(**[initialize ()] runs the game, initializing the board (connectx type t), 
   and begins a new game after the old one finishes if the user desires.*)
val initialize : unit -> unit