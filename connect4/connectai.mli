(** [pvp rows cols connect] initiates player versus player gameplay, prompting 
    different user preferences for the game mode and type *)
val pvp : int -> int -> int -> unit

(** [pvai rows cols connect] initiates player versus AI gameplay, prompting 
    different user preferences for the game mode and type *)
val pvai : int -> int -> int -> unit

(** [read_command] reads a user command and spits it out as a string *)
val read_command : unit -> string

(** [check_command_int input] checks if the input is an int *)
val check_command_int : string -> int

(** [play_game_pvp state] initiates a normal player vs player game with state 
    [state] *)
val play_game_pvp : Connectx.t -> unit

(** [play_game_pvp_terrain state] initiates a terrain mode player vs player 
    game with state 
    [state] *)
val play_game_pvp_terrain : Connectx.t -> unit

(** [play_game_ai_easy state] initiates a normal easy game versus ai with state 
    [state] *)
val play_game_ai_easy : Connectx.t -> unit

(** [play_game_ai_medium state] initiates a normal medium game versus ai with 
    state 
    [state] *)
val play_game_ai_medium : Connectx.t -> unit

(** [play_game_ai_hard state] initiates a normal hard game versus ai with state 
    [state] *)
val play_game_ai_hard : Connectx.t -> unit

(** [play_game_ai_easy state] initiates a terrain mode easy game versus ai with 
    state 
    [state] *)
val play_game_ai_easy_terrain : Connectx.t -> unit

(** [play_game_ai_medium state] initiates a terrain mode medium game versus ai
    with state 
    [state] *)
val play_game_ai_medium_terrain : Connectx.t -> unit

(** [play_game_ai_hard state] initiates a terrain mode hard game versus ai with 
    state 
    [state] *)
val play_game_ai_hard_terrain : Connectx.t -> unit

(**[trim_command str] takes a string and trims spaces on either side and makes
   all characters lowercase*)
val trim_command : string -> string

(** [take_turn_pvp game tracker] takes a single turn of a pvp game. It takes
    multiple turns as a recursive function. *)
val take_turn_pvp : Connectx.t -> int -> unit

(** [take_turn_ai_easy game tracker] takes a single turn of a normal easy ai 
    game. It takes multiple turns as a recursive function. *)
val take_turn_ai_easy : Connectx.t -> int -> unit

(** [take_turn_ai_medium game tracker] takes a single turn of a normal medium 
    ai game. It takes multiple turns as a recursive function. *)
val take_turn_ai_medium : Connectx.t -> int -> int -> unit

(** [take_turn_ai_hard game tracker] takes a single turn of a normal hard ai 
    game. It takes multiple turns as a recursive function. *)
val take_turn_ai_hard : Connectx.t -> int -> int -> unit

(** [take_turn_pvp_terrain game tracker] takes a single turn of a terrain pvp 
    game. It takes multiple turns as a recursive function. *)
val take_turn_pvp_terrain : Connectx.t -> int -> unit

(** [take_turn_ai_easy game tracker] takes a single turn of a terrain mode
    easy ai game. It takes multiple turns as a recursive function. *)
val take_turn_ai_easy_terrain : Connectx.t -> int -> unit

(** [take_turn_ai_medium game tracker] takes a single turn of a terrain mode
    medium ai game. It takes multiple turns as a recursive function. *)
val take_turn_ai_medium_terrain : Connectx.t -> int -> int -> unit

(** [take_turn_ai_hard game tracker] takes a single turn of a terrain mode
    hard ai game. It takes multiple turns as a recursive function. *)
val take_turn_ai_hard_terrain : Connectx.t -> int -> int -> unit

(** [check_mode input] forces the player's [input] to be either
    'normal' or 'terrain' *)
val check_mode : string -> string

(** [ai_move_safe col game chip] makes a safe ai move [col]. That is,
    it does not raise an exception by placing a chip in an invalid column
    or in a full column. *) 
val ai_move_safe : int -> Connectx.t -> Connectx.chips -> unit

(** [player_move_safe col game chip] makes a safe player move [col]. That is,
    it does not raise an exception by placing a chip in an invalid column
    or in a full column. *) 
val player_move_safe : int -> Connectx.t -> Connectx.chips -> unit

(** [check_chip input] checks if the input is a valid chip *)
val check_chip : string -> string

(** [check_play_col input cols] checks if the input satisifes the 
    column parameters *)
val check_play_col : string -> int -> int

(** [check_difficulty input cols] checks if the input id a valid difficulty *)
val check_difficulty : string -> string

(** [turn_rotation game tracker] rotates the turn in the game to the next player
    or AI. *)
val turn_rotation : Connectx.t -> int -> string
