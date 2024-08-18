(*Type representing a chess piece.*)
type chess_piece =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen

(**Represents a player.*)
type player =
  | P1
  | P2

(**A game_piece represents a square on the chess board. A game_piece has:
   - a piece (can be None)
   - a player (can be None)
   - and a position *)

type game_piece = {
  piece : chess_piece;
  player : player;
  mutable pos : int * int;
}

type action = {
  game_piece : game_piece;
  new_pos : int * int;
}
(**A way of representing a game piece taking a specific action*)

type t = {
  mutable is_terminal : bool;
  mutable state : game_piece list;
  mutable check : bool * bool;
  mutable player_turn : player;
}
(**A game of chess.*)

exception IllegalMoveError of string
(**Thrown when the client attempts to make an illegal move.*)

val new_game : unit -> t
(**New game of chess, with the game state reset to its defaults.*)

val get_legal_moves : t -> game_piece -> (int * int) list

(**[get_legal_moves game game_piece] is a list of all legal terminal coordinates
   for legal moves that [game_piece] can make in context of [game].*)

val possible_positions : (int * int) list
(**[possible_positions] is a list of int doubles, referring to all possible
   positions on the chess board.*)

val print_chess_game : t -> unit
(**[print_chess_game chess_game] prints the current chess game to the standard
   output*)

val get_all_occupied_positions : t -> (int * int) list
(**[get_all_occupied_positions game] returns a list of int tuples representing
   the positions on the chess board that are occupied by a piece.*)

val get_all_free_positions : t -> (int * int) list
(**[get_all_free_positions game] returns a list of int tuples representing the
   positions on a chess board that are NOT occupied by a piece.*)

val game_piece_with_pos : t -> int * int -> game_piece
(**[game_piece_with_pos pos] is the game_piece that has [pos] as the position.
   Raises failure if [pos] is not the position of any game piece. *)

val take_action : t -> action -> t
(**[take_action action] updates the game to reflect the taken [action].*)
