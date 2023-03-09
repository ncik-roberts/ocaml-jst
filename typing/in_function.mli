val print_type_expr : (Format.formatter -> Types.type_expr -> unit) ref

type 'a how_to_print =
  | Type_expr : Types.type_expr how_to_print
  | Abbrev_memo : Types.abbrev_memo how_to_print
  | To_string : ('a -> string) -> 'a how_to_print
  | Formatter : (Format.formatter -> 'a -> unit) -> 'a how_to_print
  | Thunk : 'a how_to_print -> (unit -> 'a) how_to_print

module Arg_list : sig
  type t =
    | [] : t
    | ( :: ) : (string * 'a * 'a how_to_print) * t -> t
end

module Scoped : sig
  type t

  val call_and_print
    :  ?args:Arg_list.t
    -> ?ret:'a how_to_print
    -> string
    -> (t -> 'a)
    -> 'a

  val add_stack_frame
    :  ?ret:'a how_to_print
    -> t
    -> string
    -> args:Arg_list.t
    -> (unit -> 'a)
    -> 'a
end

val within
  :  ?__LOC__:string
  -> ?ret:'a how_to_print
  -> ?args:Arg_list.t
  -> ?ty1:Types.type_expr
  -> ?ty2:Types.type_expr
  -> ?ty:Types.type_expr
  -> ?ty':Types.type_expr
  -> string
  -> (unit -> 'a)
  -> 'a

val always_print : string -> 'a -> 'a how_to_print -> unit
