type 'a how_to_print =
  | Type_expr : Types.type_expr how_to_print
  | Abbrev_memo : Types.abbrev_memo how_to_print
  | To_string : ('a -> string) -> 'a how_to_print
  | Formatter : (Format.formatter -> 'a -> unit) -> 'a how_to_print
  | Thunk : 'a how_to_print -> (unit -> 'a) how_to_print

let print_type_expr = ref (fun _ (_ : Types.type_expr) -> ())
let buf = Buffer.create 1_000

let formatter_to_string : 'a. (Format.formatter -> 'a -> unit) -> 'a -> string =
  fun print thing ->
  let formatter = Format.formatter_of_buffer buf in
  Format.pp_set_margin formatter 1_000;
  print formatter thing;
  Format.pp_print_flush formatter ();
  let result = Buffer.contents buf in
  Buffer.reset buf;
  result
;;

let abbrev_memo_to_string (abbrev : Types.abbrev_memo) =
  let rec step (abbrev : Types.abbrev_memo) acc =
    match abbrev with
    | Mnil -> `Done (List.rev acc)
    | Mcons (_, path, ty1, ty2, rest) ->
      let path = formatter_to_string Path.print path in
      let ty1 = formatter_to_string !print_type_expr ty1 in
      let ty2 = formatter_to_string !print_type_expr ty2 in
      let str = Format.sprintf "(%s,%s,%s)" path ty1 ty2 in
      step rest (str :: acc)
    | Mlink ref -> `Continue (List.rev acc, !ref)
  in
  let rec loop (abbrev : Types.abbrev_memo) =
    match step abbrev [] with
    | `Done all -> Format.sprintf "[%s]" (String.concat ";" all)
    | `Continue (all, rest) ->
      Format.sprintf "[%s;%s]" (String.concat ";" all) (loop rest)
  in
  loop abbrev
;;

let rec to_string : type a. a how_to_print -> a -> string =
  fun how_to_print thing ->
  match how_to_print with
  | Type_expr -> formatter_to_string !print_type_expr thing
  | Abbrev_memo -> abbrev_memo_to_string thing
  | To_string to_string -> to_string thing
  | Formatter formatter -> formatter_to_string formatter thing
  | Thunk how_to_print ->
    (try to_string how_to_print (thing ()) with
     | _ -> "<exn>")
;;

module Arg_list = struct
  type t =
    | [] : t
    | ( :: ) : (string * 'a * 'a how_to_print) * t -> t

  type packed_arg = T : string * 'a * 'a how_to_print -> packed_arg

  let rec to_list : t -> packed_arg list = function
    | [] -> []
    | (x, y, z) :: xs -> T (x, y, z) :: to_list xs
  ;;

  let of_list : packed_arg list -> t =
    fun list -> List.fold_right (fun (T (x, y, z)) acc -> (x, y, z) :: acc) list []
  ;;

  let unpack args =
    List.map
      (fun (T (name, arg, how_to_print)) ->
         let get () = to_string how_to_print arg in
         let initial_arg = get () in
         let final_arg = ref None in
         let result = name, initial_arg, final_arg in
         let force () = final_arg := Some (get ()) in
         result, force)
      (to_list args)
    |> List.split
  ;;
end

let protect ?on_exn f ~(finally : unit -> unit) =
  let result =
    match f () with
    | x -> `Val x
    | exception exn ->
      Option.iter (fun f -> f exn) on_exn;
      `Exn exn
  in
  finally ();
  match result with
  | `Val x -> x
  | `Exn exn -> raise exn
;;

type stack_frame =
  { name : string (* name, initial, final *)
  ; args : (string * string * string option ref) list
  ; children : stack_frame Queue.t
  ; mutable result : [ `Value of string | `Value_unknown | `Exn | `In_progress ]
  }

let with_stack_frame ~name ~args ~ret f =
  let stack_frame = { name; args; children = Queue.create (); result = `In_progress } in
  let result =
    protect
      (fun () -> f stack_frame)
      ~finally:ignore
      ~on_exn:(fun _ -> stack_frame.result <- `Exn)
  in
  stack_frame.result
  <- (match ret with
    | None -> `Value_unknown
    | Some how_to_print -> `Value (to_string how_to_print result));
  result
;;

module Scoped = struct
  type t = { mutable top_stack_frame : stack_frame }

  let rec print_children ppf children ~indent =
    Queue.iter (print_stack_frame ppf ~indent) children

  and print_stack_frame ppf stack_frame ~indent =
    let ({ name; args; children; result } : stack_frame) = stack_frame in
    let tabs = String.init indent (fun _ -> '\t') in
    Format.fprintf ppf "%s- %s\n" tabs name;
    List.iter
      (fun (arg_name, initial, final) ->
         Format.fprintf ppf "%s * %s\n" tabs arg_name;
         Format.fprintf ppf "%s   Initial: %s\n" tabs initial;
         Format.fprintf ppf "%s   Final: %s\n" tabs (Option.get !final))
      args;
    (match result with
     | `Exn -> Format.fprintf ppf "%s   Return value: exn\n" tabs
     | `Value_unknown -> ()
     | `Value x -> Format.fprintf ppf "%s   Return value: %s\n" tabs x
     | `In_progress -> assert false);
    if not (Queue.is_empty children) then Format.fprintf ppf "%s * Children:\n" tabs;
    print_children ppf children ~indent:(indent + 1)
  ;;

  let print ppf t = print_stack_frame ppf t.top_stack_frame ~indent:0

  let call_and_print (type a) ?(args = Arg_list.[]) ?ret name f : a =
    let args, force_final_args = Arg_list.unpack args in
    let t_ref = ref None in
    protect
      (fun () ->
         with_stack_frame ~name ~args ~ret (fun top_stack_frame ->
           let t = { top_stack_frame } in
           t_ref := Some t;
           f t))
      ~finally:(fun () ->
        let t = Option.get !t_ref in
        List.iter (fun f -> f ()) force_final_args;
        let ppf = Format.std_formatter in
        print ppf t)
  ;;

  let add_stack_frame ?ret t name ~args f =
    let args, force_final_args = Arg_list.unpack args in
    let parent = t.top_stack_frame in
    with_stack_frame ~name ~args ~ret (fun stack_frame ->
      Queue.add stack_frame parent.children;
      t.top_stack_frame <- stack_frame;
      protect f ~finally:(fun () ->
        t.top_stack_frame <- parent;
        List.iter (fun f -> f ()) force_final_args))
  ;;
end

let t_ref = ref None
let always_print = ref []

let within (type a) ?__LOC__:loc ?ret ?(args = Arg_list.[]) name f : a =
  if not !Clflags.verbose
  then f ()
  else (
    let name =
      match loc with
      | None -> name
      | Some loc -> Format.sprintf "%s (%s)" name loc
    in
    let args = Arg_list.of_list (Arg_list.to_list args @ !always_print) in
    match !t_ref with
    | None ->
      protect
        (fun () : a ->
           Scoped.call_and_print name ~args ?ret (fun t : a ->
             t_ref := Some t;
             f ()))
        ~finally:(fun () -> t_ref := None)
    | Some t -> Scoped.add_stack_frame t name f ~args ?ret)
;;

let always_print (type a) str (value : a) how_to_print =
  always_print := !always_print @ [ Arg_list.T (str, value, how_to_print) ]
;;

let within ?__LOC__ ?ret ?args ?ty1 ?ty2 ?ty ?ty' name f =
  let add_ty ty name args : Arg_list.t =
    match ty with
    | None -> args
    | Some ty -> (name, ty, Type_expr) :: args
  in
  let args : Arg_list.t =
    match args with
    | None -> []
    | Some args -> args
  in
  let args =
    args |> add_ty ty2 "ty2" |> add_ty ty1 "ty1" |> add_ty ty' "ty'" |> add_ty ty "ty"
  in
  within ?__LOC__ ?ret ~args name f
;;
