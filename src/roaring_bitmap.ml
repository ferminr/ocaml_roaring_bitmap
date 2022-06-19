(* 
for down:
#require "varray";;
#require "integers";;

*)


(* Imperative bitmaps of unsigned32 *)


type container = Bmap of { mutable sz: int; d: Bytes.t }  | Arr of Unsigned.UInt16.t Varray.t


type t = (int , container) Hashtbl.t


(* TODO: what better than hardcode 100? *)
let empty : t = Hashtbl.create ~random:false 100

let is_empty (b : t) = 0 == Hashtbl.length b

let in_container i = function
  | Bmap b -> 
    (* b is 1024 64-bit words (8kB = 8192 bytes), i in [0..65535]   *)
    let byte = Bytes.get_uint8 b.d (Unsigned.UInt16.to_int i / 8) in
    false (*TODO: check if relevant bit is set *)
  | Arr a ->
    i == (Varray.get a 0) (* TODO: binsearch in the array *)

let add_to_container i c = 
  let add_it = function
    | Bmap b ->
      (* b is 1024 64-bit words (8kB = 8192 bytes), i in [0..65535]   *)
      let v = Bytes.get_uint8 b.d (Unsigned.UInt16.to_int i /8) in
      Bytes.set_int8 b.d (i/8) v (*TODO*)
    | Arr a ->
      Varray.push_back a i in   (* TODO: insert at relevant posn *)
  if not (in_container i c) then
    add_it c

let add (b : t) i = 
  let key = i asr 16 and
      low = Unsigned.UInt16.of_int i in
  match Hashtbl.find_opt b key with
  | None -> Hashtbl.add b key (Arr (Varray.make 1 low ))
  | Some c -> add_to_container low c (* TODO: convert arr to bmap if size demands it *)


let mem (b : t) i = 
  let key = i asr 16 in
  match Hashtbl.find_opt b key with
  | None -> false
  | Some c -> in_container (Unsigned.UInt16.of_int i) c

let remove i b = 
  let key = i asr 16 in
  match Hashtbl.find_opt b key with
  | None -> ()
  | Some c -> ()  (* TODO *)


(* Note: min, max not efficient with a hashtable, unless cached, like sz
let min_elt (b : t) = if is_empty b then None else Some 0

let max_elt (b : t) = if is_empty b then None else Some 0
*)

(* Complexity: linear in the number of containers *)
let length (b : t) =
  let len = function
    | Bmap b -> b.sz
    | Arr a -> Varray.length a in
  Hashtbl.fold (fun _ c acc -> acc + len c) b 0



let union (b1 : t) b2 =
  let replace bm c1 c2 = c1 in
  let s, l = if Hashtbl.length b1 < Hashtbl.length b2 then b1,b2 else b2,b1 in
  let r = Hashtbl.copy l in
  Hashtbl.iter (fun k v -> (Hashtbl.add r k v)) s; (* TODO: merge where keys agree *)
  r

let intersection (b1 : t) b2 = 
  let s, l = if Hashtbl.length b1 < Hashtbl.length b2 then b1,b2 else b2,b1 in
  let r = Hashtbl.copy s in
  let f k = None in 
  Hashtbl.filter_map_inplace (fun k v -> f k) r; (* TODO *)
  r

let flip (b : t) = "todo" 


let of_iterable iter s =
  let bmap = empty in
  iter (add bmap) s;
  bmap

let of_list = of_iterable List.iter

let of_array = of_iterable Array.iter

(** {1 Iterators} *)

let to_seq b = Seq.empty
let of_seq s = of_iterable Seq.iter


let iter f (b : t) =
  let citer k = function
  | Bmap b ->  Bytes.iter (fun _ -> ()) b.d
  | Arr a -> Varray.iter (fun _ -> ()) a in

  Hashtbl.iter (fun k c -> citer k c) b (* TODO *)

let fold f init (b : t) = Seq.fold_left f init (to_seq b)


let vector_insert_at v i elt =
  let open Vector in
  (* TODO: optimize with push if adding at end *)
  let l = length v in
  (* TODO: exception if i > len *)
  resize v (1+l);
  blit v i v (i+1) (l-i);
  set v i elt  

(* 

see this in Base.Binary_search
(* Find the index where an element [e] should be inserted *)
binary_search t ~get ~length ~compare `First_greater_than_or_equal_to e;

Base.Ordered_collection_common

*)

(* see https://discuss.ocaml.org/t/ann-carray-0-0-1/9938/6
   
...struct
  external get_int64_unsafe : bytes -> int -> int64 = "%caml_bytes_get64u"
  external set_int64_unsafe : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
  type t = Bytes.t
  let create x y =
    let p = Bytes.create 16 in 
    set_int64_unsafe p 0 (Int64.of_int x);
    set_int64_unsafe p 8 (Int64.of_int y); ...

*)

(* https://discuss.ocaml.org/t/pretty-printing-binary-ints/9062/7 *)
let int_size = Sys.word_size - 1
let int2bin =
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
    done;
    (* skip leading zeros *)
    match Bytes.index_opt buf '1' with
    | None -> "0b0"
    | Some i -> "0b" ^ Bytes.sub_string buf i (int_size - i)






(* typed hole 
let g x y = x + 1 + (_) y
*)

(* 
  int sets implemented as a list of ranges

  see:  /home/fermin/.opam/default_4.14.0/lib/core_kernel/int_set/int_set.ml 
   type t = Range.t list

   https://v3.ocaml.org/p/core_kernel/v0.15.0/doc/Int_set/index.html

   val empty : t

*)



(* 
  integers

> List.map Unsigned.UInt32.to_int Unsigned.UInt32.[of_int 103; one; of_string "1000"];;
- : int list = [103; 1; 1000]
*)
