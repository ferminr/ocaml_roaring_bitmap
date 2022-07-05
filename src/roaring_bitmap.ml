
module IntHash =
  struct
    type t = int
    let equal (i : int) j = i=j
    let hash (i : int)= Hashtbl.hash i
  end



module IntHashtbl = Hashtbl.Make(IntHash)

type bmapc = { mutable sz: int; d: Bytes.t }
type container = 
  | Bmap of bmapc 
  | Arr of Unsigned.UInt16.t Varray.t

type t = container IntHashtbl.t


let create ?(n=20) () : t = IntHashtbl.create n

let empty () = create () 

let is_empty (bmap : t) = IntHashtbl.length bmap = 0


let stats (bmap : t) = 
  let cnt (nb,na,l,sz) = function
    | Bmap b -> (nb+1,na,l+b.sz, sz)
    | Arr arr  -> (nb,na+1,l+Varray.length arr,sz+Varray.length arr) in
  let (nb,na,l,sz) = IntHashtbl.fold (fun _ c acc -> cnt acc c) bmap (0,0,0,0) in
  (("bmaps", nb), ("arrays", na), ("elems", l), ("avg arr size", if na = 0 then 0 else sz / na))


(* Lookup e in arr[lo..hi] by binary search, returning the first index where we'd insert e to preserve the sorted property.
   Returns i = min value in [lo..hi] such that arr[i] >= e 
   If e>arr[i] for every i in [lo..hi], then return hi+1
   Precondition: arr sorted. 
  *)
let lookup e arr =
  let rec look e arr lo hi =
    if lo > hi then
      lo
    else 
      let mid = lo + (hi-lo) / 2 in
      match compare e (Varray.get arr mid) with
      |  0 -> mid
      | -1 -> look e arr lo (mid-1)
      |  _ -> look e arr (mid+1) hi
  in
  look e arr 0 (Varray.length arr - 1)

let mem (bmap : t) i = 
  let key = i asr 16 and
      low = Unsigned.UInt16.of_int i in
  match IntHashtbl.find_opt bmap key with
  | None -> false
  | Some (Bmap b) -> 
      (* b is 1024 64-bit words (8kB = 8192 bytes), i in [0..65535]   *)
      let byte = Bytes.get_uint8 b.d (Unsigned.UInt16.to_int low / 8) and
          mask = 1 lsl ((Unsigned.UInt16.to_int low) mod 8) in
      byte land mask <> 0
  | Some (Arr a) -> 
      let idx = lookup low a in
      idx >=0 && idx < Varray.length a


let bmapc_add b low = 
  let low = Unsigned.UInt16.to_int low in
  let index = low / 8 in 
  let byte = Bytes.get_uint8 b.d index and
      mask = 1 lsl (low mod 8) in
  if byte land mask = 0 then begin
    let newbyte = byte lor mask in 
    Bytes.set_uint8 b.d index newbyte;
    b.sz <- b.sz + 1
  end



let add (bmap: t) i = 
  let key = i asr 16 and
      low = Unsigned.UInt16.of_int i in
  match IntHashtbl.find_opt bmap key with
  | None -> IntHashtbl.add bmap key (Arr (Varray.make 1 low))
  | Some (Bmap b) -> bmapc_add b low
  | Some (Arr a) -> 
      let idx = lookup low a in 
      if idx = Varray.length a || (idx < Varray.length a && Varray.get a idx <> low) then begin
        if (Varray.length a = 4096) then
          (* convert to bmap *)
          let newc = { sz = 0 ; d = Bytes.make 8192 (Char.chr 0)} in
          Varray.iter (bmapc_add newc) a;
          bmapc_add newc low;
          IntHashtbl.replace bmap key (Bmap newc) 
        else
          Varray.insert_at a idx low
      end
      else 
        (* Nothing to do: low is already in the array *)
        () 


let bmapc_iter f bytes =
  (* bytes is 1024 64-bit words (8kB = 8192 bytes), i in [0..65535] *)
  for i = 0 to 8191 do
    let uint = Bytes.get_uint8 bytes i in
    if uint <> 0 then
      for j = 0 to 7 do
        let mask = 1 lsl j in 
        if uint land mask <> 0 then
          f (i*8 + j)
      done
  done

let bmapc_foldl f acc bytes =
  let r = ref acc in
  bmapc_iter (fun u16 -> r := f (!r) u16) bytes;
  !r
  

let remove (bmap : t) i = 
  let key = i asr 16 and
      low = i land 65535 in
  match IntHashtbl.find_opt bmap key with
  | None -> ()
  | Some (Bmap b) -> 
    (* b is 1024 64-bit words (8kB = 8192 bytes), i in [0..65535] *)
    let index = low / 8 in 
    let byte = Bytes.get_uint8 b.d index and
        mask = 1 lsl (low mod 8) in
    if byte land mask <> 0 then begin
        if b.sz > 4097 then
          let newbyte = byte land (255 - mask) in 
          Bytes.set_uint8 b.d index newbyte;
          b.sz <- b.sz - 1
        else begin
          let newc = Varray.empty () in
          (* TODO: create array of size 4096 and use set instead of push_back *)
          bmapc_iter (fun u16 -> if u16 <> low then Varray.push_back newc (Unsigned.UInt16.of_int u16)) b.d;
          IntHashtbl.replace bmap key (Arr newc) 
        end 
      end
  | Some (Arr a) -> 
    let low = Unsigned.UInt16.of_int low in
    let idx = lookup low a in 
    if idx < Varray.length a && Varray.get a idx = low then
      if Varray.length a = 1 then 
        IntHashtbl.remove bmap key
      else
        Varray.delete_at a idx



let of_iterable iter s =
  let bmap = empty() in
  iter (add bmap) s;
  bmap

let of_list = of_iterable List.iter

let of_array = of_iterable Array.iter

let of_seq = of_iterable Seq.iter


(* Note: doesn't iterate in sorted order of elements *)
let iter f (bmap : t) =
  let citer k = function
    | Bmap b -> bmapc_iter (fun u16 -> f ((k lsl 16) lor u16)) b.d
    | Arr a -> Varray.iter (fun u16 -> f ((k lsl 16) lor Unsigned.UInt16.to_int u16)) a 
  in
  IntHashtbl.iter citer bmap 


let fold f (bmap : t) z =
  let foldc k c acc = 
    match c with
    | Bmap b -> bmapc_foldl (fun acc u16 -> f acc ((k lsl 16) lor u16)) acc b.d
    | Arr a -> Varray.fold_left (fun acc u16 -> f acc ((k lsl 16) lor Unsigned.UInt16.to_int u16)) acc a 
  in 
  IntHashtbl.fold foldc bmap z


let to_seq (bmap : t) = 
  let s = Seq.empty in
  fold (fun acc i -> Seq.cons i acc) bmap s

