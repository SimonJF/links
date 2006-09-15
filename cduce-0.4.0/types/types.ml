(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident
open Encodings

let count = ref 0
		
let () =
  Stats.register Stats.Summary
    (fun ppf -> Format.fprintf ppf "Allocated type nodes:%i@\n" !count)

(*
To be sure not to use generic comparison ...
*)
let (=) : int -> int -> bool = (==)
let (<) : int -> int -> bool = (<)
let (<=) : int -> int -> bool = (<=)
let (<>) : int -> int -> bool = (<>)
let compare = 1

type const = 
  | Integer of Intervals.V.t
  | Atom of Atoms.V.t
  | Char of Chars.V.t
  | Pair of const * const
  | Xml of const * const
  | Record of const label_map
  | String of U.uindex * U.uindex * U.t * const

module Const = struct
  type t = const

  let check _ = ()
  let dump ppf _ = Format.fprintf ppf "<Types.Const.t>"

  let rec compare c1 c2 = match (c1,c2) with
    | Integer x, Integer y -> Intervals.V.compare x y
    | Integer _, _ -> -1
    | _, Integer _ -> 1
    | Atom x, Atom y -> Atoms.V.compare x y
    | Atom _, _ -> -1
    | _, Atom _ -> 1
    | Char x, Char y -> Chars.V.compare x y
    | Char _, _ -> -1
    | _, Char _ -> 1
    | Pair (x1,x2), Pair (y1,y2) ->
	let c = compare x1 y1 in
	if c <> 0 then c else compare x2 y2
    | Pair (_,_), _ -> -1
    | _, Pair (_,_) -> 1
    | Xml (x1,x2), Xml (y1,y2) ->
	let c = compare x1 y1 in
	if c <> 0 then c else compare x2 y2
    | Xml (_,_), _ -> -1
    | _, Xml (_,_) -> 1
    | Record x, Record y ->
	LabelMap.compare compare x y
    | Record _, _ -> -1
    | _, Record _ -> 1
    | String (i1,j1,s1,r1), String (i2,j2,s2,r2) ->
	let c = Pervasives.compare i1 i2 in if c <> 0 then c 
	else let c = Pervasives.compare j1 j2 in if c <> 0 then c
	else let c = U.compare s1 s2 in if c <> 0 then c (* Should compare
							    only the substring *)
	else compare r1 r2

  let rec hash = function
    | Integer x -> 1 + 17 * (Intervals.V.hash x)
    | Atom x -> 2 + 17 * (Atoms.V.hash x)
    | Char x -> 3 + 17 * (Chars.V.hash x)
    | Pair (x,y) -> 4 + 17 * (hash x) + 257 * (hash y)
    | Xml (x,y) -> 5 + 17 * (hash x) + 257 * (hash y)
    | Record x -> 6 + 17 * (LabelMap.hash hash x)
    | String (i,j,s,r) -> 7 + 17 * (U.hash s) + 257 * hash r
      (* Note: improve hash for String *)

  let equal c1 c2 = compare c1 c2 = 0
end

module Abstract =
struct
  module T = Custom.String
  type abs = T.t

  module V =
  struct
    type t = abs * Obj.t
  end

  include SortedList.FiniteCofinite(T)

  let print = function
    | Finite l -> List.map (fun x ppf -> Format.fprintf ppf "!%s" x) l
    | Cofinite l ->       
	[ fun ppf ->
	  Format.fprintf ppf "@[Abstract";
	  List.iter (fun x -> Format.fprintf ppf " \\@ !%s" x) l;
	  Format.fprintf ppf "@]" ]

end


type pair_kind = [ `Normal | `XML ]


module rec Descr : 
sig
(*
  Want to write:
    type s = { ... }
    include Custom.T with type t = s
  but a  bug (?) in OCaml 3.07 makes it impossible
*)
  type t = {
    atoms : Atoms.t;
    ints  : Intervals.t;
    chars : Chars.t;
    times : BoolPair.t;
    xml   : BoolPair.t;
    arrow : BoolPair.t;
    record: BoolRec.t;
    abstract: Abstract.t;
    absent: bool
  }
  val empty: t
  val dump: Format.formatter -> t -> unit
  val check: t -> unit
  val equal: t -> t -> bool
  val hash: t -> int
  val compare:t -> t -> int
end =
struct
  type t = {
    atoms : Atoms.t;
    ints  : Intervals.t;
    chars : Chars.t;
    times : BoolPair.t;
    xml   : BoolPair.t;
    arrow : BoolPair.t;
    record: BoolRec.t;
    abstract: Abstract.t;
    absent: bool
  }

  let print_lst ppf =
    List.iter (fun f -> f ppf; Format.fprintf ppf " |")

  let dump ppf d =
    Format.fprintf ppf "<types atoms(%a) times(%a) record(%a) xml(%a)>"
      print_lst (Atoms.print d.atoms)
      BoolPair.dump d.times
      BoolRec.dump d.record
      BoolPair.dump d.xml

  let empty = { 
    times = BoolPair.empty; 
    xml   = BoolPair.empty; 
    arrow = BoolPair.empty; 
    record= BoolRec.empty;
    ints  = Intervals.empty;
    atoms = Atoms.empty;
    chars = Chars.empty;
    abstract = Abstract.empty;
    absent= false;
  }

  let equal a b =
    (a == b) || (
      (Atoms.equal a.atoms b.atoms) &&
      (Chars.equal a.chars b.chars) &&
      (Intervals.equal a.ints  b.ints) &&
      (BoolPair.equal a.times b.times) &&
      (BoolPair.equal a.xml b.xml) &&
      (BoolPair.equal a.arrow b.arrow) &&
      (BoolRec.equal a.record b.record) &&
      (Abstract.equal a.abstract b.abstract) &&
      (a.absent == b.absent)
    )

  let compare a b =
    if a == b then 0 
    else let c = Atoms.compare a.atoms b.atoms in if c <> 0 then c
    else let c = Chars.compare a.chars b.chars in if c <> 0 then c
    else let c = Intervals.compare a.ints b.ints in if c <> 0 then c
    else let c = BoolPair.compare a.times b.times in if c <> 0 then c
    else let c = BoolPair.compare a.xml b.xml in if c <> 0 then c
    else let c = BoolPair.compare a.arrow b.arrow in if c <> 0 then c
    else let c = BoolRec.compare a.record b.record in if c <> 0 then c
    else let c = Abstract.compare a.abstract b.abstract in if c <> 0 then c
    else if a.absent && not b.absent then -1
    else if b.absent && not a.absent then 1
    else 0
      
  let hash a =
    let accu = Chars.hash a.chars in
    let accu = 17 * accu + Intervals.hash a.ints in
    let accu = 17 * accu + Atoms.hash a.atoms in
    let accu = 17 * accu + BoolPair.hash a.times in
    let accu = 17 * accu + BoolPair.hash a.xml in
    let accu = 17 * accu + BoolPair.hash a.arrow in
    let accu = 17 * accu + BoolRec.hash a.record in
    let accu = 17 * accu + Abstract.hash a.abstract in
    let accu = if a.absent then accu+5 else accu in
    accu

  let check a =
    Chars.check a.chars;
    Intervals.check a.ints;
    Atoms.check a.atoms;
    BoolPair.check a.times;
    BoolPair.check a.xml;
    BoolPair.check a.arrow;
    BoolRec.check a.record;
    Abstract.check a.abstract;
    ()


end
and Node :
sig
  type t = { id : int; cu: Compunit.t; mutable descr : Descr.t }
  val dump: Format.formatter -> t -> unit
  val check: t -> unit
  val equal: t -> t -> bool
  val hash: t -> int
  val compare:t -> t -> int
  val mk: int -> Descr.t -> t
end =

struct
  type t = { id : int; cu: Compunit.t; mutable descr : Descr.t }
  let check n = ()
  let dump ppf n = Format.fprintf ppf "X%i" n.id
  let hash x = x.id + Compunit.hash x.cu
  let compare x y = 
    let c = x.id - y.id in if c = 0 then Compunit.compare x.cu y.cu else c
  let equal x y = x==y || (x.id == y.id && (Compunit.equal x.cu y.cu))
  let mk id d = { id = id; cu = Compunit.current (); descr = d }
end

(* See PR#2920 in OCaml BTS *)
and NodeT : Custom.T with type t = Node.t =
struct
  type t = Node.t
  let dump x = Node.dump x
  let check x = Node.check x
  let equal x = Node.equal x
  let hash x = Node.hash x
  let compare x = Node.compare x
end


(* It is also possible to use Boolean instead of Bool here;
   need to analyze when each one is more efficient *)
and BoolPair : Bool.S with type elem = Node.t * Node.t = 
(*Bool.Simplify*)(Bool.Make)(Custom.Pair(NodeT)(NodeT))

and BoolRec : Bool.S with type elem = bool * Node.t label_map =
(*Bool.Simplify*)(Bool.Make)(Custom.Pair(Custom.Bool)(LabelSet.MakeMap(NodeT)))

module DescrHash = Hashtbl.Make(Descr)
module DescrMap = Map.Make(Descr)
module DescrSet = Set.Make(Descr)
module DescrSList = SortedList.Make(Descr)

type descr = Descr.t
type node = Node.t
include Descr

let forward_print = ref (fun _ _ -> assert false)

let make () = 
  incr count; 
  Node.mk !count empty

(*
let hash_cons = DescrHash.create 17000  

let define n d = 
  DescrHash.add hash_cons d n; 
  n.Node.descr <- d

let cons d = 
  try DescrHash.find hash_cons d 
  with Not_found ->
    incr count; 
    let n = Node.mk !count d in
    DescrHash.add hash_cons d n; n  
*)

let define n d = 
  n.Node.descr <- d

let cons d = 
  incr count; 
  Node.mk !count d


let any =  {
  times = BoolPair.full; 
  xml   = BoolPair.full; 
  arrow = BoolPair.full; 
  record= BoolRec.full; 
  ints  = Intervals.any;
  atoms = Atoms.any;
  chars = Chars.any;
  abstract = Abstract.any;
  absent= false;
}


let non_constructed =
  { any with  
      times = empty.times; xml = empty.xml; record = empty.record }
     
let non_constructed_or_absent = 
  { non_constructed with absent = true }
	     
let interval i = { empty with ints = i }
let times x y = { empty with times = BoolPair.atom (x,y) }
let xml x y = { empty with xml = BoolPair.atom (x,y) }
let arrow x y = { empty with arrow = BoolPair.atom (x,y) }
let record label t = 
  { empty with 
      record = BoolRec.atom (true,LabelMap.singleton label t) }
let record_fields (x : bool * node Ident.label_map) =
  { empty with record = BoolRec.atom x }
let atom a = { empty with atoms = a }
let char c = { empty with chars = c }
let abstract a = { empty with abstract = a }

let get_abstract t = t.abstract
      
let cup x y = 
  if x == y then x else {
    times = BoolPair.cup x.times y.times;
    xml   = BoolPair.cup x.xml y.xml;
    arrow = BoolPair.cup x.arrow y.arrow;
    record= BoolRec.cup x.record y.record;
    ints  = Intervals.cup x.ints  y.ints;
    atoms = Atoms.cup x.atoms y.atoms;
    chars = Chars.cup x.chars y.chars;
    abstract = Abstract.cup x.abstract y.abstract;
    absent= x.absent || y.absent;
  }
    
let cap x y = 
  if x == y then x else {
    times = BoolPair.cap x.times y.times;
    xml   = BoolPair.cap x.xml y.xml;
    record= BoolRec.cap x.record y.record;
    arrow = BoolPair.cap x.arrow y.arrow;
    ints  = Intervals.cap x.ints  y.ints;
    atoms = Atoms.cap x.atoms y.atoms;
    chars = Chars.cap x.chars y.chars;
    abstract = Abstract.cap x.abstract y.abstract;
    absent= x.absent && y.absent;
  }
    
let diff x y = 
  if x == y then empty else {
    times = BoolPair.diff x.times y.times;
    xml   = BoolPair.diff x.xml y.xml;
    arrow = BoolPair.diff x.arrow y.arrow;
    record= BoolRec.diff x.record y.record;
    ints  = Intervals.diff x.ints  y.ints;
    atoms = Atoms.diff x.atoms y.atoms;
    chars = Chars.diff x.chars y.chars;
    abstract = Abstract.diff x.abstract y.abstract;
    absent= x.absent && not y.absent;
  }
    



(* TODO: optimize disjoint check for boolean combinations *)
let trivially_disjoint a b =
  (Chars.disjoint a.chars b.chars) &&
  (Intervals.disjoint a.ints b.ints) &&
  (Atoms.disjoint a.atoms b.atoms) &&
  (BoolPair.trivially_disjoint a.times b.times) &&
  (BoolPair.trivially_disjoint a.xml b.xml) &&
  (BoolPair.trivially_disjoint a.arrow b.arrow) &&
  (BoolRec.trivially_disjoint a.record b.record) &&
  (Abstract.disjoint a.abstract b.abstract) &&
  (not (a.absent && b.absent))



let descr n = n.Node.descr
let internalize n = n
let id n = n.Node.id


let rec constant = function
  | Integer i -> interval (Intervals.atom i)
  | Atom a -> atom (Atoms.atom a)
  | Char c -> char (Chars.atom c)
  | Pair (x,y) -> times (const_node x) (const_node y)
  | Xml (x,y) -> xml (const_node x) (const_node y)
  | Record x -> record_fields (false ,LabelMap.map const_node x)
  | String (i,j,s,c) ->
      if U.equal_index i j then constant c
      else 
	let (ch,i') = U.next s i in
	constant (Pair (Char (Chars.V.mk_int ch), String (i',j,s,c)))
and const_node c = cons (constant c)

let neg x = diff any x

let any_node = cons any
let empty_node = cons empty

module LabelS = Set.Make(Label)

let any_or_absent = { any with absent = true } 
let only_absent = { empty with absent = true }

let get_record r =
  let labs accu (_,r) = 
    List.fold_left 
      (fun accu (l,_) -> LabelS.add l accu) accu (LabelMap.get r) in
  let extend descrs labs (o,r) =
    let rec aux i labs r =
      match labs with
	| [] -> ()
	| l1::labs ->
	    match r with
	      | (l2,x)::r when l1 == l2 -> 
		  descrs.(i) <- cap descrs.(i) (descr x);
		  aux (i+1) labs r
	      | r ->
		  if not o then 
		    descrs.(i) <- cap descrs.(i) only_absent; (* TODO:OPT *)
		  aux (i+1) labs r
    in
    aux 0 labs (LabelMap.get r);
    o
  in
  let line (p,n) =
    let labels = 
      List.fold_left labs (List.fold_left labs LabelS.empty p) n in
    let labels = LabelS.elements labels in
    let nlab = List.length labels in
    let mk () = Array.create nlab any_or_absent in

    let pos = mk () in
    let opos = List.fold_left 
		 (fun accu x -> 
		    (extend pos labels x) && accu)
		 true p in
    let p = (opos, pos) in

    let n = List.map (fun x ->
			let neg = mk () in
			let o = extend neg labels x in
			(o,neg)
		     ) n in
    (labels,p,n)
  in
  List.map line (BoolRec.get r)
   




(* Subtyping algorithm *)

let diff_t d t = diff d (descr t)
let cap_t d t = cap d (descr t)
let cup_t d t = cup d (descr t)
let cap_product any_left any_right l =
  List.fold_left 
    (fun (d1,d2) (t1,t2) -> (cap_t d1 t1, cap_t d2 t2))
    (any_left,any_right)
    l
let any_pair = { empty with times = any.times }


let rec exists max f =
  (max > 0) && (f (max - 1) || exists (max - 1) f)

exception NotEmpty

type slot = { mutable status : status; 
	       mutable notify : notify;
	       mutable active : bool }
and status = Empty | NEmpty | Maybe
and notify = Nothing | Do of slot * (slot -> unit) * notify

let slot_empty = { status = Empty; active = false; notify = Nothing }
let slot_not_empty = { status = NEmpty; active = false; notify = Nothing }

let rec notify = function
  | Nothing -> ()
  | Do (n,f,rem) -> 
      if n.status == Maybe then (try f n with NotEmpty -> ());
      notify rem

let rec iter_s s f = function
  | [] -> ()
  | arg::rem -> f arg s; iter_s s f rem


let set s =
  s.status <- NEmpty;
  notify s.notify;
  s.notify <- Nothing; 
  raise NotEmpty

let rec big_conj f l n =
  match l with
    | [] -> set n
    | [arg] -> f arg n
    | arg::rem ->
	let s = 
	  { status = Maybe; active = false; 
	    notify = Do (n,(big_conj f rem), Nothing) } in
	try 
	  f arg s;
	  if s.active then n.active <- true
	with NotEmpty -> if n.status == NEmpty then raise NotEmpty

let guard a f n =
  match a with
    | { status = Empty } -> ()
    | { status = Maybe } as s -> 
	n.active <- true; 
	s.notify <- Do (n,f,s.notify)
    | { status = NEmpty } -> f n


(* Fast approximation *)

module ClearlyEmpty = 
struct

let memo = DescrHash.create 8191
let marks = ref [] 

let rec slot d =
  if not ((Intervals.is_empty d.ints) && 
	  (Atoms.is_empty d.atoms) &&
	  (Chars.is_empty d.chars) &&
	  (Abstract.is_empty d.abstract) &&
	  (not d.absent)) then slot_not_empty 
  else try DescrHash.find memo d
  with Not_found ->
    let s = { status = Maybe; active = false; notify = Nothing } in
    DescrHash.add memo d s;
    (try
       iter_s s check_times (BoolPair.get d.times);  
       iter_s s check_xml (BoolPair.get d.xml); 
       iter_s s check_arrow (BoolPair.get d.arrow);
       iter_s s check_record (get_record d.record);
       if s.active then marks := s :: !marks else s.status <- Empty;
     with
	 NotEmpty -> ());
    s

and check_times (left,right) s =
  let (accu1,accu2) = cap_product any any left in
  let single_right (t1,t2) s =
    let t1 = descr t1 and t2 = descr t2 in
    if trivially_disjoint accu1 t1 || trivially_disjoint accu2 t2 then set s 
    else
      let accu1 = diff accu1 t1 in guard (slot accu1) set s;
      let accu2 = diff accu2 t2 in guard (slot accu2) set s in
  guard (slot accu1) (guard (slot accu2) (big_conj single_right right)) s

and check_xml (left,right) s =
  let (accu1,accu2) = cap_product any any_pair left in
  let single_right (t1,t2) s =
    let t1 = descr t1 and t2 = descr t2 in
    if trivially_disjoint accu1 t1 || trivially_disjoint accu2 t2 then set s 
    else
      let accu1 = diff accu1 t1 in guard (slot accu1) set s;
      let accu2 = diff accu2 t2 in guard (slot accu2) set s in
  guard (slot accu1) (guard (slot accu2) (big_conj single_right right)) s

and check_arrow (left,right) s =
  let single_right (s1,s2) s =
    let accu1 = descr s1 and accu2 = neg (descr s2) in
    let single_left (t1,t2) s =
      let accu1 = diff_t accu1 t1 in guard (slot accu1) set s;
      let accu2 = cap_t  accu2 t2 in guard (slot accu2) set s
    in
    guard (slot accu1) (big_conj single_left left) s
  in
  big_conj single_right right s

and check_record (labels,(oleft,left),rights) s =
  let rec single_right (oright,right) s = 
    let next =
      (oleft && (not oright)) ||
      exists (Array.length left)
	(fun i -> trivially_disjoint left.(i) right.(i))
    in
    if next then set s
    else
      for i = 0 to Array.length left - 1 do
	let di = diff left.(i) right.(i) in guard (slot di) set s
      done
  in
  let rec start i s =
    if (i < 0) then big_conj single_right rights s
    else guard (slot left.(i)) (start (i - 1)) s
  in
  start (Array.length left - 1) s


let is_empty d =
  let s = slot d in
  List.iter 
    (fun s' -> 
       if s'.status == Maybe then s'.status <- Empty; s'.notify <- Nothing) 
    !marks;
  marks := [];
  s.status == Empty
end

let clearly_disjoint t1 t2 =
(*
  if trivially_disjoint t1 t2 then true
  else
    if ClearlyEmpty.is_empty (cap t1 t2) then
      (Printf.eprintf "!\n"; true) else false
*)
  trivially_disjoint t1 t2 || ClearlyEmpty.is_empty (cap t1 t2) 

(* TODO: need to invesigate when ClearEmpty is a good thing... *)

let memo = DescrHash.create 8191
let marks = ref [] 

let count_subtype = Stats.Counter.create "Subtyping internal loop" 

let complex = ref 0

let rec slot d =
  incr complex;
  Stats.Counter.incr count_subtype; 
  if not ((Intervals.is_empty d.ints) && 
	  (Atoms.is_empty d.atoms) &&
	  (Chars.is_empty d.chars) &&
	  (Abstract.is_empty d.abstract) &&
	  (not d.absent)) then slot_not_empty 
  else try DescrHash.find memo d
  with Not_found ->
    let s = { status = Maybe; active = false; notify = Nothing } in
    DescrHash.add memo d s;
    (try
       iter_s s check_times (BoolPair.get d.times);  
       iter_s s check_xml (BoolPair.get d.xml); 
       iter_s s check_arrow (BoolPair.get d.arrow);
       iter_s s check_record (get_record d.record);
       if s.active then marks := s :: !marks else s.status <- Empty;
     with
	 NotEmpty -> ());
    s

and check_times (left,right) s =
  let rec aux accu1 accu2 right s = match right with
    | (n1,n2)::right ->
	let t1 = descr n1 and t2 = descr n2 in
	if trivially_disjoint accu1 t1 || 
	   trivially_disjoint accu2 t2 then (
	     aux accu1 accu2 right s )
	else (
          let accu1' = diff accu1 t1 in 
	  guard (slot accu1') (aux accu1' accu2 right) s;

          let accu2' = diff accu2 t2 in 
	  guard (slot accu2') (aux accu1 accu2' right) s  
	)
    | [] -> set s
  in
  let (accu1,accu2) = cap_product any any left in
  guard (slot accu1) (guard (slot accu2) (aux accu1 accu2 right)) s

and check_xml (left,right) s =
  let rec aux accu1 accu2 right s = match right with
    | (n1,n2)::right ->
	let t1 = descr n1 and t2 = descr n2 in
	if clearly_disjoint accu1 t1 || 
	   trivially_disjoint accu2 t2 then (
	     aux accu1 accu2 right s )
	else (
          let accu1' = diff accu1 t1 in 
	  guard (slot accu1') (aux accu1' accu2 right) s;

          let accu2' = diff accu2 t2 in 
	  guard (slot accu2') (aux accu1 accu2' right) s  
	)
    | [] -> set s
  in
  let (accu1,accu2) = cap_product any any_pair left in
  guard (slot accu1) (guard (slot accu2) (aux accu1 accu2 right)) s

and check_arrow (left,right) s =
  let single_right (s1,s2) s =
    let rec aux accu1 accu2 left s = match left with
      | (t1,t2)::left ->
          let accu1' = diff_t accu1 t1 in 
	  guard (slot accu1') (aux accu1' accu2 left) s;

          let accu2' = cap_t  accu2 t2 in 
	  guard (slot accu2') (aux accu1 accu2' left) s
      | [] -> set s
    in
    let accu1 = descr s1 in
    guard (slot accu1) (aux accu1 (neg (descr s2)) left) s
  in
  big_conj single_right right s

and check_record (labels,(oleft,left),rights) s =
  let rec aux left rights s = match rights with
    | [] -> set s
    | (oright,right)::rights ->
	let next =
	  (oleft && (not oright)) ||
	  exists (Array.length left)
	    (fun i -> trivially_disjoint left.(i) right.(i))
	in
	if next then aux left rights s
	else
	  for i = 0 to Array.length left - 1 do
	    let left' = Array.copy left in
	    let di = diff left.(i) right.(i) in
	    left'.(i) <- di;
	    guard (slot di) (aux left' rights) s;
	  done
  in
  let rec start i s =
    if (i < 0) then aux left rights s
    else
      guard (slot left.(i)) (start (i - 1)) s
  in
  start (Array.length left - 1) s



let timer_subtype = Stats.Timer.create "Types.is_empty"


let is_empty d =
  Stats.Timer.start timer_subtype; 
  let s = slot d in
  List.iter 
    (fun s' -> 
       if s'.status == Maybe then s'.status <- Empty; s'.notify <- Nothing) 
    !marks;
  marks := [];
  Stats.Timer.stop timer_subtype 
    (s.status == Empty)

(*
let is_empty d =
(*  let b1 = ClearlyEmpty.is_empty d in
  let b2 = is_empty d in
  assert (b2 || not b1);
  Printf.eprintf "b1 = %b; b2 = %b\n" b1 b2;
  b2  *)
  if ClearlyEmpty.is_empty d then (Printf.eprintf "!\n"; true) else is_empty d
*)  

(*
let is_empty d =
(*  Format.fprintf Format.std_formatter "complex=%i@."
	  !complex; *)
  if !complex = 0 then
    (let r = is_empty d in
     if !complex > 100 then
       (let c = !complex in
	Format.fprintf Format.std_formatter "is_empty (%i)@." c
	  (*Descr.dump (*!forward_print*) d*));
     complex := 0; r)
  else is_empty d
*)

let non_empty d = 
  not (is_empty d)

let subtype d1 d2 =
  is_empty (diff d1 d2)

let disjoint d1 d2 =
  is_empty (cap d1 d2)

let equiv d1 d2 = (subtype d1 d2) && (subtype d2 d1)

module Product =
struct
  type t = (descr * descr) list

  let other ?(kind=`Normal) d = 
    match kind with
      | `Normal -> { d with times = empty.times }
      | `XML -> { d with xml = empty.xml }

  let is_product ?kind d = is_empty (other ?kind d)

  let need_second = function _::_::_ -> true | _ -> false

  let normal_aux = function
    | ([] | [ _ ]) as d -> d
    | d ->

    let res = ref [] in

    let add (t1,t2) =
      let rec loop t1 t2 = function
	| [] -> res := (ref (t1,t2)) :: !res
	| ({contents = (d1,d2)} as r)::l ->
	    (*OPT*) 
(*	    if equal_descr d1 t1 then r := (d1,cup d2 t2) else*)
	      
	      let i = cap t1 d1 in
	      if is_empty i then loop t1 t2 l
	      else (
		r := (i, cup t2 d2);
		let k = diff d1 t1 in 
		if non_empty k then res := (ref (k,d2)) :: !res;
		
		let j = diff t1 d1 in 
		if non_empty j then loop j t2 l
	      )
      in
      loop t1 t2 !res
    in
    List.iter add d;
    List.map (!) !res


(* Partitioning:

(t,s) - ((t1,s1) | (t2,s2) | ... | (tn,sn))
=
(t & t1, s - s1) | ... | (t & tn, s - sn) | (t - (t1|...|tn), s)

*)
  let get_aux any_right d =
    let accu = ref [] in
    let line (left,right) =
      let (d1,d2) = cap_product any any_right left in
      if (non_empty d1) && (non_empty d2) then
	let right = List.map (fun (t1,t2) -> descr t1, descr t2) right in
	let right = normal_aux right in
	let resid1 = ref d1 in
	let () = 
	  List.iter
	    (fun (t1,t2) ->
	       let t1 = cap d1 t1 in
	       if (non_empty t1) then
		 let () = resid1 := diff !resid1 t1 in
		 let t2 = diff d2 t2 in
		 if (non_empty t2) then accu := (t1,t2) :: !accu
	    ) right in
	if non_empty !resid1 then accu := (!resid1, d2) :: !accu 
    in
    List.iter line (BoolPair.get d);
    !accu
(* Maybe, can improve this function with:
     (t,s) \ (t1,s1) = (t&t',s\s') | (t\t',s),
   don't call normal_aux *)


  let get ?(kind=`Normal) d = 
    match kind with
      | `Normal -> get_aux any d.times
      | `XML -> get_aux any_pair d.xml

  let pi1 = List.fold_left (fun acc (t1,_) -> cup acc t1) empty
  let pi2 = List.fold_left (fun acc (_,t2) -> cup acc t2) empty
  let pi2_restricted restr = 
    List.fold_left (fun acc (t1,t2) -> 
		      if is_empty (cap t1 restr) then acc
		      else cup acc t2) empty

  let restrict_1 rects pi1 =
    let aux acc (t1,t2) = 
      let t1 = cap t1 pi1 in if is_empty t1 then acc else (t1,t2)::acc in
    List.fold_left aux [] rects
  
  type normal = t

  module Memo = Map.Make(BoolPair)

  (* TODO: try with an hashtable *)
  (* Also, avoid lookup for simple products (t1,t2) *)
  let memo = ref Memo.empty
  let normal_times d = 
    try Memo.find d !memo 
    with
	Not_found ->
	  let gd = get_aux any d in
	  let n = normal_aux gd in
(* Could optimize this call to normal_aux because one already
   know that each line is normalized ... *)
	  memo := Memo.add d n !memo;
	  n

  let memo_xml = ref Memo.empty
  let normal_xml d = 
    try Memo.find d !memo_xml
    with
	Not_found ->
	  let gd = get_aux any_pair d in
	  let n = normal_aux gd in
	  memo_xml := Memo.add d n !memo_xml;
	  n

  let normal ?(kind=`Normal) d =
    match kind with 
      | `Normal -> normal_times d.times 
      | `XML -> normal_xml d.xml


(*
  let merge_same_2 r =
    let r = 
      List.fold_left 
	(fun accu (t1,t2) ->
	   let t = try DescrMap.find t2 accu with Not_found -> empty in
	   DescrMap.add t2 (cup t t1) accu
	) DescrMap.empty r in
    DescrMap.fold (fun t2 t1 accu -> (t1,t2)::accu) r []
*)	 

  let constraint_on_2 n t1 =
    List.fold_left 
      (fun accu (d1,d2) ->
	 if is_empty (cap d1 t1) then accu else cap accu d2)
      any
      n


  let clean_normal l =
    let rec aux accu (t1,t2) =
      match accu with
	| [] -> [ (t1,t2) ]
	| (s1,s2) :: rem when equiv t2 s2 -> (cup s1 t1, s2) :: rem
	| (s1,s2) :: rem -> (s1,s2) :: (aux rem (t1,t2)) in
    List.fold_left aux [] l

  let any = { empty with times = any.times }
  and any_xml = { empty with xml = any.xml }
  let is_empty d = d == []
  let any_of = function `XML -> any_xml | `Normal -> any
end

module Record = 
struct
  let has_record d = not (is_empty { empty with record = d.record })
  let or_absent d = { d with absent = true }
  let absent = or_absent empty
  let any_or_absent = or_absent any
  let any_or_absent_node = cons any_or_absent
  let has_absent d = d.absent

  let absent_node = cons absent

  module T = struct
    type t = descr
    let any = any_or_absent
    let cap = cap
    let cup = cup
    let diff = diff
    let is_empty = is_empty
    let empty = empty
  end
  module R = struct
    type t = descr
    let any = { empty with record = any.record }
    let cap = cap
    let cup = cup
    let diff = diff
    let is_empty = is_empty
    let empty = empty
  end
  module TR = Normal.Make(T)(R)

  let any_record = { empty with record = BoolRec.full }

  let atom o l = 
    if o && LabelMap.is_empty l then any_record else
    { empty with record = BoolRec.atom (o,l) }

  type zor = Pair of descr * descr | Any

  let aux_split d l=
    let f (o,r) =
      try
	let (lt,rem) = LabelMap.assoc_remove l r in
	Pair (descr lt, atom o rem)
      with Not_found -> 
	if o then
	  if LabelMap.is_empty r then Any else
	    Pair (any_or_absent, { empty with record = BoolRec.atom (o,r) })
	else
	  Pair (absent,
		{ empty with record = BoolRec.atom (o,r) })
    in
    List.fold_left 
      (fun b (p,n) ->
	 let rec aux_p accu = function
	   | x::p -> 
	       (match f x with
		  | Pair (t1,t2) -> aux_p ((t1,t2)::accu) p
		  | Any -> aux_p accu p)
	   | [] -> aux_n accu [] n
	 and aux_n p accu = function
	   | x::n -> 
	       (match f x with
		  | Pair (t1,t2) -> aux_n p ((t1,t2)::accu) n
		  | Any -> b)
	   | [] -> (p,accu) :: b in
	 aux_p [] p)
      []
      (BoolRec.get d.record)

  let split (d : descr) l =
    TR.boolean (aux_split d l)

  let split_normal d l =
    TR.boolean_normal (aux_split d l)


  let pi l d = TR.pi1 (split d l)

  let project d l =
    let t = pi l d in
    if t.absent then raise Not_found;
    t

  let project_opt d l =
    let t = pi l d in
    { t with absent = false }

  let condition d l t =
    TR.pi2_restricted t (split d l)

(* TODO: eliminate this cap ... (record l absent_node) when
   not necessary. eg. { ..... } \ l *)

  let remove_field d l = 
    cap (TR.pi2 (split d l)) (record l absent_node)

  let all_labels d =
    let res = ref LabelSet.empty in
    let aux (_,r) =
      let ls = LabelMap.domain r in
      res := LabelSet.cup ls !res in
    BoolRec.iter aux d.record;
    !res

  let first_label d =
    let min = ref Label.dummy in
    let aux (_,r) = match LabelMap.get r with
	(l,_)::_ -> min := Label.min l !min | _ -> () in
    BoolRec.iter aux d.record;
    !min

  let empty_cases d =
    let x = BoolRec.compute
	      ~empty:0 ~full:3 ~cup:(lor) ~cap:(land)
	      ~diff:(fun a b -> a land lnot b)
	      ~atom:(function (o,r) ->
		       assert (LabelMap.get r == []);
		       if o then 3 else 1
		    )
	      d.record in
    (x land 2 <> 0, x land 1 <> 0)

  let has_empty_record d =
    BoolRec.compute
      ~empty:false ~full:true ~cup:(||) ~cap:(&&)
      ~diff:(fun a b -> a && not b)
      ~atom:(function (o,r) ->
	       List.for_all 
	         (fun (l,t) -> (descr t).absent)
	         (LabelMap.get r)
	    )
      d.record
    

(*TODO: optimize merge
   - pre-compute the sequence of labels
   - remove empty or full { l = t }
*)

  let merge d1 d2 = 
    let res = ref empty in
    let rec aux accu d1 d2 =
      let l = Label.min (first_label d1) (first_label d2) in
      if l == Label.dummy then
	let (some1,none1) = empty_cases d1 
	and (some2,none2) = empty_cases d2 in
	let _none = none1 && none2 and some = some1 || some2 in
	let accu = LabelMap.from_list (fun _ _ -> assert false) accu in
	(* approx for the case (some && not none) ... *)
	res := cup !res (record_fields (some, accu))
      else
	let l1 = split d1 l and l2 = split d2 l in
	let loop (t1,d1) (t2,d2) =
	  let t = 
	    if t2.absent 
	    then cup t1 { t2 with absent = false } 
	    else t2 
	  in
	  aux ((l,cons t)::accu) d1 d2
	in
	List.iter (fun x -> List.iter (loop x) l2) l1
	  
    in
    aux [] d1 d2;
    !res

  let any = { empty with record = any.record }

  let get d =
    let rec aux r accu d =
      let l = first_label d in
      if l == Label.dummy then
	let (o1,o2) = empty_cases d in 
	if o1 || o2 then (LabelMap.from_list_disj r,o1,o2)::accu else accu
      else
	List.fold_left 
	  (fun accu (t1,t2) -> 
	     let x = (t1.absent, { t1 with absent = false }) in
	     aux ((l,x)::r) accu t2)
	  accu
	  (split d l)
    in
    aux [] [] d
end


module Print = 
struct
  let rec print_const ppf = function
    | Integer i -> Intervals.V.print ppf i
    | Atom a -> Atoms.V.print_quote ppf a
    | Char c -> Chars.V.print ppf c
    | Pair (x,y) -> Format.fprintf ppf "(%a,%a)" print_const x print_const y
    | Xml (x,y) -> Format.fprintf ppf "XML(%a,%a)" print_const x print_const y
    | Record r -> 
	Format.fprintf ppf "Record{";
	LabelMap.iteri
	  (fun l c -> 
	     Format.fprintf ppf "%a : %a; " Label.print_attr l print_const c)
	  r;
	Format.fprintf ppf "}"
    | String (i,j,s,c) ->
	Format.fprintf ppf "\"%a\" %a"
	U.print (U.mk (U.get_substr s i j))
	print_const c

  let nil_atom = Atoms.V.mk_ascii "nil"
  let nil_type = atom (Atoms.atom nil_atom)
  let (seqs_node,seqs_descr) = 
    let n = make () in
    let d = cup nil_type (times any_node n) in
    define n d;
    (n, d)

  let is_regexp t = subtype t seqs_descr

  type gname = string * Ns.QName.t

  type nd = { id : int; 
	     mutable def : d list; 
	     mutable state : 
	       [ `Expand | `None | `Marked 
	       | `GlobalName of gname
	       | `Named of U.t ] }
  and  d =
    | Name of gname
    | Regexp of nd Pretty.regexp
    | Atomic of (Format.formatter -> unit)
    | Pair of nd * nd
    | Char of Chars.V.t
    | Xml of [ `Tag of (Format.formatter -> unit) | `Type of nd ] * nd * nd
    | Record of (bool * nd) label_map * bool * bool
    | Arrows of (nd * nd) list * (nd * nd) list
    | Neg of nd
    | Abs of nd
  let compare x y = x.id - y.id

  module S = struct
    type t = nd
    let compare x y = x.id - y.id
    let hash x = x.id
    let equal x y = x.id = y.id
  end
  module Decompile = Pretty.Decompile(DescrHash)(S)

  module DescrPairMap = Map.Make(Custom.Pair(Descr)(Descr))

  let named = ref DescrMap.empty
  let named_xml = ref DescrPairMap.empty
  let register_global cu (name : Ns.QName.t) d = 
    if equal { d with xml = BoolPair.empty } empty then 
      (let l = (*Product.merge_same_2*) (Product.get ~kind:`XML d) in
       match l with
	 | [(t1,t2)] -> 
	     if DescrPairMap.mem (t1,t2) !named_xml then ()
	     else
	       named_xml := DescrPairMap.add (t1,t2) (cu,name) !named_xml
	 | _ -> ());
    if DescrMap.mem d !named then ()
    else named := DescrMap.add d (cu,name) !named
	
  let unregister_global d =
    if equal { d with xml = BoolPair.empty } empty then
      (let l = Product.get ~kind:`XML d in
      match l with
	 | [(t1,t2)] -> 
	     named_xml := DescrPairMap.remove (t1,t2) !named_xml
	 | _ -> ());
    named := DescrMap.remove d !named

  let memo = DescrHash.create 63
  let counter = ref 0
  let alloc def = { id = (incr counter; !counter); def = def; state = `None }

  let count_name = ref 0
  let name () =
    incr count_name;
    U.mk ("X" ^ (string_of_int !count_name))

  let to_print = ref []

  let trivial_rec b = 
    b == BoolRec.empty || 
    (is_empty { empty with record = BoolRec.diff BoolRec.full b})

  let trivial_pair b = b == BoolPair.empty || b == BoolPair.full

  let worth_abbrev d = 
    not (trivial_pair d.times && trivial_pair d.xml && 
	 trivial_pair d.arrow && trivial_rec d.record) 

  let worth_complement d =
    let aux f x y = if f x y = 0 then 1 else 0 in
    let n = 
      aux Atoms.compare d.atoms any.atoms +
      aux Chars.compare d.chars any.chars +
      aux Intervals.compare d.ints any.ints +
      aux BoolPair.compare d.times any.times +
      aux BoolPair.compare d.xml any.xml +
      aux BoolPair.compare d.arrow any.arrow +
      aux BoolRec.compare d.record any.record +
      aux Abstract.compare d.abstract any.abstract
    in
    n >= 5

  let rec prepare d =
    try DescrHash.find memo d
    with Not_found ->
      try 
	let n = DescrMap.find d !named in
	let s = alloc [] in
	s.state <- `GlobalName n;
	DescrHash.add memo d s;
	s
      with Not_found ->
	if d.absent then alloc [Abs (prepare ({d with absent=false}))]
	else if worth_complement d 
	then alloc [Neg (prepare (neg d))]
	else let slot = alloc [] in
	if not (worth_abbrev d) then slot.state <- `Expand;
	DescrHash.add memo d slot;
	let (seq,not_seq) =
	  if (subtype { empty with times = d.times } seqs_descr) then
	    (cap d seqs_descr, diff d seqs_descr)
	  else 
	    (empty, d) in
	
	let add u = slot.def <- u :: slot.def in
	if (non_empty seq) then
	  add (Regexp (decompile seq));  
	List.iter
	  (fun (t1,t2) -> add (Pair (prepare t1, prepare t2)))
	  (Product.get not_seq);
	List.iter
	  (fun (t1,t2) ->
	     try 
	       let n = DescrPairMap.find (t1,t2) !named_xml in
	       add (Name n)
	     with Not_found ->
	       let tag = 
		 match Atoms.print_tag t1.atoms with
		   | Some a when is_empty { t1 with atoms = Atoms.empty } -> `Tag a
		   | _ -> `Type (prepare t1) in
	       assert (equal { t2 with times = empty.times } empty);
	       List.iter
		 (fun (ta,tb) -> 
		    add (Xml (tag, prepare ta, prepare tb)))
		 (Product.get t2);
	  )
	  ((*Product.merge_same_2*) (Product.get ~kind:`XML not_seq));
	List.iter
	  (fun (r,some,none) -> 
	     let r = LabelMap.map (fun (o,t) -> (o, prepare t)) r in
	     add (Record (r,some,none)))
	  (Record.get not_seq);
	(match Chars.is_char not_seq.chars with
	   | Some c -> add (Char c)
	   | None ->
	       List.iter (fun x -> add (Atomic x)) (Chars.print not_seq.chars));
	List.iter (fun x -> add (Atomic x)) (Intervals.print not_seq.ints);
	List.iter (fun x -> add (Atomic x)) (Atoms.print not_seq.atoms);
	List.iter (fun x -> add (Atomic x)) (Abstract.print not_seq.abstract);
	List.iter
	  (fun (p,n) ->
	     let aux (t,s) = prepare (descr t), prepare (descr s) in
	     let p = List.map aux p and n = List.map aux n in
	     add (Arrows (p,n)))
	  (BoolPair.get not_seq.arrow);
	if not_seq.absent then add (Atomic (fun ppf -> Format.fprintf ppf "#ABSENT"));
	slot.def <- List.rev slot.def;
	slot
      
  and decompile d =
    Decompile.decompile 
      (fun t -> 
	 let tr = Product.get t in 
	 let tr = Product.clean_normal tr in

	 let tr = List.map (fun (l,t) -> prepare l, t) tr in
	 tr, Atoms.contains nil_atom t.atoms)
      d

  let gen = ref 0

  let rec assign_name s =
    incr gen;
    match s.state with
      | `None ->  
	  let g = !gen in
	  s.state <- `Marked; 
	  List.iter assign_name_rec s.def;
	  if (s.state == `Marked) && (!gen == g) then s.state <- `None
      | `Marked -> s.state <- `Named (name ()); to_print := s :: !to_print
      | _ -> ()
  and assign_name_rec = function
    | Neg t -> assign_name t
    | Abs t -> assign_name t
    | Name _ | Char _ | Atomic _ -> ()
    | Regexp r -> assign_name_regexp r
    | Pair (t1,t2) -> assign_name t1; assign_name t2
    | Xml (tag,t2,t3) -> 
	(match tag with `Type t -> assign_name t | _ -> ());
	assign_name t2;
	assign_name t3
    | Record (r,_,_) ->
	List.iter (fun (_,(_,t)) -> assign_name t) (LabelMap.get r)
    | Arrows (p,n) ->
	List.iter (fun (t1,t2) -> assign_name t1; assign_name t2) p;
	List.iter (fun (t1,t2) -> assign_name t1; assign_name t2) n
  and assign_name_regexp = function
    | Pretty.Epsilon | Pretty.Empty -> ()
    | Pretty.Alt (r1,r2) 
    | Pretty.Seq (r1,r2) -> assign_name_regexp r1; assign_name_regexp r2
    | Pretty.Star r | Pretty.Plus r -> assign_name_regexp r
    | Pretty.Trans t -> assign_name t

  let print_gname ppf (cu,n) = 
    Format.fprintf ppf "%s%a" cu Ns.QName.print n

  let rec do_print_slot pri ppf s =
    match s.state with
      | `Named n -> U.print ppf n
      | `GlobalName n -> print_gname ppf n
      | _ -> do_print_slot_real pri ppf s.def
  and do_print_slot_real pri ppf def =
    let rec aux ppf = function
      | [] -> Format.fprintf ppf "Empty"
      | [ h ] -> (do_print pri) ppf h
      | h :: t -> Format.fprintf ppf "%a |@ %a" (do_print pri) h aux t
    in
    if (pri >= 2) && (List.length def >= 2) 
    then Format.fprintf ppf "@[(%a)@]" aux def
    else aux ppf def
  and do_print pri ppf = function
    | Neg { def = [] } -> Format.fprintf ppf "Any"
    | Neg t -> Format.fprintf ppf "Any \\ (@[%a@])" (do_print_slot 0) t
    | Abs t -> Format.fprintf ppf "?(@[%a@])" (do_print_slot 0) t
    | Name n -> print_gname ppf n
    | Char c -> Chars.V.print ppf c
    | Regexp r -> Format.fprintf ppf "@[[ %a ]@]" (do_print_regexp 0) r
    | Atomic a -> a ppf
    | Pair (t1,t2) -> 
	Format.fprintf ppf "@[(%a,%a)@]" 
	  (do_print_slot 0) t1 
	  (do_print_slot 0) t2
    | Xml (tag,attr,t) -> 
	Format.fprintf ppf "<%a%a>%a" 
	  do_print_tag tag
	  do_print_attr attr
	  (do_print_slot 2) t
    | Record (r,some,none) ->
	Format.fprintf ppf "@[{";
	do_print_record ppf (r,some,none);
	Format.fprintf ppf " }@]"
    | Arrows (p,n) ->
	(match p with
	   | [] -> Format.fprintf ppf "Arrow"
	   | (t,s)::l ->
	       Format.fprintf ppf "%a" (do_print_arrow pri) (t,s);
	       List.iter 
		 (fun (t,s) ->
		    Format.fprintf ppf " &@ %a" (do_print_arrow pri) (t,s)
		 ) l);
	List.iter 
	  (fun (t,s) ->
	     Format.fprintf ppf " \\@ %a" (do_print_arrow pri) (t,s)
	  ) n
  and do_print_arrow pri ppf (t,s) =
    if (pri = 3) then Format.fprintf ppf "(";
    Format.fprintf ppf "%a -> %a"
      (do_print_slot 3) t
      (do_print_slot 2) s;
    if (pri = 3) then Format.fprintf ppf ")"
  and do_print_tag ppf = function
    | `Tag s -> s ppf
    | `Type t -> Format.fprintf ppf "(%a)" (do_print_slot 0) t
  and do_print_attr ppf = function
    | { state = `Marked|`Expand|`None; 
	def = [ Record (r,some,none) ] } -> do_print_record ppf (r,some,none)
    | t -> Format.fprintf ppf " (%a)" (do_print_slot 2) t
  and do_print_record ppf (r,some,none) =
    List.iter 
      (fun (l,(o,t)) ->
	 let opt = if o then "?" else "" in
	 Format.fprintf ppf "@ @[%a=%s@]%a"
	   Label.print_attr l opt (do_print_slot 0) t
      ) (LabelMap.get r);
    if not none then Format.fprintf ppf "@ (+others)";
    if some then Format.fprintf ppf " ..";
  and do_print_regexp pri ppf = function
    | Pretty.Empty ->  Format.fprintf ppf "Empty" (*assert false *)
    | Pretty.Epsilon -> ()
    | Pretty.Seq (Pretty.Trans { def = [ Char _ ] }, _) as r-> 
	(match extract_string [] r with
	  | s, None ->
	      Format.fprintf ppf "'";
	      List.iter (Chars.V.print_in_string ppf) s;
	      Format.fprintf ppf "'"
	  | s, Some r ->
	      if pri >= 3 then Format.fprintf ppf "@[(";
	      Format.fprintf ppf "'";
	      List.iter (Chars.V.print_in_string ppf) s;
	      Format.fprintf ppf "' %a" (do_print_regexp 2) r;
	      if pri >= 3 then Format.fprintf ppf ")@]")
    | Pretty.Seq (r1,r2) -> 
	if pri >= 3 then Format.fprintf ppf "@[(";
	Format.fprintf ppf "%a@ %a" 
	  (do_print_regexp 2) r1 
	  (do_print_regexp 2) r2;
	if pri >= 3 then Format.fprintf ppf ")@]"
    | Pretty.Alt (r,Pretty.Epsilon) | Pretty.Alt (Pretty.Epsilon,r) ->
	Format.fprintf ppf "@[%a@]?" (do_print_regexp 3) r
    | Pretty.Alt (r1,r2) -> 
	if pri >= 2 then Format.fprintf ppf "@[(";
	Format.fprintf ppf "%a |@ %a" 
	  (do_print_regexp 1) r1 
	  (do_print_regexp 1) r2;
	if pri >= 2 then Format.fprintf ppf ")@]"
    | Pretty.Star r -> 
	Format.fprintf ppf "@[%a@]*" (do_print_regexp 3) r
    | Pretty.Plus r -> 
	Format.fprintf ppf "@[%a@]+" (do_print_regexp 3) r
    | Pretty.Trans t ->
	do_print_slot pri ppf t
  and extract_string accu = function
    | Pretty.Seq (Pretty.Trans { def = [ Char c ] }, r) ->
	extract_string (c :: accu) r
    | Pretty.Trans { def = [ Char c ] } ->
	(List.rev (c :: accu), None)
    | r -> (List.rev accu,Some r)


  let get_name = function
    | { state = `Named n } -> n
    | _ -> assert false

  let print ppf t =
    let t = prepare t in
    assign_name t;
    Format.fprintf ppf "@[@[%a@]" (do_print_slot 0) t;
    (match List.rev !to_print with
       | [] -> ()
       | s::t ->
	   Format.fprintf ppf 
	     " where@ @[<v>%a = @[%a@]" U.print (get_name s) 
	   (do_print_slot_real 0) s.def;
	   List.iter 
	     (fun s -> 
		Format.fprintf ppf " and@ %a = @[%a@]"  U.print
		  (get_name s) (do_print_slot_real 0) s.def)
	     t;
	   Format.fprintf ppf "@]"
    );
    Format.fprintf ppf "@]";
    count_name := 0;
    to_print := [];
    DescrHash.clear memo

  let print_noname ppf t =   
    let old_named = !named in
    let old_named_xml = !named_xml in
    unregister_global t;
    print ppf t;
    named := old_named;
    named_xml := old_named_xml

  let print_node ppf n = print ppf (descr n)

  let () = forward_print := print

  let print_to_string f x =
    let b = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer b in
    f ppf x;
    Format.pp_print_flush ppf ();
    Buffer.contents b

  let to_string t = print_to_string print t
    
end

module Positive =
struct
  type rhs = [ `Type of descr | `Cup of v list | `Times of v * v | `Xml of v * v ]
  and v = { mutable def : rhs; mutable node : node option }


  let rec make_descr seen v =
    if List.memq v seen then empty
    else
      let seen = v :: seen in
      match v.def with
	| `Type d -> d
	| `Cup vl -> 
	    List.fold_left (fun acc v -> cup acc (make_descr seen v)) empty vl
	| `Times (v1,v2) -> times (make_node v1) (make_node v2)
	| `Xml (v1,v2) -> xml (make_node v1) (make_node v2)

  and make_node v =
    match v.node with
      | Some n -> n
      | None ->
	  let n = make () in
	  v.node <- Some n;
	  let d = make_descr [] v in
	  define n d;
	  n

  let forward () = { def = `Cup []; node = None }
  let def v d = v.def <- d
  let cons d = let v = forward () in def v d; v
  let ty d = cons (`Type d)
  let cup vl = cons (`Cup vl)
  let times d1 d2 = cons (`Times (d1,d2))
  let xml d1 d2 = cons (`Xml (d1,d2))
  let define v1 v2 = def v1 (`Cup [v2]) 

  let solve v = internalize (make_node v)
end


let memo_normalize = ref DescrMap.empty


let rec rec_normalize d =
  try DescrMap.find d !memo_normalize
  with Not_found ->
    let n = make () in
    memo_normalize := DescrMap.add d n !memo_normalize;
    let times = 
      List.fold_left 
	(fun accu (d1,d2) -> BoolPair.cup accu (BoolPair.atom (rec_normalize d1, rec_normalize d2)))
	BoolPair.empty (Product.normal d)
    in
    let xml = 
      List.fold_left 
	(fun accu (d1,d2) -> BoolPair.cup accu (BoolPair.atom (rec_normalize d1, rec_normalize d2)))
	BoolPair.empty (Product.normal ~kind:`XML d)
    in
    let record = d.record
    in
    define n { d with times = times; xml = xml; record = record };
    n

let normalize n =
  descr (internalize (rec_normalize n))

module Arrow =
struct
  let check_simple left (s1,s2) =
    let rec aux accu1 accu2 = function
      | (t1,t2)::left ->
          let accu1' = diff_t accu1 t1 in
          if non_empty accu1' then aux accu1 accu2 left;
          let accu2' = cap_t accu2 t2 in
          if non_empty accu2' then aux accu1 accu2 left
      | [] -> raise NotEmpty
    in
    let accu1 = descr s1 in
    (is_empty accu1) ||
    (try aux accu1 (diff any (descr s2)) left; true with NotEmpty -> false)

  let check_line_non_empty (left,right) =
    not (List.exists (check_simple left) right)

  let sample t =
    let (left,right) = List.find check_line_non_empty (BoolPair.get t.arrow) in
    List.fold_left (fun accu (t,s) -> cap accu (arrow t s)) 
      { empty with arrow = any.arrow } left
	
      
  let check_strenghten t s =
(*
    let left = match (BoolPair.get t.arrow) with [ (p,[]) ] -> p | _ -> assert false in
    let rec aux = function
      | [] -> raise Not_found
      | (p,n) :: rem ->
	  if (List.for_all (fun (a,b) -> check_simple left a b) p) &&
	    (List.for_all (fun (a,b) -> not (check_simple left a b)) n) then
	      { empty with arrow = Obj.magic [ (SortedList.cup left p, n) ] }  (* rework this ! *)
	  else aux rem
    in
    aux (BoolPair.get s.arrow)
*)
    if subtype t s then t else raise Not_found

  let check_simple_iface left s1 s2 =
    let rec aux accu1 accu2 = function
      | (t1,t2)::left ->
          let accu1' = diff accu1 t1 in
          if non_empty accu1' then aux accu1 accu2 left;
          let accu2' = cap accu2 t2 in
          if non_empty accu2' then aux accu1 accu2 left
      | [] -> raise NotEmpty
    in
    let accu1 = descr s1 in
    (is_empty accu1) ||
    (try aux accu1 (diff any (descr s2)) left; true with NotEmpty -> false)

  let check_iface iface s =
    let rec aux = function
      | [] -> false
      | (p,n) :: rem ->
	  ((List.for_all (fun (a,b) -> check_simple_iface iface a b) p) &&
	   (List.for_all (fun (a,b) -> not (check_simple_iface iface a b)) n))
	  || (aux rem)
    in
    aux (BoolPair.get s.arrow)

  type t = descr * (descr * descr) list list

  let get t =
    List.fold_left
      (fun ((dom,arr) as accu) (left,right) ->
	 if check_line_non_empty (left,right)
	 then
	   let left = List.map (fun (t,s) -> (descr t, descr s)) left in
	   let d = List.fold_left (fun d (t,_) -> cup d t) empty left in
	   (cap dom d, left :: arr)
	 else accu
      )
      (any, [])
      (BoolPair.get t.arrow)

  let domain (dom,_) = dom

  let apply_simple t result left = 
    let rec aux result accu1 accu2 = function
      | (t1,s1)::left ->
          let result = 
	    let accu1 = diff accu1 t1 in
            if non_empty accu1 then aux result accu1 accu2 left
            else result in
          let result =
	    let accu2 = cap accu2 s1 in
            aux result accu1 accu2 left in
	  result
      | [] -> 
          if subtype accu2 result 
	  then result
	  else cup result accu2
    in
    aux result t any left
      
  let apply (_,arr) t =
    List.fold_left (apply_simple t) empty arr

  let need_arg (dom, arr) =
    List.exists (function [_] -> false | _ -> true) arr

  let apply_noarg (_,arr) =
    List.fold_left 
      (fun accu -> 
	 function 
	   | [(t,s)] -> cup accu s
	   | _ -> assert false
      )
      empty arr

  let any = { empty with arrow = any.arrow }
  let is_empty (_,arr) = arr == []
end
  

module Int = struct
  let has_int d i = Intervals.contains i d.ints
  let get d = d.ints
  let any = { empty with ints = any.ints }
end

module Atom = struct
  let has_atom d a = Atoms.contains a d.atoms
  let get d = d.atoms
  let any = { empty with atoms = any.atoms }
end

module Char = struct
  let has_char d c = Chars.contains c d.chars
  let is_empty d = Chars.is_empty d.chars
  let get d = d.chars
  let any = { empty with chars = any.chars }
end

let rec tuple = function
  | [t1;t2] -> times t1 t2
  | t::tl -> times t (cons (tuple tl))
  | _ -> assert false

let rec_of_list o l =
  let map = 
    LabelMap.from_list (fun _ _ -> assert false)
      (List.map 
	 (fun (opt,qname,typ) ->
	    qname, cons (if opt then Record.or_absent typ else typ))
	 l)
  in
  record_fields (o,map)

let empty_closed_record = rec_of_list false []
let empty_open_record = rec_of_list true []


let cond_partition univ qs =
  let rec add accu (t,s) =
    let t = if subtype t s then t else cap t s in
    if (subtype s t) || (is_empty t) then accu else
      let rec aux accu u =
	let c = cap u t in
	if (is_empty c) || (subtype (cap u s) t) then u::accu
	else c::(diff u t)::accu
      in
      List.fold_left aux [] accu
  in
  List.fold_left add [ univ ] qs
    
    
