(*Nick Desisto
CSCI320 ML programming assignment 1*)

(*; I worked alone on this assignment.*)


exception EmptyList;
(* Declaring test variables *)
val ints1 = [~11,2,10,3,~4,5];
val ints2 = [1,2,3,4,5];

val reals1 = [~11.5,1.8,1.2,~13.0];
val reals2 = [1.2,2.3,~3.4,4.5,5.6];

val strings1 = ["a","ab","abc"];
val strings2 = ["aaa","bbb","ccc","ddd"];

val bools1 = [true, true, false];
val bools2 = [false,false,false];

(*
  1A : Function that changes negatives to zero.
    arg n : number to changes
    returns n if n is greater than 0 else 0
*)
fun no_negs(n) = if n > 0.0 then n else 0.0;

(*
  1B : Function that adds one to a number.
  arg n : number to increment
  returns n + 1
*)
fun add1(n) = n + 1;

(*
Implementation from class of reduce
  arg f : function to use
  arg [] : list to reduce
  returns a single element after the list is reduced by
  the function.
*)
fun   reduce  (f ,[]) = raise EmptyList
    | reduce  (f ,[a]) = a
    | reduce  (f, (h::t) ) = f( h ,reduce(f,t));

(* 4/5 reduces from beginning instead of end*)
fun lreduce(f,[]) = raise EmptyList
  | lreduce(f,[a]) = a
  | lreduce(f,(h::t) ) = f( reduce(f,t),h);


(* 6. Filter Function implementation *)
fun filter (f,[]) = []
  | filter (f,(h::d)) = if f(h) then h::filter(f,d) else filter(f,d);

(* 7a *)
fun greater_than_zero(x) = if x > 0 then true else false;
(* 7b *)
fun is_between_zero_and_one(x) = if x > 0 andalso x < 1 then true else false;
(* 7c *)
fun string_longer_than_3(x) = if size(x) > 3 then true else false;


(*
  Function that returns the bigger of two elements.
*)
fun bigger(x:real,y:real) = if x > y then x else y;
(*
  Function that returns the smaller of two elements.
*)
fun smaller(x:real,y:real) = if x < y then x else y;
(*
  logical or function.
*)
fun or(x:bool,y:bool) = if x orelse y then true else false;


val _ = print "1a. Replace negative elements with 0 on a list L.\n";
val _ = print "TESTING map no_negs\n----------\n";
val _ = print "map no_negs [~11.5,1.8,1.2,~13.0]  --> [0,1.8,1.2,0] : ";
map no_negs reals1;
val _ = print "\n";
val _ = print "map no_negs [1.2,2.3,~3.4,4.5,5.6] --> [1.2,2.3,0,4.5,5.6] : ";
map no_negs reals2;
val _ = print "\n";

val _ = print "1b. Add 1 to every element of a list of ints.\n"
val _ = print "TESTING map add1\n----------\n";
val _ = print "map add1 [~11,2,10,3,~4,5] --> [~10,3,11,4,~3,6] : ";
map add1 ints1;
val _ = print "\n";
val _ = print "map add1 [1,2,3,4,5]       --> [2,3,4,5,6] : ";
map add1 ints2;
val _ = print "\n";


val _ = print "1c. Return the size of each list in a list of strings.\n"
val _ = print "TESTING map size\n----------\n";
val _ = print "map size [\"a\",\"ab\",\"abc\"]          --> [1,2,3] : ";
map size strings1;
val _ = print "\n";
val _ = print "map size [\"aaa\",\"bbb\",\"ccc\",\"ddd\"] --> [3,3,3,3] : ";
map size strings2;
val _ = print "\n";


val _ = print "2a. Find the max of a list of reals";
val _ = print "TESTING reduce bigger\n----------\n";
val _ = print "reduce(bigger,[~11.5,1.8,1.2,~13.0]) --> 1.8 : ";
reduce(bigger,reals1);
val _ = print "\n";
val _ = print "reduce(bigger,[1.2,2.3,~3.4,4.5,5.6]) --> 5.6 : ";
reduce(bigger,reals2);
val _ = print "\n";


val _ = print "2b. Find the min of a list of reals\n";
val _ = print "TESTING reduce smaller\n----------\n";
val _ = print "reduce(smaller,[~11.5,1.8,1.2,~13.0]) --> ~13.0 : ";
reduce(smaller,reals1);
val _ = print "\n";
val _ = print "reduce(smaller,[1.2,2.3,~3.4,4.5,5.6]) --> ~3.4 : ";
reduce(smaller,reals2);
val _ = print "\n";

val _ = print "2c. find the logical OR of a list of booleans.";
val _ = print "TESTING reduce or\n----------\n";
val _ = print "reduce(or,[true,true,false]) --> true\n";
reduce(or,bools1);
val _ = print "\n";
val _ = print "reduce(or,[false,false,false]) --> false\n";
reduce(or,bools2);
val _ = print "\n"

val _ = print "3. What is the result of reduce (op -, L)? \n";
val _ = print "TESTING reduce op - \n----------\n";
val _ = print "reduce(op -,[1,2,3,4,5]) --> ~15\n";

reduce(op -, ints1);
val _ = print "ANSWER : The result is the negative sum of the elements of the list. \n";
lreduce(op -,ints1);
val _ = "4-5";
val _ = print "The answer from l reduce is the positive of the answer from reduce.";

val _ = print "7a. Testing filter";
val _ = print "TESTING reduce or\n----------\n";
val _ = print "reduce(or,[true,true,false]) --> true\n";
reduce(or,bools1);
val _ = print "\n";

val _ = print "Testing filter with greater than zero, 0 < x < 1, and strings longer than 3";
filter(string_longer_than_3,["hello","pizza","pie","lap","moose"]);
filter(greater_than_zero,reals1);
filter(is_between_zero_and_one,reals1);





OS.Process.exit(OS.Process.success)
