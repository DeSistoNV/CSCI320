(*Nick DeSisto
CSCI 320 : ML & Scheme data structures assignment
I worked on this submission alone.*)


(*BST declaration from textbook*)
datatype 'a BST = Nil | Node of 'a * 'a BST * 'a BST;

(*access functions from textbook*)
fun leftchild (Node(data,left,right)) = left
| leftchild Nil = raise Empty;
fun rightchild (Node(data,left,right)) = right
| rightchild Nil = raise Empty;
fun data (Node(data,left,right)) = data
| data Nil = raise Empty;


(*defining test trees*)
val tree = Node("horse",
Node( "cow", Nil, Node("dog",Nil,Nil) ),
Node("zebra",Node("yak",Nil,Nil), Nil));
val tree2 = Node("horse",
Node( "cow", Node("apple",Nil,Node("Banana",Nil,Nil)), Node("dog",Nil,Nil) ),
Node("zebra",Node("yak",Nil,Nil), Nil));
val tree3 = Node("horse",
Node( "cow", Node("cat",Nil,Nil), Node("dog",Nil,Nil) ),
Node("zebra",Node("yak",Nil,Nil), Nil));
val tree4 = Node("horse",
Node( "cow", Nil, Node("dog",Nil,Nil) ),
Node("zebra",Node("yak",Nil,Nil), Node("zzz",Nil,Nil)));


(*find(x,tree) returns true only if x is in tree*)
fun find(x: string, Nil) = false
 |  find(x: string, Node(data,left,right)) =
      if x = data then true
      else if x < data then find(x, left)
           else find(x, right);

(*returns number of nodes with only one child in a binary tree*)
fun parentsOfOne(Nil) = 0
 | parentsOfOne(Node(data,Nil,Nil)) = 0
 | parentsOfOne(Node(data,left,Nil))
    = 1 + parentsOfOne left
 | parentsOfOne(Node(data,Nil,right))
    = 1 + parentsOfOne right
 | parentsOfOne(Node(data,left,right))
  = parentsOfOne(left) + parentsOfOne(right);
  val tree = Node("horse",
  Node( "cow", Nil, Node("dog",Nil,Nil) ),
  Node("zebra",Node("yak",Nil,Nil), Nil));



val _ = print "testing find on : val tree = Node(\"horse\", Node( \"cow\", Nil, Node(\"dog\",Nil,Nil) ), Node(\"zebra\",Node(\"yak\",Nil,Nil), Nil)) \n";
val _ = print "find(\"horse\",tree) ---> true\n";
find ("horse", tree);
val _ = print "find(\"cow\",tree)   ---> true\n";
find ("cow", tree);
val _ = print "find(\"dog\",tree)   ---> true\n";
find ("dog", tree);
val _ = print "find(\"horse\",tree) ---> true\n";
find ("zebra", tree);
val _ = print "find(\"zebra\",tree) ---> true\n";
find ("horse", tree);
val _ = print "find(\"yak\",tree)  ---> true\n";
find ("yak", tree);

val _ = print "find(\"co\",tree)  ---> false\n";
find ("co", tree);
val _ = print "find(\"og\",tree)  ---> false\n";
find ("og", tree);
val _ = print "find(\"y\",tree)   ---> false\n";
find ("y", tree);







val _ = print "testing parentsOfOne on : val tree = Node(\"horse\", Node( \"cow\", Nil, Node(\"dog\",Nil,Nil) ), Node(\"zebra\",Node(\"yak\",Nil,Nil), Nil)) ---> 2\n";
parentsOfOne(tree);


val _ = print "testing parentsOfOne on : val tree2 = Node(\"horse\",Node( \"cow\", Node(\"apple\",Nil,Node(\"Banana\",Nil,Nil)), Node(\"dog\",Nil,Nil) ),Node(\"zebra\",Node(\"yak\",Nil,Nil), Nil)); ---> 2\n";
parentsOfOne(tree2);


val _ = print "testing parentsOfOne on :  val tree3 = Node(\"horse\",Node( \"cow\", Node(\"cat\",Nil,Nil), Node(\"dog\",Nil,Nil) ),Node(\"zebra\",Node(\"yak\",Nil,Nil), Nil)); ---> 1\n";
parentsOfOne(tree3);


val _ = print "testing parentsOfOne on : val tree4 = Node(\"horse\",Node( \"cow\", Nil, Node(\"dog\",Nil,Nil) ),Node(\"zebra\",Node(\"yak\",Nil,Nil), Node(\"zzz\",Nil,Nil))); ---> 1\n";
parentsOfOne(tree4);
