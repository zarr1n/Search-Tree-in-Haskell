--Zarrintaj Ahmadzada – 2644037
-- I read and accept the submission rules and the extra rules specified in each question. This is my own work that is done by myself only.




-- Part One: Complete Ternary Tree Builder

data TernaryTree = EmptyTree | Node Int TernaryTree TernaryTree TernaryTree deriving (Show, Eq)

--insertElement :: [Int] -> TernaryTree -> TernaryTree             --THIS IS THE TENARY TREE IMPLEMENTATION THAT I TRIED TO WRITE FROM LINE 8-19
--insertElement [] _=EmptyTree
--insertElement (x:xs) EmptyTree = Node x (insertElement(xs) EmptyTree) (insertElement(xs) EmptyTree) (insertElement(xs) EmptyTree)
--insertElement (x:xs) (Node a left middle right)=Node x (insertElement(xs) left) (insertElement(xs) middle) (insertElement(xs) right)

--countdown 0 = []  
--countdown n = n : countdown (n - 1)

--complete [] = EmptyTree
--complete (x:xs) = insertElement x (complete xs)

--completeternary n = complete (countdown n)


build i n = if i > n then EmptyTree else Node i (build (3*i - 1) n) (build (3*i) n) (build (3*i + 1) n)
completeternary n = build 1 n  






--Part Two: Tree height

completeternaryheight EmptyTree = 0   --setting empty trees height to 0
completeternaryheight (Node a left middle right) = 1 + completeternaryheight left    --since the tree is always built for the left node counting left nodes height is enough







--Part Three: Children numbers

count EmptyTree=0
count (Node a left middle right)=if left==EmptyTree then 0 else (if middle==EmptyTree then 1 else (if right==EmptyTree then 2 else 3))  --counts how many children each node has 

counter3 EmptyTree=0
counter3 (Node a left middle right)= if count (Node a left middle right)==3 then 1+counter3(left)+counter3(middle)+counter3(right)  else counter3(left)+counter3(middle)+counter3(right)   --counts how many nodes have 3 children
counter2 EmptyTree=0
counter2 (Node a left middle right)= if count (Node a left middle right)==2 then 1+counter2(left)+counter2(middle)+counter2(right)  else counter2(left)+counter2(middle)+counter2(right)   --counts how many nodes have 2 children
counter1 EmptyTree=0
counter1 (Node a left middle right)= if count (Node a left middle right)==1 then 1+counter1(left)+counter1(middle)+counter1(right)  else counter1(left)+counter1(middle)+counter1(right)   --counts how many nodes have 1 children
counter0 EmptyTree=0
counter0 (Node a left middle right)= if count (Node a left middle right)==0 then 1  else counter0(left)+counter0(middle)+counter0(right)                                                   --counts how many nodes have 0 children

childnoternary (Node a left middle right)= [(counter3 (Node a left middle right)), counter2 (Node a left middle right), counter1 (Node a left middle right), counter0 (Node a left middle right)]    --shows each counts function in a list









--Part Four: Children analysis

list3 EmptyTree=[]
list3 (Node a left middle right)= if count(Node a left middle right)==3 then [a]++list3(left)++ list3(middle)++ list3(right) else list3(left)++ list3(middle)++ list3(right)   --makes a list of nodes that have 3 children

list2 EmptyTree=[]
list2 (Node a left middle right)= if count(Node a left middle right)==2 then [a]++list2(left)++ list2(middle)++ list2(right) else list2(left)++ list2(middle)++ list2(right)   --makes a list of nodes that have 2 children

list1 EmptyTree=[]
list1 (Node a left middle right)= if count(Node a left middle right)==1 then [a]++list1(left)++ list1(middle)++ list1(right) else list1(left)++ list1(middle)++ list1(right)   --makes a list of nodes that have 1 children

list0 EmptyTree=[]
list0 (Node a left middle right)= if count(Node a left middle right)==0 then [a]++list0(left)++ list0(middle)++ list0(right) else list0(left)++ list0(middle)++ list0(right)   --makes a list of nodes that have 0 children

childlistternary (Node a left middle right)=[list3 (Node a left middle right),list2 (Node a left middle right),list1 (Node a left middle right),list0 (Node a left middle right)]   --merges all lists together