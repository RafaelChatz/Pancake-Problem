# Pancake-Problem

Simple sorting
===================

**naive :: [Int] -> [Int]** :

Simple and fast pancake sorting.


Minimum number of moves
============

**bfs ::  [Int] -> [Int]** : 

Bfs is being used to find the shortest path.


Bidirectional search
=================

**bidirectional ::  [Int] -> [Int]**  :

Two stacks are used to find a solution faster. The first starts from the begining and the               
second from the sorted list.When we find the same list from both stacks we merge the path lists.

Many pancake lists
============

**batch ::  [[Int]] -> [[Int]]** : 

We solve many lists by finding similarities from solved lists using bfs.The first one is always solved using bfs.Then we find lists that need similar paths to be solved.We solve another list with bfs and repeat the process.

Burned pancakes
============

All the algorithms mentioned above are being used to sort burned pancakes.

**1.burnt_naive :: [[(Int,Int)]] -> [Int]**

**2.burnt_visualize :: [[(Int,Int)]] -> [Int]**

**3.burnt_bfs :: [[(Int,Int)]] -> [Int]**

**4.burnt_bidirectional :: [[(Int,Int)]] -> [Int]**

**5.burnt_batch :: [[(Int,Int)]] -> [Int]**
