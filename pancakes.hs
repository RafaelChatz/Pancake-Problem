import Data.List
import Data.Maybe
import Debug.Trace
import System.IO

data QNode = QNode {
                    qlist :: [Int],
                    qhistory :: [Int]
                   } deriving (Show)

data BQNode = BQNode {
                    bqlist :: [(Int,Int)],
                    bqhistory :: [Int]
                   } deriving (Show)

take_n :: [Int] -> Int -> [Int]
take_n s 0 = []
take_n (s:ss) l = s: take_n ss (l-1)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge (x:xs) ys = x:merge xs ys

rev :: [Int] -> [Int] -> Int -> [Int]
rev s a 0= merge a s
rev (s:ss) a n = rev ss (merge [s] a ) (n-1)

reverseLs :: [Int] -> Int -> [Int]
reverseLs s loc = rev s [] loc

maxloc :: [Int] -> Int -> Int
maxloc ss l = let s= (take_n ss l ) in (fromJust (elemIndex (maximum s) s))


expand :: QNode -> Int -> [QNode]
expand q i = if i <= length(qlist q) then
               (if (qhistory q) == [] then
                   (QNode {qlist=(reverseLs (qlist q) i),qhistory=((qhistory q) ++ [i])}):(expand q (i+1))
                else
                   (if (last (qhistory q)) /= i then
                      (QNode {qlist=(reverseLs (qlist q) i),qhistory=((qhistory q) ++ [i])}):(expand q (i+1))
                    else
                       expand q (i+1)
                   )
              )
              else
                 []


burnt_expand :: BQNode -> Int -> [BQNode]
burnt_expand q i = if i <= length(bqlist q) then
               (if (bqhistory q) == [] then
                   (BQNode {bqlist=(burnt_reverseLs (bqlist q) i),bqhistory=((bqhistory q) ++ [i])}):(burnt_expand q (i+1))
                else
                   (if (last (bqhistory q)) /= i then
                      (BQNode {bqlist=(burnt_reverseLs (bqlist q) i),bqhistory=((bqhistory q) ++ [i])}):(burnt_expand q (i+1))
                    else
                       burnt_expand q (i+1)
                   )
              )
              else
                 []

isGoalState :: [Int] -> Bool
isGoalState [] = True
isGoalState [q] = True
isGoalState (q1:q2:qs) = q1 <= q2 && isGoalState (q2:qs)

findGoalState :: [QNode] -> [Int]
findGoalState [] = []
findGoalState (z:zs) = if isGoalState (qlist z) then (qhistory z)
                       else findGoalState zs

burnt_findGoalState :: [BQNode] -> [Int]
burnt_findGoalState [] = []
burnt_findGoalState (z:zs) = if burnt_isGoalState (bqlist z) then (bqhistory z)
                       else burnt_findGoalState zs

notMember :: QNode -> [QNode] -> Bool
notMember _ [] = True
notMember x (q:qs) = ((qlist x) /= (qlist q)) && notMember x qs

burnt_notMember :: BQNode -> [BQNode] -> Bool
burnt_notMember _ [] = True
burnt_notMember x (q:qs) = ((bqlist x) /= (bqlist q)) && burnt_notMember x qs

append :: [QNode] -> [QNode] -> [QNode]
append [] _ = []
append z [] = z
append (z:zs) qs = if notMember z qs then z:(append zs qs)
                   else append zs qs

burnt_append :: [BQNode] -> [BQNode] -> [BQNode]
burnt_append [] _ = []
burnt_append z [] = z
burnt_append (z:zs) qs = if burnt_notMember z qs then z:(burnt_append zs qs)
                   else burnt_append zs qs

common2 :: QNode -> [QNode] -> [Int]
common2 _ [] = []
common2 q1 (q2:qs2) = if (qlist q1) == (qlist q2) then merge (qhistory q1) (reverse (qhistory q2))
                     else common2 q1 qs2

common :: [QNode] -> [QNode] -> [Int]
common [] _ = []
common (q1:qs1) q2 = if h == [] then common qs1 q2
                      else h
      where h = common2 q1 q2


burnt_common2 :: BQNode -> [BQNode] -> [Int]
burnt_common2 _ [] = []
burnt_common2 q1 (q2:qs2) = if (bqlist q1) == (bqlist q2) then mergeI (bqhistory q1) (reverse (bqhistory q2))
                     else burnt_common2 q1 qs2

burnt_common :: [BQNode] -> [BQNode] -> [Int]
burnt_common [] _ = []
burnt_common (q1:qs1) q2 = if h == [] then burnt_common qs1 q2
                      else h
      where h = burnt_common2 q1 q2

bid :: [QNode] -> [QNode] -> [Int]
bid (q1:qs1) (q2:qs2) = if goal == [] then (bid (qs1 ++ (append x qs1)) (qs2 ++ (append y qs2)))
                        else goal
    where
        x = expand q1 2
        y = expand q2 2
        goal = common (q1:qs1 ++ x) (q2:qs2 ++ y)


burnt_bid :: [BQNode] -> [BQNode] -> [Int]
burnt_bid (q1:qs1) (q2:qs2) = if goal == [] then (burnt_bid (qs1 ++ (burnt_append x qs1)) (qs2 ++ (burnt_append y qs2)))
                        else goal
    where
        x = burnt_expand q1 2
        y = burnt_expand q2 2
        goal = burnt_common (q1:qs1 ++ x) (q2:qs2 ++ y)

make_matching1 :: [Int] -> Int -> [(Int,Int)]
make_matching1 [] l = []
make_matching1 (s:ss) l= (s,l):(make_matching1 ss (l+1))

burnt_make_matching1 :: [(Int,Int)] -> Int -> [((Int,Int),Int)]
burnt_make_matching1 [] l = []
burnt_make_matching1 (s:ss) l= (s,l):(burnt_make_matching1 ss (l+1))

make_matching :: [Int] -> [(Int,Int)]
make_matching s = make_matching1 (sort s) 1

burnt_make_matching :: [(Int,Int)] -> [((Int,Int),Int)]
burnt_make_matching s = burnt_make_matching1 (sort s) 1

make_similar2 :: Int -> [(Int,Int)] -> Int
make_similar2 s (l:ls) = if (s==(fst l))
                         then (snd l)
                         else make_similar2 s ls

make_similar1 :: [Int] -> [(Int,Int)] -> [Int]
make_similar1 [] d = []
make_similar1 (s:ss) d = (make_similar2 s d):(make_similar1 ss d)

make_similar :: [Int] -> [Int]
make_similar s = make_similar1 s (make_matching s)

burnt_make_similar2 :: (Int,Int) -> [((Int,Int),Int)] -> (Int,Int)
burnt_make_similar2 s (l:ls) = if (s==(fst l))
                         then ((snd l),(snd(fst l)) )
                         else burnt_make_similar2 s ls

burnt_make_similar1 :: [(Int,Int)] -> [((Int,Int),Int)] -> [(Int,Int)]
burnt_make_similar1 [] d = []
burnt_make_similar1 (s:ss) d = (burnt_make_similar2 s d):(burnt_make_similar1 ss d)

burnt_make_similar :: [(Int,Int)] -> [(Int,Int)]
burnt_make_similar s = burnt_make_similar1 s (burnt_make_matching s)

make_similar_batch :: [[Int]] -> [[Int]]
make_similar_batch [] = []
make_similar_batch (s:ss) = (make_similar s):(make_similar_batch ss)

burnt_make_similar_batch :: [[(Int,Int)]] -> [[(Int,Int)]]
burnt_make_similar_batch [] = []
burnt_make_similar_batch (s:ss) = (burnt_make_similar s):(burnt_make_similar_batch ss)

bfs_s2 :: [[Int]] -> [Int] -> [([Int],[Int])]
bfs_s2 [s]  [] = [(s,[])]
bfs_s2 (s:ss) (l:ls) = (s,(l:ls)):(bfs_s2 ss ls)

bfs_s1 :: [[Int]] -> [Int] -> [([Int],[Int])]
bfs_s1 [] l = []
bfs_s1 [s] [] = [(s,[])]
bfs_s1 (s:ss) l = (s,l):(bfs_s2 ss (tail l))

bfs_s :: [Int] -> [([Int],[Int])]
bfs_s s = if(1==1)
          then bfs_s1 (visualize s d) d
          else bfs_s1 (visualize s d) d
          where d= bfs s


burnt_bfs_s2 :: [[(Int,Int)]] -> [Int] -> [([(Int,Int)],[Int])]
burnt_bfs_s2 [s]  [] = [(s,[])]
burnt_bfs_s2 (s:ss) (l:ls) = (s,(l:ls)):(burnt_bfs_s2 ss ls)

burnt_bfs_s1 :: [[(Int,Int)]] -> [Int] -> [([(Int,Int)],[Int])]
burnt_bfs_s1 [] l = []
burnt_bfs_s1 [s] [] = [(s,[])]
burnt_bfs_s1 (s:ss) l = (s,l):(burnt_bfs_s2 ss (tail l))

burnt_bfs_s :: [(Int,Int)] -> [([(Int,Int)],[Int])]
burnt_bfs_s s = if(1==1)
          then burnt_bfs_s1 (burnt_visualize s d) d
          else burnt_bfs_s1 (burnt_visualize s d) d
          where d= burnt_bfs s

combine:: [(Int,[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
combine x y = nub (x ++ y)


index_batch1 :: [[Int]] -> Int -> [(Int,[Int])]
index_batch1 [s] l = [(l,s)]
index_batch1 (s:ss) l = (l,s):(index_batch1 ss (l+1))

index_batch :: [[Int]] -> [(Int,[Int])]
index_batch [s] = [(1,s)]
index_batch (s:ss) = (1,s):(index_batch1 ss 2)

burnt_index_batch1 :: [[(Int,Int)]] -> Int -> [(Int,[(Int,Int)])]
burnt_index_batch1 [s] l = [(l,s)]
burnt_index_batch1 (s:ss) l = (l,s):(burnt_index_batch1 ss (l+1))

burnt_index_batch :: [[(Int,Int)]] -> [(Int,[(Int,Int)])]
burnt_index_batch [s] = [(1,s)]
burnt_index_batch (s:ss) = (1,s):(burnt_index_batch1 ss 2)

rem_index_batch ::  [(Int,[Int])] -> [[Int]]
rem_index_batch [s] = [snd s]
rem_index_batch (s:ss) = (snd s):(rem_index_batch ss)

reduce_batch1 :: (Int,[Int]) -> [(Int,[Int])] -> [(Int,[Int])]
reduce_batch1 s [] = [s]
reduce_batch1 s (l:ls) = if ((fst s)==(fst l))
                         then []
                         else reduce_batch1 s ls

reduce_batch :: [(Int,[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
reduce_batch [] l = []
reduce_batch (s:ss) l = ((reduce_batch1 s l)++(reduce_batch ss l))

burnt_reduce_batch1 :: (Int,[(Int,Int)]) -> [(Int,[Int])] -> [(Int,[(Int,Int)])]
burnt_reduce_batch1 s [] = [s]
burnt_reduce_batch1 s (l:ls) = if ((fst s)==(fst l))
                         then []
                         else burnt_reduce_batch1 s ls

burnt_reduce_batch :: [(Int,[(Int,Int)])] -> [(Int,[Int])] -> [(Int,[(Int,Int)])]
burnt_reduce_batch [] l = []
burnt_reduce_batch (s:ss) l = ((burnt_reduce_batch1 s l)++(burnt_reduce_batch ss l))

burnt_take_n :: [(Int,Int)] -> Int -> [(Int,Int)]
burnt_take_n s 0 = []
burnt_take_n (s:ss) l = s: burnt_take_n ss (l-1)

burnt_merge :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
burnt_merge [] ys = ys
burnt_merge (x:xs) ys = x:burnt_merge xs ys

burnt_rev :: [(Int,Int)] -> [(Int,Int)] -> Int -> [(Int,Int)]
burnt_rev s a 0= burnt_merge a s
burnt_rev (s:ss) a n = if((snd s)==0)
                 then burnt_rev ss (burnt_merge [((fst s),1)] a ) (n-1)
                 else burnt_rev ss (burnt_merge [((fst s),0)] a ) (n-1)

burnt_reverseLs :: [(Int,Int)] -> Int -> [(Int,Int)]
burnt_reverseLs s loc = burnt_rev s [] loc

ftuple :: [(Int,Int)] -> [Int]
ftuple [] = []
ftuple (s:ss) =((fst s):ftuple ss)

burnt_maxloc :: [(Int,Int)] -> Int -> Int
burnt_maxloc ss l = let s= (burnt_take_n ss l ) in ( let  d= (ftuple s) in (fromJust (elemIndex (maximum d) d) ) )

burnt_getn :: [(Int,Int)] -> Int -> (Int,Int)
burnt_getn (s:ss) 1 = s
burnt_getn (s:ss) n = burnt_getn ss (n-1)

burnt_isGoalState :: [(Int,Int)] -> Bool
burnt_isGoalState (s1:ss) =if((length ss)>1)
                then if((fst s1)<= (fst (head ss)) )
                    then if ((snd s1)==0)
                         then burnt_isGoalState ss
                         else False
                    else False
                else if((fst s1)<=(fst (head ss)) )
                     then if ((snd s1)==0)
                          then if ((snd (head ss) ) ==0)
                                then True
                                else False
                          else False
                     else False

takefst :: [(Int,Int)] -> [Int]
takefst [] = []
takefst [a] = [(fst a )]
takefst (a:as) = (fst a ):(takefst as)

sortT1 :: [Int] -> [(Int,Int)]
sortT1 [] = []
sortT1 [s] = [(s,0)]
sortT1 (s:ss) = (s,0):(sortT1 ss)

sortT :: [(Int,Int)] -> [(Int,Int)]
sortT s = sortT1 (sort (takefst s))


mergeI :: [Int] -> [Int] -> [Int]
mergeI [] ys = ys
mergeI (x:xs) ys = x:mergeI xs ys

visualize1 :: [Int] -> [Int] -> [[Int]]
visualize1 a [] = []
visualize1 a (s:ss) = (d):(visualize1 d ss)
                     where d= (reverseLs a s)


burnt_visualize1 :: [(Int,Int)] -> [Int] -> [[(Int,Int)]]
burnt_visualize1 a [] = []
burnt_visualize1 a (s:ss) = (d):(burnt_visualize1 d ss)
                     where d= (burnt_reverseLs a s)
-------------------------------------------------------------------------------------------
---naive
naive1 :: [Int] -> [Int] -> Int -> [Int]
naive1 [a] q l = []
naive1 s q 0 = q
naive1 (s:ss) q l = if (isGoalState (s:ss)==False)
                    then (
                            let d=maxloc (s:ss) l in (  if (d==0)
                                                        then naive1 (reverseLs (s:ss) l) (q ++ [l]) (l-1)
                                                        else if ((d+1)==l)
                                                        then naive1 (s:ss) q (l-1)
                                                        else naive1 (reverseLs (reverseLs (s:ss) (d+1)) l) ((q ++ [d+1]) ++ [l]) (l-1)
                                                     )
                         )
                    else q

naive :: [Int] -> [Int]
naive s = naive1 s [] (length s)
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---bfs
bfs1 :: [QNode] -> [Int]
bfs1 (q:qs) = if (goal == []) then bfs1 (qs ++ (append z qs))
              else goal
    where z = expand q 2
          goal = findGoalState z


bfs :: [Int] -> [Int]
bfs list = if isGoalState list then []
           else bfs1 [(QNode {qlist=list,qhistory=[]})]

-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---bidirectional
bidirectional :: [Int] -> [Int]
bidirectional list = if sl == list then []
                     else bid [(QNode {qlist=list,qhistory=[]})] [(QNode {qlist=sl,qhistory=[]})]
        where sl = sort list
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---batch
batch3 :: [(Int,[Int])] -> [([Int],[Int])] -> [([Int],[Int])] -> [(Int,[Int])]
batch3 [] _ _ = []
batch3 (s:ss) []  dd = batch3 ss dd dd
batch3 (s:ss) (d:ds) dd = if ((snd s) == (fst d))
                  then ((fst s),(snd d)):(batch3 ss dd dd)
                  else  (batch3 (s:ss) ds dd)

batch2 :: [(Int,[Int])] -> [([Int],[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
batch2 s d w =  (w++q)
                where q=(batch3 s d d)

batch1 :: [(Int,[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
batch1 []  q = q
batch1 (s:ss) q =  batch1 p r1
                where d=bfs_s (snd s)
                      r=(q ++ [( (fst s),(snd (head d)) )] )
                      b=batch2 ss d r
                      p= reduce_batch (s:ss) b
                      r1 = combine r b


batch :: [[Int]] -> [[Int]]
batch s = rem_index_batch (sort(batch1 (index_batch (make_similar_batch s))  []))
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---burnt_naive
burnt_naive1 :: [(Int,Int)] -> [Int] -> Int -> [Int]
burnt_naive1 [a] q l = []
burnt_naive1 s q 0 = q
burnt_naive1 (s:ss) q l = if (burnt_isGoalState (s:ss)==False)
                    then (
                            let d=burnt_maxloc (s:ss) l in (
                                                        if (d==0)
                                                        then if ( (snd s)==0 )
                                                             then burnt_naive1 (burnt_reverseLs ( burnt_reverseLs (s:ss) 1 ) l) ((q ++ [1]) ++ [l]) (l-1)
                                                             else burnt_naive1 (burnt_reverseLs (s:ss) l) (q ++ [l]) (l-1)
                                                        else if ((d+1)==l)
                                                             then if( (snd (burnt_getn (s:ss) l))==0 )
                                                                  then burnt_naive1 (s:ss) q (l-1)
                                                                  else burnt_naive1 (burnt_reverseLs ( burnt_reverseLs ( burnt_reverseLs (s:ss) l ) 1 ) l) (((q ++ [l]) ++ [1]) ++ [l]) (l-1)
                                                        else (
                                                                if( (snd (burnt_getn (s:ss) (d+1)))==0 )
                                                                then burnt_naive1 (burnt_reverseLs (burnt_reverseLs (s:ss) (d+1)) l) ((q ++ [d+1]) ++ [l]) (l-1)
                                                                else burnt_naive1 (burnt_reverseLs ( burnt_reverseLs ( burnt_reverseLs (s:ss) (d+1) ) 1) l) (((q ++ [d+1]) ++ [1]) ++ [l]) (l-1)

                                                             )
                                                     )
                         )
                    else q


burnt_naive :: [(Int,Int)] -> [Int]
burnt_naive s = burnt_naive1 s [] (length s)
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---burnt_bfs
burnt_bfs1 :: [BQNode] -> [Int]
burnt_bfs1 (q:qs) = if (goal == []) then burnt_bfs1 (qs ++ (burnt_append z qs))
              else goal
    where z = burnt_expand q 1
          goal = burnt_findGoalState z


burnt_bfs :: [(Int,Int)] -> [Int]
burnt_bfs list = if burnt_isGoalState list then []
           else burnt_bfs1 [(BQNode {bqlist=list,bqhistory=[]})]
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
--- burnt_bidirectional
burnt_bidirectional :: [(Int,Int)] -> [Int]
burnt_bidirectional list = if sl == list then []
                     else burnt_bid [(BQNode {bqlist=list,bqhistory=[]})] [(BQNode {bqlist=sl,bqhistory=[]})]
        where sl = sortT list
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---batch

burnt_batch3 :: [(Int,[(Int,Int)])] -> [([(Int,Int)],[Int])] -> [([(Int,Int)],[Int])] -> [(Int,[Int])]
burnt_batch3 [] _ _ = []
burnt_batch3 (s:ss) []  dd = burnt_batch3 ss dd dd
burnt_batch3 (s:ss) (d:ds) dd = if ((snd s) == (fst d))
                  then ((fst s),(snd d)):(burnt_batch3 ss dd dd)
                  else  (burnt_batch3 (s:ss) ds dd)

burnt_batch2 :: [(Int,[(Int,Int)])] -> [([(Int,Int)],[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
burnt_batch2 s d w =  (w++q)
                where q=(burnt_batch3 s d d)

burnt_batch1 :: [(Int,[(Int,Int)])] -> [(Int,[Int])] -> [(Int,[Int])]
burnt_batch1 []  q = q
burnt_batch1 (s:ss) q =  burnt_batch1 p r1
                where d=burnt_bfs_s (snd s)
                      r=(q ++ [( (fst s),(snd (head d)) )] )
                      b=burnt_batch2 ss d r
                      p= burnt_reduce_batch (s:ss) b
                      r1 = combine r b


burnt_batch :: [[(Int,Int)]] -> [[Int]]
burnt_batch s = rem_index_batch (sort(burnt_batch1 (burnt_index_batch (burnt_make_similar_batch s))  []))
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---visualize
visualize :: [Int] -> [Int] -> [[Int]]
visualize a (s:ss) = (a):(visualize1 a (s:ss))
-------------------------------------------------------------------------------------------
--
--
--
-------------------------------------------------------------------------------------------
---burnt_visualize
burnt_visualize :: [(Int,Int)] -> [Int] -> [[(Int,Int)]]
burnt_visualize a (s:ss) = (a):(burnt_visualize1 a (s:ss))
-------------------------------------------------------------------------------------------
