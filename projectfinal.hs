import Prelude hiding (head,tail,(++))  

data Name = I String
			|U String
			deriving (Show,Eq) 

data Rating a= NoRating  
             | R a 
              deriving(Show,Eq)
		 	
dis::Eq a=>[a]->[a]
dis []=[]
dis (x:xs)=if(find x xs) then dis xs else x:dis xs 
find e [] =False
find e (x:xs) = if e==x then True
		else find e xs



fromRatingsToItems :: Eq a => [(b,a,c)]->[a]
fromRatingsToItems []= []
fromRatingsToItems a= dis (getlist a )
getlist [] = []	
getlist ((a,b,c):xs)= b:getlist xs


fromRatingsToUsers :: Eq a=> [(a,b,c)]->[a]
getuser []=[]
getuser ((a,b,c):xs)= a:getuser xs
fromRatingsToUsers [] = []
fromRatingsToUsers a =
	dis(getuser a)

hasRating :: (Eq a ,Eq b)=>a->b->[(a,b,c)]->Bool
hasRating x y xs = didrate x y xs
didrate a b []=False
didrate a b ((x,y,z):xs) =if (a==x && b==y ) then True else 
		didrate a b xs	



getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating x y xs =if(hasRating x y xs)==False then error "No given rating"
	else didrate2 x y xs

didrate2 a b ((x,y,z):xs) =if (a==x && b==y ) then z else 
		didrate2 a b xs

		
formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]

formMatrixUser x [] li =[]
formMatrixUser x (h:t) li=if(hasRating x h li == True) then((R (getRating x h li )) : formMatrixUser x t li)
		else (NoRating:(formMatrixUser x t li)) 
		
		
formMatrix :: (Eq a ,Eq b ,Fractional c)=>[b]->[a]->[(b,a,c)]->[[Rating c]]		
formMatrix [] z li = []
formMatrix	(x:xs) z li =(formMatrixUser x z li :formMatrix xs z li)	




numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem n [] =0
numberRatingsGivenItem n (h:t)=if( h!!n ==NoRating) then 0+numberRatingsGivenItem n t 
	else 1+numberRatingsGivenItem n t
	
	
differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings a NoRating =0.0
differeneRatings NoRating a=0.0
differeneRatings (R r) (R r1) =(r-r1) 


matrixPairs :: (Num a,Ord a)=> a -> [(a,a)]
matrixPairs n =helper4 0 n
helper4 c n=if(c<n) then pp (helper3 c 0 n)(helper4 (c+1) n)
	else [] 
helper3 n1 n2 n3=if(n2<n3) then ((n1,n2):(helper3 n1 (n2+1) n3))
	else []
pp [] [] = []
pp [] (h:t) = h:pp [] t
pp (h:t) [] = h:pp t []
pp (h:t) (a:b) = h : pp t (a:b)


dMatrix :: Fractional a => [[Rating a]] -> [a]
diff (x,y) l=(differeneRatings (l!!x)(l!!y))
add1 y []=0
add1 y (x:xs)=(diff y x)+ (add1 y xs) 
loop [] l=[]
loop (x:xs) l=((add1 x l):(loop xs l))
dMatrix l = (loop (matrixPairs (length(l!!0))) l)

freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
helper5 (x,y) l=if((l!!x)/=NoRating && (l!!y)/=NoRating) then 1
		else 0
helper6 x []=0
helper6 x (y:ys)=helper5 x y + helper6 x ys 
helper7 [] l=[]
helper7 (x:xs) l2=helper6 x l2 : helper7 xs l2 
freqMatrix l=(helper7 (matrixPairs (length(l!!0))) l)
diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]

div1 [] []=[]
div1 (x:xs) (y:ys)=((x/y):(div1 xs ys))
diffFreqMatrix l= (div1 (dMatrix l)(freqMatrix l)) 


helper [] [] n=[]
helper (y:ys) ((x,xx):xs) n =if(x==n&&(x/=xx))then (y:(helper ys xs n)) else (helper ys xs n)

user l n =(fromRatingsToUsers l)!!n
remove []=[]
remove (x:xs)=if(x==NoRating) then xs else x:(remove xs)

add2 [] []=[]
add2 (x:xs) ((R y):ys)= ((x+y):(add2 xs ys)) 

add3 l=foldr (+) 0 l 
predict l i it =add3 (add2 (helper (diffFreqMatrix (formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l))  (matrixPairs (length (fromRatingsToItems l))) it) (remove (formMatrixUser (user l i)  (fromRatingsToItems l) l )))/length1 (add2 (helper (diffFreqMatrix (formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l))  (matrixPairs (length (fromRatingsToItems l))) it) (remove (formMatrixUser (user l i)  (fromRatingsToItems l) l )))
 
length1 []=0
length1 (x:xs)= 1+length1 xs
