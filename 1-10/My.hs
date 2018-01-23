myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt l i = 
    elementAt' l i 1
    where 
        elementAt' (x:xs) i currI = 
            if i == currI
                then x
                else elementAt' xs i (currI + 1)

elementAt' (x:_) 1 = x
elementAt' (_:xs) i = elementAt' xs (i-1)

myLength [] = 0
myLength (_:xs) = myLength xs + 1

myReverse [] = []
myReverse (x:xs) = xs ++ [x]

isPalindromeNaive [] = True
isPalindromeNaive [_] = True
isPalindromeNaive list = 
    (head list) == (last list) && (isPalindromeNaive $ tail . init $ list)

data NestedList a  = Elem a | List [NestedList a] deriving(Show, Read)

flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


compress [] = []
compress [x] = [x]
compress [x, y] = if x == y then [x] else [x, y]
compress (x:y:xs)
    | x == y = compress(x:compress xs)
    | otherwise = (x:compress(y:compress xs))


compress' (x:ys@(y:_))
    | x == y = compress' ys
    | otherwise = x : compress' ys
compress' l = l

myPack li@(x:xs) = reverse $ foldl fun [[x]] xs
    where fun acc@(y@(z:zs):ys) el
            | z == el = (el:y) : ys
            | otherwise = [el] : acc

encode list = map pack' $ myPack list
        where pack' li = (length li, head li)