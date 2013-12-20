-- #21
insertAt :: a -> [a] -> Int -> [a]
insertAt element [] _ = [element]
insertAt element elements 0 = element : elements
insertAt element (currentElement : remainder) n
  | n < 0 = element : currentElement : remainder
  | otherwise = currentElement : (insertAt element remainder (n - 1))

-- #20
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt n list = removeAtN [] list n
  where removeAtN accumulator list 0 = (Just (head list), accumulator ++ (tail list))
        removeAtN accumulator list n
          | n >= (length list) || n < 0 = (Nothing, list)
          | otherwise = removeAtN (accumulator ++ [(head list)]) (tail list) (n - 1)

-- #19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate elements n = (slice elements n ((length elements) - n)) ++ (take n elements)

-- #18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (currentElement : remainder) 0 0 = []
slice (currentElement : remainder) 0 length = currentElement : (slice remainder 0 (length - 1))
slice (_ : remainder) start length = slice remainder (start - 1) length

-- #17
split :: [a] -> Int -> ([a],[a])
split elements 0 = ([], elements)
split elements n = splitN ([], elements) n
  where splitN :: ([a], [a]) -> Int -> ([a], [a])
        splitN (elements, []) _ = (elements, [])
        splitN (elements, remainingElements) 0 = (elements, remainingElements)
        splitN (splitElements, remainingElements) n
          | n < 0 = ([], remainingElements)
          | otherwise = splitN (splitElements ++ [head remainingElements], tail remainingElements) $ n - 1

-- #16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery inputElements n
  | n == 0 = inputElements
  | otherwise = dropEveryN n 0 inputElements
  where dropEveryN _ _ [] = []
        dropEveryN n counter (currentElement : remainder)
          | counter == n - 1 = dropEveryN n 0 remainder
          | otherwise = currentElement : (dropEveryN n (counter + 1) remainder)

-- #15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (currentElement : remainder) n = (replicate n currentElement) ++ (repli remainder n)

-- #14
dupli :: [a] -> [a]
dupli [] = []
dupli (currentElement : remainder) = currentElement : currentElement : (dupli remainder)

-- #13
encodeDirect :: (Eq a) => [a] -> [Varied a]
encodeDirect [] = []
encodeDirect a = buildEncodedList [] a
  where buildEncodedList accumulation [] = reverse accumulation -- No more elements to encode, return accumulation
        buildEncodedList [] elementsToEncode = buildEncodedList [Single (head elementsToEncode)] (tail elementsToEncode)
        buildEncodedList (Multiple number value:accumulation) elementsToEncode
          | value == currentElement = buildEncodedList (Multiple (number + 1) value : accumulation) remainder
          | otherwise = buildEncodedList (Single currentElement : Multiple number value : accumulation) remainder
          where currentElement = head elementsToEncode
                remainder = tail elementsToEncode
        buildEncodedList (Single value : accumulation) elementsToEncode
          | value == currentElement = buildEncodedList (Multiple 2 value : accumulation) remainder
          | otherwise = buildEncodedList (Single currentElement : Single value : accumulation) remainder
          where currentElement = head elementsToEncode
                remainder = tail elementsToEncode

-- #12
decodeModified :: [Varied a] -> [a]
decodeModified [] = []
decodeModified (Single currentElement : remainder) = [currentElement] ++ (decodeModified remainder)
decodeModified (Multiple number value : remainder) = (take number (repeat value)) ++ (decodeModified remainder)
            
-- #11
data Varied a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Varied a]
encodeModified [] = []
encodeModified a = map getVaried (pack a)
  where getVaried a
          | length a == 1 = Single (head a)
          | otherwise = Multiple (length a) (head a)

-- #10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode a = map getCountAndElement (pack a)
  where getCountAndElement a = ((length a), (head a))

-- #9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [a] = [[a]]
pack (currentElement:remainder) = if currentElement `elem` (head (pack remainder))
                                  then (currentElement : (head (pack remainder))) : (tail (pack remainder))
                                  else [currentElement] : (pack remainder)

-- #8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (current:next:remainder)
  | current == next = compress (next : remainder)
  | otherwise = current : compress (next : remainder)
compress (last:[]) = [last]

-- Failed, misread requirements
onlyUniques :: (Eq a) => [a] -> [a]
onlyUniques [] = []
onlyUniques (currentElement : remainder)
  | elem currentElement remainder = onlyUniques remainder
  | otherwise = currentElement : onlyUniques remainder
                                             

-- #7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem element) = [element]
flatten (List list) = foldr (++) [] $ map flatten list


-- #6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome elements = elements == (myReverse elements)

-- #5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (currentElement:remainder) = (myReverse remainder) ++ [currentElement]

-- #4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (currentElement:remainder) = 1 + myLength(remainder)

-- #3
myElementAt :: (Eq b,   Num b) => [a] -> b -> Maybe a
myElementAt [] _ = Nothing
myElementAt (currentElement : _) 0 = Just currentElement
myElementAt (_ : remainder) elementAt = myElementAt remainder (elementAt - 1)

-- #2
myButLast :: [a] -> Maybe a
myButLast (elementBeforeLast : _ : []) = Just elementBeforeLast
myButLast [] = Nothing
myButLast (_ : remainder) = myButLast remainder

-- #1
myLast :: [a] -> a
myLast (lastElement : []) = lastElement
myLast (_ : remainder) = myLast remainder
