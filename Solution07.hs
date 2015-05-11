module Solution07 where

data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)

isElem :: NestedList a -> Bool
isElem (Elem _) = True
isElem (List _) = False

isList :: NestedList a -> Bool
isList = not . isElem

isFlatList :: NestedList a -> Bool
isFlatList (Elem _ ) = False
isFlatList (List xs) = and $ map isElem xs

subLists :: NestedList a -> [NestedList a]
subLists (Elem _ ) = []
subLists (List xs) = filter isList xs

toNativeList :: NestedList a -> [a]
toNativeList  (Elem x ) = [x]
toNativeList  (List xs)
 | isFlatList (List xs) = map (\(Elem x) -> x) xs
 | otherwise            = error "Can't make list from deep-nested."

flatten :: NestedList a -> [a]
flatten (Elem x     ) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []    ) = []

flatten' :: NestedList a -> [a]
flatten' (Elem x ) = [x]
flatten' (List xs) = concatMap flatten' xs
