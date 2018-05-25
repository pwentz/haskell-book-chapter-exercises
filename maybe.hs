isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe def fn mb =
    case mb of
        Nothing -> def
        Just x -> fn x

fromMaybe :: a -> Maybe a -> a
fromMaybe def mb =
    case mb of
        Nothing -> def
        Just x -> x

filterMap :: [Maybe a] -> [a]
filterMap = foldr (\mb acc -> mayybe acc (: acc) mb) []
