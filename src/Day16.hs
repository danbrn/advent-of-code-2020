-- | Advent of Code 2020 - Day 16

module Day16
    ( day16
    , day16'
    )
where

import           AoC
import           Data.List.Split                ( splitOn )
import           Data.List                      ( isPrefixOf
                                                , transpose
                                                , sortOn
                                                )

import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )

day16 :: [String] -> Int
day16 input = sum $ filter (not . isValid rules) values
  where
    (rules, _, tickets) = parseInput input
    values              = concat tickets

day16' :: [String] -> Int
day16' input =
    product . map snd . filter (\(f, _) -> "departure" `isPrefixOf` f) $ zip
        (reduce $ possibilities rules tickets)
        ticket
  where
    (rules, ticket, tickets) = parseInput input
    tickets'                 = filter (allValid rules) tickets
    fields                   = zip (map fst rules) [0 .. length ticket - 1]


parseInput input = (rules, ticket, tickets)
  where
    parts   = splitOn [""] input
    rules   = map parseRule $ parts !! 0
    ticket  = parseTicket $ parts !! 1 !! 1
    tickets = map parseTicket . tail $ parts !! 2

parseTicket = map read . splitOn ","

parseRule line =
    ( fieldName
    , \x ->
        (x >= numbers !! 0 && x <= numbers !! 1)
            || (x >= numbers !! 2 && x <= numbers !! 3)
    )
  where
    ruleList  = splitOn ": " line
    fieldName = head ruleList
    numbers =
        concat . map (map (read :: String -> Int) . splitOn "-") $ splitOn
            " or "
            (ruleList !! 1)

isValid rules value = any (\(_, f) -> f value) rules

validValueForField value (_, f) = f value

validFields rules value = filter (not . null)
    $ map (\r -> if validValueForField value r then fst r else "") rules

validTicketFields rules = map (validFields rules)

allValid rules = all (isValid rules)

possibilities rules =
    map (foldr1 S.intersection . map S.fromList . validTicketFields rules)
        . transpose
        . filter (allValid rules)

reduce :: [S.Set String] -> [String]
reduce = concatMap S.elems . reduce'
  where
    reduce' :: [S.Set String] -> [S.Set String]
    reduce' ps
        | all ((== 1) . S.size) ps = ps
        | otherwise = reduce'
        $ map (\s -> if S.size s == 1 then s else s S.\\ singles) ps
      where
        singles = S.fromList . concatMap S.elems $ filter ((== 1) . S.size) ps
