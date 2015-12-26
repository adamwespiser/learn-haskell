-- Types and Programming Languages
-- Chapter 5: Untyped Lambda Calculus
--
tru :: a -> a -> a
tru = \t -> \f -> t
fls :: a -> a -> a
fls = \t -> \f -> f
test = \l -> \m -> \n -> l m n
and' = \b -> \c -> b c fls
not' = \b -> \t -> \f -> b f t

-- test tru v w
-- (\l -> \m -> \n -> l m n) tru v w
-- (\m -> \n -> tru m n) v w
-- (\n -> tru v n) w
-- (tru v w)
-- (\t -> \f -> t) v w
-- (\f -> v) w 
-- v
--
-- and tru tru
-- (\b -> \c -> b c fls) tru tru
-- (\c -> tru c fls) tru
-- (tru tru fls)
-- (\t -> \f -> t) tru fls
-- (\f -> tru) fls
-- tru
--
-- not tru
-- eq. to (\t -> \f -> f)
--
-- not = (\b -> \t -> \f -> b f t)
-- not tru
-- (\b -> \t -> \f -> b f t) tru
-- (\t -> \f -> tru f t)
-- or == (\p -> \q -> p p q)

churchToHask :: (Bool -> Bool -> Bool) -> Bool
churchToHask b = b True False

haskToChurch :: Bool -> (a -> a -> a)
haskToChurch b = if b then tru else fls


