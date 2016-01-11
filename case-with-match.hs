


caseMatch :: Show a => Maybe a -> String
caseMatch x = case x of 
  Just y -> show y
  Nothing -> "Nothing"
