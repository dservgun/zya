
data TestCSV = TestCSV {a :: String, b :: String} deriving (Generic, Show)

instance CSVFormatter [Char] where 
  output aString = Set.fromList [Value . pack $ aString]


instance CSVFormatter TestCSV

