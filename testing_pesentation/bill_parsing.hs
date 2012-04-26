import Control.Monad.Error
import Test.QuickCheck
import System.Time
import Control.Applicative
import System.Locale

data Bill = Invoice {issueDate :: String, number :: String} 
            | BIF {issueDate :: String, number :: String} 
            | BEF {ban :: String, number :: String}
            deriving Show

-- Property: parsing a file name must equal formatting the corresponding bill
--prop_parse_is_reversible :: String -> Property
--prop_parse_is_reversible fileName = forAll validFileNames $ (\fileName -> (toFileName . parseFileName) $ fileName == fileName)

newtype DateString = DateString {value :: String}

instance Show DateString where
  show (DateString value) = show value

instance Arbitrary DateString where
  arbitrary = (DateString . formatCalendarTime defaultTimeLocale "%Y%m%d" . toUTCTime) <$> (TOD <$> choose (1300000000,1900000000) <*> pure 0)
  
