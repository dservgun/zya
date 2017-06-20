module Data.Zya.Core.Types where
import Data.Text

type Topic = Text 

data Subscription = 
  Subscription {
    topic :: Topic
    , reader :: Reader
    , writer :: Writer
}

type ReaderLocation = Integer -- for larger files

data WriteAudit = WriteAudit {
  writeLocation :: Location
  , lastWrite :: Timestamp
  , subscription :: Subscription

}
data ReaderAudit = ReaderAudit {
  messages :: Message Pages
  , pageSize :: PageSize
  , lastAccess :: Timestamp

}
data Reader = Reader {
  readStream :: Stream
  , position :: ReaderLocation
  , audit :: Audit
}
data Writer = Writer {
    writeStream :: Stream
    , writePosition :: WriterLocation
    , waudit :: WriteAudit

}

