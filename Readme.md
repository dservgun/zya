# Synopsis

A realtime webserver allowing users to store and forward custom messages. 

## Message protocol
All messages are saved and retrieved using json so writing clients can be simpler. We are not aiming
for greater performance, just sufficient so that most applications can start using this service without having to resort to hard to maintain clients.

## Features 
Peer-to-peer discovery
Subscription model : clients subscribe on a websocket connection.

## API
module Data.Zya.Types where

data Subscription = 
	Subscription {
		topic :: Topic
		reader :: Reader
		writer :: Writer
}

type ReaderLocation = Integer -- for larger files

data WriteAudit = WriteAudit {
	writeLocation :: Location
	lastWrite :: Timestamp
	writer :: Writer -- which subscription wrote last 

}
data ReaderAudit = ReaderAudit {
	messages :: Message Pages
	pageSize :: PageSize
	lastAccess :: Timestamp

}
data Reader = Reader {
	reader :: Stream
	position :: ReaderLocation
	audit :: Audit
}






## Libraries

### Yesod 
### CloudHaskell

## Databases

### Postgres