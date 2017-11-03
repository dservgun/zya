Roadmap
=============



*  Back-pressure.
*  Websocket clients using Elm, Qt or purescript.
*  Integrate blockstack with messsages that need to be audited.
*  Topic and consumption offsets for clients.
*  Oauth 2.0 based access layer.


Client Protocol
=======================

```
--------------Application types ---

data OpenIdProvider = Google | Facebook | LinkedIn deriving (Show, Typeable, Generic, ToJSON, FromJSON)
{- | Email needs to be validated. TODO
-}
type Email = Text
{- | Support for login based on the email id and the open id.
-}



newtype Device = Device {_undevice :: Text} deriving(Show, Generic, ToJSON, FromJSON)
newtype UserName = UserName {_unUserName :: Text} deriving (Show, Generic, ToJSON, FromJSON)

-- The message id is unique among all the processes.
type MessageId = Text


{--
  We use time stamp despite as a way to present some form of ordering. The values are at
  best approximate.
--}
data LocalMessage =
    Login{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Logout {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime}
  | Session {_kind :: Text, userName :: UserName , device :: Device, _timestamp :: UTCTime, _topics :: [Topic]}
  | Topics {_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime, topics :: [Topic]}
  | Publish{_kind :: Text, _userName :: UserName, _device :: Device, _timestamp :: UTCTime
                , topic :: Topic, messageId :: Text, messaggePayload :: Text}
  | Commit {_kind :: Text, userName :: UserName, device :: Device, topic :: Topic, messageId :: MessageId, timestamp :: UTCTime}
    deriving(Generic, ToJSON, FromJSON, Show)


```




### AllTopics - Query
  * Returns all topics with a message count and a default page size per topic.
  * Can return invalid authorization error.

### TopicAllocator - Global
  * A service that routes a CreateTopic request. Checks the load of topics on each processId and selects one of the processIds and sends the selected ``` CreateTopic ``` request.
  ** This will start a local timeout waiting for an acknowledgment from the processid.
  ** A timeout or an error will result in a round robin allocation process.

### CreateTopic - Global
  * Creates a new topic with admin rights to the current user. The topic is by default part of a
  global namespace.
  * Returns an error when the topic already exists or user is not authorized to creating topics
  ** Failure/tolerance

### CreateTopic - Local
  * Creates a new topic local to the current user with admin rights.
  * Returns an error if the topic exists in the user's local space.

### Delete a topic - Global
  * Delete a topic if the user has admin rights to delete a topic.
  * Returns an error if either the topic does not exist or user is not allowed to delete topics.

###  Subscribe to a topic - Global
  * Subscribe to a topic with an offset request.
```haskell

type Start = Integer
type End = Integer
data OffsetHint = Beginning | Latest | MessageRange of (Start , End)
data Subscribe =
  Subscribe {
    topic :: Topic
    user :: User
    , reader :: OffsetHint
}

```
  ** Successful subscription sends a batch of messages about the size of the batch size. New messages are sent to the client upon receiving commit for a percentage of messages. A poorman's flow control.

### Subscribe to a topic - Local
  Same as the global subscription within the user space.

### Commit
  * Request to commit message(s), usually a batch size.
  * There are 2 cases to commit:
    ** Client sending a commit successfully.
      - Failure in client sending leaves the messages uncommitted and will be re-sent to
      the client upon reconnect. This implies that the client has to be able to test
    ** The two phase commit.
      - Need to handle this gracefully.

### Handshake
  * Query the service map to find the list of hosts and ports to connect to each of the service
  * Disconnect from the global service and reconnect to one of the servers from the list.

### ServiceBackupMap
  * A list of backups for each topic. These backups are mostly in standby mode.

### Write messages to a topic - global
  * Write a set of messages for a topic.
  * Successful write involves a copy to each of the backup topic services or at least 3 backup services.
  * Each request contains a client request time stamp and a timeout. The client uses this to invalidate the commit. The server cant do much with the timestamp.

### Rollback -- TODO
  * Intent is to rollback messages as a result of a timeout.
  * One possible implementation is to maintain gen0 and gen1 pages for a partition and
  search for rollback in gen0. If we dont find it, we return an error for rollback.
  * Periodically merge gen0 with gen1 to make room from new messages.


--- Whats the application flow?
. Writers write to the database.
. Subscribers when available, come up and begin to listen on a topic.
. Readers when available, read messages since the last committed read. If the subscriber
  opts for uncommitted reads, read from the last n messages before the reader begins processing.

