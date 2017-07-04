# Client protocol
Clients  connect on a well known port to query for a service map. 

## Service map
A service map contains the services offered with the service names (for example port number the)
give service is running on. For example, 
	```
		Database : ["conn1", "conn2", "conn3"]
		WebServer : ["host1:port1", "host2:port2", "host1:port3"].
		ComputeServer : ["host10", "host11"]
	```. 

## Query and commands

### Login - Command

Support for open-id based login, with some support for customized login though this is less preferable.

```haskell

data OpenIdProvider = Google | Yahoo | Facebook | LinkedIn deriving (Show)
{- | Email needs to be validated. TODO
-}
type Email = Text 
{- | Support for login based on the email id and the open id. 
-}
data Login = Login {
    email :: Email 
    , openId :: OpenIdProvider
}

```

### AllTopics - Query
	* Returns all topics with a message count in a default page size.
	* Can return invalid authorization error.
### CreateTopic - Global
	* Creates a new topic with admin rights to the current user. The topic is by default part of a 
	global namespace.
	* Returns an error when the topic already exists or user is not authorized to creating topics

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
	* There are 2 parts to commit: 
		** Client sending a commit successfully. 
			- Failure in client sending leaves the messages uncommitted and will be sent to 
			the client upon reconnect.
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


