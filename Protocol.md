# Client protocol
Clients  connect on a well known port to query for a service map.

## Service map
A service map maintains the services offered with the service names the port number the
given service is running on. For example,
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

