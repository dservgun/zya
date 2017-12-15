High level requirement
=====================

* EthZyear is a remote service that does the following
  ** Spins up a shell with a (g)seth over a domain socket.
  ** Waits for the node to come up, which should be quick and connect to the server.
  ** Process incoming requests after assigning an ethzyear session.
      *** The session will contain a unique key to generate a reversable hash for the user.
      *** This key will be used to create a session key that will be stored in one of the many databases
        for future access.
      *** When the user is using sensitive commands that require the users password, the hash will
      be used to create a password that gets used to create the account.
  ** Each node will have an effective time to balance the startup time and the run time but also to not keep 
  the node running for too long to expose the domain sockets.
  ** The domain socket is created with rw_____ for the user.