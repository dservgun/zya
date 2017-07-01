# Client protocol
Clients  connect on a well known port to query for a service map. 

## Service map
A service map contains the services offered with the service names (for example port number the)
give service is running on. For example, ``` WebServer : ["host1:port1", "host2:port2", "host1:port3"].```. 
Each client will need a handshake to reach to the appropriate service.
