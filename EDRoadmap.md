### Roadmap

#### Demo
The demo needs to allow scaling remote execution of commands. Remote
execution of commands means the following, we need to pare down the 
requirements as needed (priority : higher priority means just that)
  * Each command is first saved in the database with state. - 1 day + 10
  * Command is routed to the available compute node (assuming that a command is running on a single node). As in the same command can be split into multiple commands and saved to the database queue. - 1 day + 40
  * Create a test client (no ui)  - 1 day + 40 
  * Create a websockets ui : elm perhaps to interact with the cloud.5 days + 70
  * Create a libreoffice visualization interface using native websockets similar to elm ui.   4 days + 70
  * Global history of all commands is available to anyclient. 1 day + 5
  * Sockets/websockets based upload application to allow for large file uploads (no limits)
  * Tezos smart contracts to manage data upload with a user negotiated key ring for storing
    local data to the cloud. Most costs originate from these interactions as the servers may be able to communicate on hpc computing networks as mentioned below therefore will save the costs of the network overhead in lieu of reserved instances and networks. 

#### Code refactoring
Improve design and refactor the code to keep it maintainable.

#### Elastic

Estimate the amount of cpu needed and spin up instances as needed.

#### Status reporting

Monitor progress and liveness of the system, integrate with event management tools 
such as grafana (not splunk) or kibana. Setup a logging cloud service that publishes 
messages to the cloud.

#### Sizing data

Connect and install file systems such as lustre or ceph through the cloud. The cloud
should be able to spin up a ceph or lustre cluster using their configuration formats or 
define one of our own. Mount hdf5 or similar file system to handle large image data, dna 
as well as statistical data.

#### Sizing network

Enable cci interfaces between cloud components. 1 - 20 days depending on whether the
interface binding is available or not.  +  40

#### Utilities to cost the bandwidth usage and limits

Write utilities to manage costs of bandwidth usage so that all best practices are 
monitored.

#### Security audit

Use TLS and the cutting edge ssl libraries such as HACL. + 40 - 4 days. 

#### Implement supervisory services and nodes
This is built into cloud haskell, but i still need to call it so that 
a service can restart or be moved closer to a data node for example.

#### Use criterion to measure performance.
Another haskell library that can help run micro-benchmarks as in, with a sensitivity
of about pico seconds.

#### All services within the cloud, will use binary format if applicable
JSON or most text formats are expensive and compression is not secure, we will not be using any compression of data (add reference here). Replace JSON with Binary wherever the 
messages are interacting with the cloud.

#### Webserver/service architecture
Elm clients will connect to the cloud using websockets api and this should be the only 
slower part of the network, though this is still a heavy volume interface. As mentioned 
elsewhere, varying network speeds will require some buffering/retries from websockets clients to allow for network interrupts as well as to provide a "no stopping" perception
to the user.

#### Allow users to create video conferences through the cloud. 
This is not a particularly important feature for the demo, perhaps; it is conceivable
that during clinical data uploads, users can setup a video conference and the video 
from the conference gets added to the lab data for the patient. To prevent this data
from being inter-mingled with statistical and big data, these files will shared among the
members of the conference. 

#### Visualization software
As mentioned above, use libreoffice and elm to present data to the user as each user will 
have different form factors.
