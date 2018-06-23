### Roadmap

#### Demo
The demo needs to allow scaling remote execution of commands. Remote
execution of commands means the following, we need to pare down the 
requirements as needed (priority : higher priority means just that)
  * Each command is first saved in the database with state. - 1 day + 10
  * Command is routed to the available compute node (assuming that a command is running on a single node). As in the same command can be split into multiple commands and saved to the database queue. - 1 day + 40
  * Create a test client (no ui)  - 1 day + 40 
  * Create a websockets ui : elm perhaps to interact with the cloud.5 days + 70
  * Global history of all commands is available to anyclient. 1 day + 5
  * 

#### Code refactoring

#### Elastic

#### Status reporting

#### Sizing data