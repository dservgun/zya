Roadmap
=============



*  Back-pressure.
*  Websocket clients using Elm, Qt or purescript.
*  Integrate blockstack with messsages that need to be audited.
*  Topic and consumption offsets for clients.
*  Oauth 2.0 based access layer.


Client Protocol
=======================

* ``` data Login = Login Identifier IdentifierType
      data IdentifierType  =
          OAuth Version
          | CustomValidation Version
      data Version = (Int, Int, Int, Int)
      data Device = Text
      data LoginRequest = LoginRequest {login :: Login, device :: Device}
      type Reason = Text
      data AuthenticationStatus = Validated Reason  | NewUser Reason | AccessDenied Reason
      data LoginResponse = LoginResponse {login :: Login, authenticationStatus :: AuthenticationStatus}
      data RequestMessage = MessageRequest {login :: Login, topic :: Topic , n :: Int} -- The last n messages for the topic.
      -- Returns all the topics available to the user.
      data AllTopics = AllTopics  {login :: Login , topics :: [Topic]}
      data CommitMessage = CommitMessage {login :: Login, topic :: Topic, messageId :: MessageId}
      data BeginSession = BeginSession {login :: Login, topic :: Topic}
      data EndSession = EndSession {login :: Login, topic :: Topic}
      data MessageResponse = MessageResponse {login :: Loogin, topic :: Topic, messageId :: [MessageId], messagesOutstanding :: Int}
      data MessageEvent = MessageEvent {topic :: Topic, messageId :: MessageId, version :: Int}

  ```