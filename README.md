Chatroom
====

## How to start server and connect

```
cabal run
```

and Chatroom server should be running on port 4000. Use a `telnet` friendly client (e.g. `nc`) to
connect

```
nc localhost 4000
```

## Available commands

- :login USERNAME
- :create ROOMNAME
- :join ROOMNAME
- :listrooms
- Anything else will be broadcasted to the room which you have joined
