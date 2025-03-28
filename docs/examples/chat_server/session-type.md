# Chat Server Protocol

## Custom Types

```
type RoomName = string
type Port = number
```

## Global Protocol

### ChatServer

```
μ chat . {
    C -> S : LookupRoom(RoomName) . {
        S -> C : RoomPort(RoomName, Port) . chat,
        S -> C : RoomNotFound(RoomName) . chat
    },
    C -> S : CreateRoom(RoomName) . {
        S -> C : CreateRoomSuccess(RoomName) . chat,
        S -> C : RoomExists(RoomName) . chat
    },
    C -> S : ListRooms(unit) . S -> C : RoomList(RoomName[]) . chat,
    C -> S : Bye(string) . end
}
```

### C to R

```
μ loop_c . {
    C -> R : OutgoingChatMessage(string) . loop_c,
    C -> R : LeaveRoom(unit) . end
}
```

### R to C

```
μ loop_r . {
    R -> C : IncomingChatMessage(string) . loop_r,
    R -> C : Bye(unit) . end
}
```

## Local Protocols (Client)

### ChatServer

```
μ chat . +S:{
    LookupRoom(number).&S:{
        RoomPort(number, number). chat,
        RoomNotFound(number). chat
    },
    CreateRoom(number).&S:{
        CreateRoomSuccess(number). chat,
        RoomExists(number). chat
    },
    ListRooms(unit).&S:RoomList(string[]). chat,
    Bye(string). end
}
```

### C to R

```
μ loop_c . +R:{
    OutgoingChatMessage(string). loop_c,
    LeaveRoom(unit). end
}
```

### R to C

```
μ loop_r . &R:{
    IncomingChatMessage(string). loop_r,
    Bye(unit). end
}
```

## Local Protocols (Server)

### ChatServer

```
μ chat . &C:{
    LookupRoom(number).+C:{
        RoomPort(number, number). chat,
        RoomNotFound(number). chat
    },
    CreateRoom(number).+C:{
        CreateRoomSuccess(number). chat,
        RoomExists(number). chat
    },
    ListRooms(unit).+C:RoomList(string[]). chat,
    Bye(string). end
}
```

## Local Protocols (Room)

### C to R

```
μ loop_r_recv . &C:{
    OutgoingChatMessage(string). loop_r_recv,
    LeaveRoom(unit). end
}
```



### R to C

```
μ loop_r_send . +C:{
    IncomingChatMessage(string). loop_r_send,
    Bye(unit). end
}
```