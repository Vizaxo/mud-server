# README

This project is a server for a [MUD](https://en.wikipedia.org/wiki/MUD) designed to be played over Telnet.

It's written in Haskell, based around the FRP library [reactive-banana](https://hackage.haskell.org/package/reactive-banana-1.2.1.0).

## Building and running

This project is built with Stack:

```
stack build
```

Run the server binary, with an optional port number. The default port is 8080.

```
stack exec server -- [$port]
```

Connect to the server with telnet. The `$port` to connect to is the port that was specified above. If running on a local computer, localhost can be used as the `$server-ip`.

```
telnet $server-ip $port
```

You will then be presented with a login message, and can begin playing the game

```
Welcome to the MUD!
What is your name?
```

## Code hierachy

### /lib

/lib contains the majority of the code for the game.
/lib/Client.hs is a set of helper functions for managing the clients connected to the game
/lib/Combat.hs handles combat between entities
/lib/Commands.hs contains the datatype defining the available commands
/lib/ConsoleControl.hs manages console input and output
/lib/Event.hs contains datatypes defining events for interacting with the client
/lib/FRP.hs is the main game file, setting up and running the FRP network
/lib/GameState.hs defines the persistent state of the game, and provides some functions for interacting with it
/lib/Mud.hs defines most of the game logic
/lib/Networking.hs defines a slightly higher-level networking interface than exposed by the `networking` library
/lib/Parser.hs is the parser for player input
/lib/Player.hs defines the datatype of players
/lib/Stats.hs defines the datatype of stats, and a few helper functions
/lib/Utils.hs contains generic helper functions
/lib/World.hs defines the game world

### /src

/src/Main.hs is the executable code. This is just a wrapper around `runServer` from /lib/FRP.hs, which gets the port from the command-line argument (or uses 8080 as a default if none was provided).
