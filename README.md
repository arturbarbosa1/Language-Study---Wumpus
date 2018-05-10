# Language-Study---Wumpus
Artur Barbosa
Timbille Kulendi

Setup:

CLI COMMANDS
1) cd filepath
2. Compile code //stack build
3. //stack exec wumpus-server
4. //stack exec wumpus-client
5. //stack exec wumpus-client
6. enjoy the game

The first client will play as the hunter while the second client will play as the wumpus. 

Objects:

>Wumpus: A beast that eats the hunter if in the same room.
>Bats: creatures that instantly carry you to a random room.
>Pits (2): fatal to you if you enter the room.

Actions:

There are two possible actions

>Move
>>to one of the three rooms connected to your current one.
>>Example Command: move 11

>Shoot
>>fire a "crooked arrow" a distance of 1-5 rooms; you must name each room it will reach.
>>Example Command: shoot 1 2 3 4 5


Hunter                        

>Warning messages: Give you information about the contents of adjacent rooms.
>>Wumpus: "I smell a wumpus"
>>Bat: "Bats nearby"
>>Pit: "I feel a draft"

Wumpus

>Wumpus has two senses - can smell the player and can hear the clatter of an arrow that misses
>Wumpus has two actions - move in some direction to an adjacent room, or sleep and stay in the current room


Features
>Game is played over a network, with each player communicating with a central server
>Turn based, server outputs whose turn it is
>20 possible rooms
>Wumpus has two senses - can smell the player and can hear the clatter of an arrow that misses
>Wumpus has just one action - move in some direction to an adjacent room


