# Turn Notation

## Single Turn Notation

### Coordinates
In Torosalto's notation, a board coordinate is represented by a letter followed a number, such as: D1, F7 K10, B4. 

### Actions
 - Place: Represented by a single coordinate, such as 'E7'.
 - Free: Represented by two coordinates split by a "+" token, such as 'B9+ D3'
 - Hop: An initial coordinate describes the starting stone, and then compass direction shorthands (e.g. N, NE, E, SE, S, SW, W, NW) represent the direction of hops. A '>' token is placed between the initial coordinate and every compass direction. (e.g. 'J2>W>S>W>N').
 - Optionally, at the end of the hop sequence a '+' token followed by a placement coordinate can be used. (e.g. 'J2>W>S>W>N+D7')
 - A fourth special action is to resign from the game, which is represented by a full turn notation consisting of '!'.

## Multi Turn Notation

The series of turns made by alternating players that make up a game can be described using slash separated single turn notation fragments (e.g. 'C4/D5/B3/F5/C3>NW>NE+C6/A5+F2')

# Board Notation

A game board is represented as a series of slash separated column notation fragments, followed by a series of special signals separated by spaces representing game metadata

"3R2B3/10/8RR/3BBB1R2/5B3R/1B8/5RB3/10/4B5/10 @ 24 B 3 0 B5"
A single column: 3R2B3

## The board
The column notation represents the state of squares in that column in an ordered sequence. Numbers represent contiguous sequences of empty squares, and each R & B represent a single Red and Blue stone.

A single column: 3R2B3
-> [:empty :empty :empty :blue :blue :blue :empty :red :empty :empty]

## The Metadata

After the slash separated column fragments, metadata fragments provide information on the state of the game. Metadata is represented in this order:

1) Is this game ongoing, won, won through resignation, or a draw? e.g:
 - "@" => Active Game
 - "=" => Draw
 - "#R" => Red winner
 - "#B" => Blue winner
 - "!R" => Red Resigned
 - "!B" => Blue Resigned
2) What is the turn number
3) Number of Red Prisoners
4) Number of Blue Prisoners
5) Number of  Prisoners
