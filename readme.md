# Toro Salto
*Torus Hop*

An abstract strategy game for two players.

## Physical Setup
Toro Salto is played on a regular checkerboard with even squares. The game is designed for a 10x10 board, but can be played well on an 8x8 board used for chess or checkers.

Each player has a set of pieces, which are called stones. On one side of the board, a clear space, called the prison, is reserved for stones captured during play.

## Win Condition
A player wins by being the first to connect a line of 5 of their stones in a row. A winning line can be diagonal or orthogonal. Lines end at the boundary of the board,

## The Board Wraps
In Toro Salto, the boundary edges of the gameboard connect to each and the gameboard wraps around. This means that a square on the right edge of the board will be adjacent to three squares the left edge as if the board had looped around. Similarly,top and bottom squares can be at adjacent, and following these rules, all corner squares are also adjacent to each other.

This is relevant for hopping and placing non-adjacent stones, but this does not apply to the win condition. A winning line cannot wrap around the board.

## How To Play
Players alternate turns throughout the game. On each turn, you can make one of three choices: Place your stone on an empty square, hop a stone you control over other stones, or free two of your stones from prison.

### Placing Stones
Place one stone you control on any empty square on the board that is not *blocked*. This stone can come from your reserve of unused pieces, or from the prison.

#### Open Squares
An *open* square is an empty square that is not *blocked* with no adjacent stones. Be mindful that adjacency wraps around the edges of the board, and so a seemingly *open* square along the boundary of a board may be adjacent to stones on the opposite boundary. The rules for *open* squares are relevant when placing a stone after a multi-hop or when freeing stones. 

### Hopping Stones
To perform a hop, choose a stone you control on the board, and then choose a direction where the adjacent square contains any stone, and the next square after that is empty. Hop your stone over the adjacent stone into the next empty square. Remove the captured adjacent stone and place it in the prison. 

If possible, you may choose to continue perfoming additional hops with that stone within a single turn. You can choose to move your first hop orthogonally or diagonally, however, you cannot switch between performing diagonal and orthogonal hops within a single turn. 

#### A Single Hop Blocks a Square
If your turn is composed of a single hop, then the square that was hopped over becomes *blocked* on the opposing player's immediate turn. They cannot place a stone on that square, but they can freely hop a stone into that square. This *block* only lasts for the opposing player's immediate turn.

#### Multi-Hops Place A Stone
If your turn is composed of multiple hops, then after you finish hopping you may place one new stone on an *open* square before ending your turn.

### Freeing Stones
You may take two of your stones from the prison, if available, and place them on *open* squares on the board. These two stones cannot be placed adjacent to each other.
