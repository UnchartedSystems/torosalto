# Toro Salto
*Torus Hop* ◦ An abstract strategy game for two players.

## This Repository

The purpose of this project is to create a correspondence web app, a cli app, an AI harness, and an AI benchmark for ToroSalto.

## Notation

`torosalto.notation` converts notation to data and data to canonical notation:

| Parse | Format | Value |
| --- | --- | --- |
| `parse-turn` | `format-turn` | One turn map |
| `parse-turns` | `format-turns` | A sequence of turns; parsing returns a vector |
| `parse-game` | `format-game` | A board snapshot |

Turn notation uses one-based squares from `A1` through `Z26`:

- Place: `C10`
- Free: `C10+E5`
- Hop: `C10>NW>SW`
- Hop and place: `C10>NW>SW+E5`
- Turn sequence: `C4/D5/B3/F5/C3>NW>NE+C6/A5+F2`

Directions are `NW`, `N`, `NE`, `W`, `E`, `SW`, `S`, and `SE`.
Input is case-insensitive and allows spaces, tabs, and newlines between tokens.
A square such as `C10` is one token; `C 10` is invalid. Empty or whitespace-only
input to `parse-turns` returns `[]`, and formatting an empty sequence returns
`""`. Empty turns within a sequence and trailing `/` separators are invalid.

Board snapshots use positive numbers for runs of empty cells and `R`/`B` for
stones, followed by metadata:

```text
1R6/8/8/8/8/8/8/5B2 @ 7 B 3 4 B2
board signifier turn-number player red-prisoners blue-prisoners [blocked-square]
```

The signifier records the game's status:

| Signifier | Status |
| --- | --- |
| `@` | Active |
| `=` | Draw |
| `#R`, `#B` | Red wins, Blue wins |
| `!R`, `!B` | Red resigns, Blue resigns |

The turn player, turn number, prisoners, and blocked square are preserved for
every status. Wins and draws set `:winner` to `:red`, `:blue`, or `:draw`.
Resignations also set `:resigned` to the resigning player and `:winner` to the
opponent, so `!R` round-trips distinctly from `#B`. This is notation support;
the game schema and rules do not yet define resignation behavior. Formatting
requires a resignation's winner to be the opponent of the resigning player.

For compatibility, slash-separated board segments are **columns**, starting
with A; cells in each column start at square 1 and continue upward. The example
places a red stone at A2 and a blue stone at H6, with B2 blocked. Only 8×8 and
10×10 snapshots are supported. Size is inferred from the number of columns.
Formatting combines empty runs and emits uppercase text with single spaces in
the metadata. Numbers and squares must remain intact when adding whitespace.

```clojure
(require '[torosalto.notation :as notation])

(notation/parse-turn "c10 > nw > sw + e5")
;; => {:action :hop, :details {:coords [2 9],
;;                            :directions [[-1 1] [-1 -1]],
;;                            :place-coords [4 4]}}

(notation/format-turns (notation/parse-turns "a1 / b2"))
;; => "A1/B2"
```

All six converters throw `ExceptionInfo` on invalid input, without printing.
Error data is under `:torosalto.notation/error` or `:torosalto.schema/error`.
Formatting preserves snapshot results and omits other extra fields; snapshots
with non-nil `next-blocked` are rejected. Snapshot version is always 0. Missing,
nil, or false optional coordinates normalize to nil; missing `next-blocked`
normalizes to nil. Round trips preserve data with these normalizations.

Conversion validates syntax and representation. `torosalto.rules` determines
whether a turn is legal for a particular game, including its board bounds and
whether hops mix orthogonal and diagonal directions.

Run the notation regression and generated round-trip tests with:

```sh
clojure -M:test
```

## Game Rules

### Physical Setup
Toro Salto is played on a regular checkerboard with even squares. The game is designed for a 10x10 board, but can be played well on an 8x8 board used for chess. Each player has a set of pieces, which are called stones. On one side of the board, a clear space is reserved for stones captured during play, this space is called the prison.

### Win Condition
A player wins by being the first to connect a line of 5 of their stones in a row. A winning line can be diagonal or orthogonal. Lines end at the boundary of the board and cannot wrap.

If no action is possible at the start of a turn, the game is a draw.

### The Board Wraps
In Toro Salto, the boundary edges of the gameboard connect to each and the gameboard wraps around. This means that a square on the right edge of the board will be adjacent to three squares the left edge as if the board had looped around. Similarly,top and bottom squares can be at adjacent, and following these rules, all corner squares are also adjacent to each other.

This is relevant for hopping and placing non-adjacent stones, but this does not apply to the win condition.

### How To Play
Players alternate turns throughout the game. On each turn, you must choose one of three actions: Place your stone on an empty square, hop a stone you control over other stones, or free two of your stones from prison.

#### Placing Stones
Place a new stone on any empty square on the board that is not *blocked*. 

#### Open Squares
An *open* square is an empty square that is not *blocked* and has no stones in adjacent squares. Be mindful that adjacency wraps around the edges of the board, and so a seemingly *open* square along the boundary of a board may be adjacent to stones along the opposite boundary. The rules for *open* squares are relevant when placing a stone after a multi-hop or when freeing stones. If there are no *open* squares available on the board, then that place action is skipped.

#### Hopping Stones
To perform a hop, choose a stone you control on the board, and then choose an orthogonal or diagonal direction where the adjacent square contains any stone, and the following square after that is empty. Hop your stone over the adjacent stone into the following empty square; this square is now the new position of your stone. Remove the captured adjacent stone and place it in the prison. 

As long as it is possible, you may choose to use that stone to perfom additional hops before the end of your turn; however, you cannot perform both diagonal and orthogonal hops within a single turn. If your first hop is diagonal, all subsequent hops must be in diagonal directions, and vice versa.

##### A Single Hop Blocks a Square
If your turn is composed of a single hop, then the square that was hopped over becomes *blocked* for the duration of the opposing player's immediate turn. They cannot place a stone on that square, but they can freely hop a stone into that square.

##### Multi-Hops Place A Stone
If you perform multiple hops within a turn, then you may place one stone on an *open* square before ending your turn.

#### Freeing Stones
Take two of your available stones from the prison, and place them on *open* squares on the board. You cannot place these two stones in adjacent squares. Freeing is only possible if the necessary stones and open spaces are available.
