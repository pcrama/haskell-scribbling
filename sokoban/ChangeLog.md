# Revision history for sokoban

## 0.1.0.0  -- 2019-03-04

* First version: Curses interface

## 0.2.0.0  -- 2019-03-05

* Unlimited undo possible, limited at 50 at player's request.  Undo
  information is registered for each crate movement, not player
  movements (these can be simply undone by moving)
* Catch & ignore exceptions while waiting for user input (otherwise,
  touching the screen while playing in Termux would crash the game)
* Encode player start position in level's map
* Hide cursor
* Load levels from data file (separated by at least one empty line)

## 0.3.0.0  -- 2019-03-06

* Interactively select level to play

## 0.4.0.0  -- 2019-03-07

* Reorganize code & add tests
* Add command line argument parsing for amount of allowed undo actions
