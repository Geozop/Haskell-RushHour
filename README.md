# Haskell-RushHour
"Rush Hour" game-solving algorithm, using Haskell

## Info
"Rush Hour" is the copyrighted name of a puzzle where the board contains multiple vehicles which can move either north-south or east-west. There is a "goal" car that is to be moved off the board, but many vehicles block it.

The program's "main" function is called statesearch, and it expects an input of an array of strings that represent the board. Empty spaces are dashes, and the goal car is made of two X characters on the third row from the top. Eg:

statesearch ["------", "--C---","XXC---","------","------","------"]
