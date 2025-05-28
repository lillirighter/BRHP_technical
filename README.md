initiated 5/27/25
contact lilli.righter@gmail.com

"BRHP Safecracker puzzle technical interview.pdf" provides the problem with instructions.

safecracker.csv is the spreadsheet version of the puzzle in its initial state. Each row is one column on the puzzle, where the base stays constant.

puzzle_script.R reads in the safecracker.csv and 
  outputs instructions in the following format:
  Turn wheel 1 XX times, turn wheel 2 XX times, turn wheel 3 XX times, and turn wheel 4 XX times.
  and also outputs solved_puzzle.csv, which is a table corresponding to the puzzle in the solved configuration. 
  Each row is one column on the puzzle and adds up to 50.
