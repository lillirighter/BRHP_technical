Initiated 5/27/25
contact lilli.righter@gmail.com

"BRHP Safecracker puzzle technical interview.pdf" provides the problem with instructions.

safecracker.csv is the spreadsheet version of the puzzle in its initial state. Each row is one column on the puzzle, where the base stays constant.
This file is the input to the solver script.

puzzle_script.R reads in the safecracker.csv and 
  outputs solve_instructions.txt in the following format:
  "Turn wheel 1 XX times, turn wheel 2 XX times, turn wheel 3 XX times, and turn wheel 4 XX times."
  and also outputs puzzle_solved.csv, which is a table corresponding to the puzzle in the solved configuration. 
  Each row of the table is one column on the puzzle and adds up to 50.
  
 How to use for any similar puzzle with 16 columns that need to add up to 50, and 4 turnable wheels:
 
 1. Open safecracker.csv in any spreadsheet software (e.g. Excel), delete the current values (except the column names). 
 
 2. Take a picture of your wooden puzzle as it is right now so you can reference it in its current state later. 
 
 3. Each column of the spreadsheet corresponds to one LAYER of the puzzle, and what appears as a vertical column of numbers from all the layers on the wooden puzzle will be a row in the spreadsheet. Fill in the values from your puzzle one layer at a time-- there should be sixteen values in a layer, and therefore 16 values in each column. Fill in the Base and Wheel layers first. Every time there is a gap in a wheel, leave a cell of the spreadsheet blank. Fill in the uncovered values on the Floor layers-- they should align with the blank cells of the Wheel columns. Now, you'll also need to fill in the values of the Floor layers that are covered up. Rotate the top Wheel (wheel 4) one time, and fill in the values of Floor 3 that are now visible. Continue turning each wheel one time and filling in the floor underneath each, until all empty Floor layer values have been filled in. Take care never to rotate a wheel until the floor with the same number has been filled in, because rotating the wheel will also change the position of the floor of the same number. Save this spreadsheet as a csv in the same folder that puzzle_script.R is saved in! Close the csv.
 
4. Reset your puzzle so that all layers are in the same position as the photo you took in step 2. You'll have to rotate them either one step in the opposite direction, or 15 steps in the same direction you already rotated the wheels.
 
 5. Open puzzle_script.R in R or RStudio. Run the script. This should take about 5 minutes. 
 
 6. The script should create two new files in the same folder: solve_instructions.txt, and puzzle_solved.csv. Open solve_instructions.txt.
 
 7. Rotate the each Wheel layer of the puzzle COUNTERCLOCKWISE the number of times indicated in the instructions. 
 
 8. Open puzzle_solved.csv. Check each column of numbers on your wooden puzzle against each row of the spreadsheet: the sequence of numbers should be the same as you go up the layers, and each column of the puzzle should add up to 50.
 
 9. If you need to retain a record of the puzzle in its original state and its solution, make sure to rename the two .csv files before you restart this process with a new puzzle. Otherwise the script will overwrite the puzzle_solved.csv with the new puzzle's solution. 
 
 Other tips:
 Before you solve the first puzzle using this R script, you'll need to install R (https://www.r-project.org/), and you'll need to install three packages: 
 readr
 tidyverse
 glue


To install a package, open R and in the console, write the following, with one of the package names inside the single quotes (packagename is just a placeholder here):
install.packages('packagename') 

And press enter. R should give you a message about where it has installed the packages. Repeat for all packages.
