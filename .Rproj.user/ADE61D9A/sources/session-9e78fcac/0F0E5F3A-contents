library(readr)
library(tidyverse)
library(glue)

positions <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
safecracker <- read_csv("safecracker.csv") %>%
  rename(floor_1 = "Floor 1",
         wheel_1 = "Wheel 1",
         floor_2 = "Floor 2",
         wheel_2 = "Wheel 2",
         floor_3 = "Floor 3",
         wheel_3 = "Wheel 3",
         floor_4 = "Floor 4",
         wheel_4 = "Wheel 4") %>%
  mutate(complement_value = 50-Base)%>%
  cbind(positions)

for i in safecracker$Base {
  posish<-which(safecracker$Base == i)
  
  for vals in safecracker$wheel_1

w <-ifelse(is.na(wheel_1), floor_1, wheel_1)
x <-ifelse(is.na(wheel_2), floor_2, wheel_2) # these need to become fixed until it's their turn to rotate
y <-ifelse(is.na(wheel_3), floor_3, wheel_3)
z <-ifelse(is.na(wheel_4), floor_4, [wheel_4,posish])

Base + w + x + y + z = summed_val

ifelse(summed_val = 50, )

}

num_turns<-(posish - row(i))

for all base values, find set of positions that add up to complement value
compare possible solutions 
extract constant set of positions


####notes and garbage
[row,column]
subset(df, A == "rowName1")$columnName
## [1] 11
2) Another possibility assuming that the row names are unique is to make them real row names removing the column that contains them. Then just use ordinary subscripting.

library(tibble)

df2 <- column_to_rownames(df, "A")
df2["rowName1", "columnName"]
## [1] 11