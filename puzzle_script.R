library(readr)
library(tidyverse)
library(glue)
library(janitor)

#read in our safecracker puzzle as a data frame in its initial unrotated state
safecracker <- read_csv("safecracker.csv")
colnames(safecracker) <- colnames(safecracker) %>% 
  str_replace_all(" ","_") #make the table have code-friendly column names

#some useful functions for later
report_winner <- function(data) {
  winning_solution <- data %>%
    dplyr:: select("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4")
  glue("From the starting position, turn Wheel 1: {winning_solution$num_turns_1} time(s), turn Wheel 2: {winning_solution$num_turns_2} time(s), turn Wheel 3: {winning_solution$num_turns_3} time(s), and turn Wheel 4: {winning_solution$num_turns_4} time(s).")
}

get_new_position <-function(winner) {
  
  Base_constant <- as.list(safecracker$Base)
  floor_1 <- as.list(safecracker$Floor_1)
  wheel_1 <- as.list(safecracker$Wheel_1)
  floor_2 <- as.list(safecracker$Floor_2)
  wheel_2 <- as.list(safecracker$Wheel_2)
  floor_3 <- as.list(safecracker$Floor_3)
  wheel_3 <- as.list(safecracker$Wheel_3) 
  floor_4 <- as.list(safecracker$Floor_4)
  wheel_4 <- as.list(safecracker$Wheel_4)
  
  turns_1 <- as.numeric(winner$num_turns_1) ## TURN THESE BACK TO WINNER PLS
  turns_2 <-as.numeric(winner$num_turns_2)
  turns_3 <- as.numeric(winner$num_turns_3)
  turns_4 <- as.numeric(winner$num_turns_4)
  
  wheel_1_new <-  split(wheel_1,rep(1:2,c(16-turns_1,turns_1)))
  wheel_1_solved <- if (length(wheel_1_new)<2)  wheel_1 else c(wheel_1_new[[2]], wheel_1_new[[1]])
  floor_2_new <- split(floor_2,rep(1:2,c(16-turns_1,turns_1)))
  floor_2_solved <- if (length(floor_2_new)<2) floor_2 else c(floor_2_new[[2]], floor_2_new[[1]])
  
  wheel_2_new <- split(wheel_2,rep(1:2,c(16-turns_2,turns_2)))
  wheel_2_solved <- if (length(wheel_2_new)<2) wheel_2 else c(wheel_2_new[[2]], wheel_2_new[[1]])
  floor_3_new <- split(floor_3,rep(1:2,c(16-turns_2,turns_2)))
  floor_3_solved <- if (length(floor_3_new)<2) floor_3 else c(floor_3_new[[2]], floor_3_new[[1]])
  
  wheel_3_new <- split(wheel_3,rep(1:2,c(16-turns_3,turns_3)))
  wheel_3_solved <- if (length(wheel_3_new)<2) wheel_3 else c(wheel_3_new[[2]], wheel_3_new[[1]])
  floor_4_new <- split(floor_4,rep(1:2,c(16-turns_3,turns_3)))
  floor_4_solved <-  if (length(floor_4_new)<2) floor_4 else c(floor_4_new[[2]], floor_4_new[[1]])
  
  wheel_4_new <- split(wheel_4,rep(1:2,c(16-turns_4,turns_4)))
  wheel_4_solved <- if (length(wheel_4_new)<2) wheel_4 else c(wheel_4_new[[2]], wheel_4_new[[1]])
  
  safecracker_solved <-data.frame(unlist(Base_constant),unlist(floor_1),unlist(wheel_1_solved), unlist(floor_2_solved), unlist(wheel_2_solved), unlist(floor_3_solved), unlist(wheel_3_solved), unlist(floor_4_solved), unlist(wheel_4_solved))
  colnames(safecracker_solved) <- colnames(safecracker_solved) %>% 
    str_remove_all("\\.") %>%
    str_remove_all("_solved") %>%
    str_remove_all("_constant") %>%
    str_remove_all("unlist")
  
  verifications <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(verifications) <- c('works', 'total') 
  for (i in 1:nrow(safecracker_solved)) {
    constant <- safecracker_solved$Base[[i]]
    w <- ifelse(is.na(safecracker_solved$wheel_1[[i]]), safecracker_solved$floor_1[[i]], safecracker_solved$wheel_1[[i]])
    x <- ifelse(is.na(safecracker_solved$wheel_2[[i]]), safecracker_solved$floor_2[[i]], safecracker_solved$wheel_2[[i]])
    y <- ifelse(is.na(safecracker_solved$wheel_3[[i]]), safecracker_solved$floor_3[[i]], safecracker_solved$wheel_3[[i]])
    z <- ifelse(is.na(safecracker_solved$wheel_4[[i]]), safecracker_solved$floor_4[[i]], safecracker_solved$wheel_4[[i]])
    summed_val <- constant + w + x + y + z
    works<- if_else(summed_val == 50, "TRUE", "FALSE")
    momentoftruth <- c(works, summed_val)
    verifications<-rbind(verifications,momentoftruth)
  }
  safecracker_solved <-cbind(safecracker_solved, verifications)
  
}


##initiate an empty dataframe where we will record the results of all possible solutions for base position number 1 (this will be 65,536)
solutions_list <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_list) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')

##for base position 1 only, we run through a nested iteration of all possible combos: 
##for each position of each wheel, we try all possible combinations of other wheels,
##and test if they add up to 50, and then output a spreadsheet of number of turns of each wheel 
##and whether that combo works!
posish<-1
constant<-safecracker$Base[[1]]
for (h in 1:nrow(safecracker)) { 
  w<-if (is.na(safecracker$Wheel_1[[h]])) safecracker$Floor_1[[posish]] else safecracker$Wheel_1[[h]]
  rot_a <-h
  num_turns_w <- if (rot_a>posish) (posish + (16-rot_a)) else (posish-rot_a)
  for (J in 1:nrow(safecracker)) {
    x<-if (is.na(safecracker$Wheel_2[[J]])) safecracker$Floor_2[[h]] else safecracker$Wheel_2[[J]]
    rot_b <-J
    num_turns_x <- if (rot_b>posish) (posish + (16-rot_b)) else (posish-rot_b)
    for (K in 1:nrow(safecracker)) {
      y<-if (is.na(safecracker$Wheel_3[[K]])) safecracker$Floor_3[[J]] else safecracker$Wheel_3[[K]]
      rot_c <-K
      num_turns_y <- if (rot_c>posish) (posish + (16-rot_c)) else (posish-rot_c)
      for (L in 1:nrow(safecracker)) {
        z <- if (is.na(safecracker$Wheel_4[[L]])) safecracker$Floor_4[[K]] else safecracker$Wheel_4[[L]]
        rot_d <-L
        num_turns_z <- if (rot_d>posish) (posish + (16-rot_d)) else (posish-rot_d)
        
        summed_val <- constant + w + x + y + z
        print(summed_val)
        works<- if (summed_val == 50) ("TRUE") else  ("FALSE")
        the_numbers <- c(constant,w,x,y,z)
        print(the_numbers)
        latest <- c(posish,num_turns_w,num_turns_x,num_turns_y,num_turns_z,works)
        latest_df<-data.frame(matrix(latest, 1))
        colnames(latest_df) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
        solutions_list<-rbind(solutions_list,latest_df)
      }
    }
  }
}


##now we want to get rid of the combinations that don't add up to 50 and 
##we should be left with only possible rotations for base number one, which will narrow down to 2k-3k
solutions_in_running <- solutions_list %>%
  filter(works=="TRUE")

solutions_in_running[,'which_one'] = NA

#next we will test all these solutions across all 16 base rows based on the number of turns
for (combo in 1:nrow(solutions_in_running)) {
  safecracker_solved <- data.frame(matrix(ncol = 1, nrow = 0))
  tester<-solutions_in_running %>%
    slice(combo)
  get_new_position(tester)
  if (all(safecracker_solved$works == "TRUE"))
    solutions_in_running$which_one[[combo]] <- "WINNER"
    break
}

#

##find the winner, which should be a combination that is present for all Base positions
winner <- get_dupes(solutions_that_work, !Base_posish)
#output instructions to heist team

test_row <- solutions_in_running %>%
  slice(2)


report_winner(stil_runnin)

##and create a table that reflects the end position

hehe <-get_new_position(stil_runnin)


####in progress ugly version




single_line_test <- function(h, i, J, K, L) {
  posish<-i
  constant<-safecracker$Base[[i]]
w<-if (is.na(safecracker$Wheel_1[[h]])) safecracker$Floor_1[[posish]] else safecracker$Wheel_1[[h]]
rot_a <-h
num_turns_w <- if (rot_a>posish) (posish + (16-rot_a)) else (posish-rot_a)

x<-if (is.na(safecracker$Wheel_2[[J]])) safecracker$Floor_2[[h]] else safecracker$Wheel_2[[J]]
rot_b <-J
num_turns_x <- if (rot_b>posish) (posish + (16-rot_b)) else (posish-rot_b)

y<-if (is.na(safecracker$Wheel_3[[K]])) safecracker$Floor_3[[J]] else safecracker$Wheel_3[[K]]
rot_c <-K
num_turns_y <- if (rot_c>posish) (posish + (16-rot_c)) else (posish-rot_c)

z <- if (is.na(safecracker$Wheel_4[[L]])) safecracker$Floor_4[[K]] else safecracker$Wheel_4[[L]]
rot_d <-L
num_turns_z <- if (rot_d>posish) (posish + (16-rot_d)) else (posish-rot_d)

summed_val <- constant + w + x + y + z
print(summed_val)
works<- if (summed_val == 50) ("TRUE") else  ("FALSE")
the_numbers <- c(constant,w,x,y,z)
print(the_numbers)
latest <- c(posish,num_turns_w,num_turns_x,num_turns_y,num_turns_z,works)
latest_df<-data.frame(matrix(latest, 1))
colnames(latest_df) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
solutions_list<-rbind(solutions_list,latest_df)
}

single_line_test(h<-7,i<-9,J<-16,K<-3, L<-3)



solutions_round_2 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_round_2) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
posish <-2
constant <-safecracker$Base[[2]]
for (h in 1:nrow(safecracker)) { 
  w<-if (is.na(safecracker$Wheel_1[[h]])) safecracker$Floor_1[[posish]] else safecracker$Wheel_1[[h]]
  rot_a <-h
  num_turns_w <- if (rot_a>posish) (posish + (16-rot_a)) else (posish-rot_a)
  for (J in 1:nrow(safecracker)) {
    x<-if (is.na(safecracker$Wheel_2[[J]])) safecracker$Floor_2[[h]] else safecracker$Wheel_2[[J]]
    rot_b <-J
    num_turns_x <- if (rot_b>posish) (posish + (16-rot_b)) else (posish-rot_b)
    for (K in 1:nrow(safecracker)) {
      y<-if (is.na(safecracker$Wheel_3[[K]])) safecracker$Floor_3[[J]] else safecracker$Wheel_3[[K]]
      rot_c <-K
      num_turns_y <- if (rot_c>posish) (posish + (16-rot_c)) else (posish-rot_c)
      for (L in 1:nrow(safecracker)) {
        z <- if (is.na(safecracker$Wheel_4[[L]])) safecracker$Floor_4[[K]] else safecracker$Wheel_4[[L]]
        rot_d <-L
        num_turns_z <- if (rot_d>posish) (posish + (16-rot_d)) else (posish-rot_d)
        
        summed_val <- constant + w + x + y + z
        print(summed_val)
        works<- if_else(summed_val == 50, "TRUE", "FALSE")
        the_numbers <- c(constant,w,x,y,z)
        print(the_numbers)
        latest <- c(posish,num_turns_w,num_turns_x,num_turns_y,num_turns_z,works)
        latest_df<-data.frame(matrix(latest, 1))
        colnames(latest_df) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
        solutions_round_2<-rbind(solutions_round_2,latest_df)
      }
    }
  }
}

stw_2<-solutions_round_2 %>%
  filter(works=="TRUE")

in_running<- inner_join(solutions_in_running,stw_2, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4", "works"))



solutions_round_3 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_round_3) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
posish <-3
constant <-safecracker$Base[[3]]
for (h in 1:nrow(safecracker)) { 
  w<-if (is.na(safecracker$Wheel_1[[h]])) safecracker$Floor_1[[posish]] else safecracker$Wheel_1[[h]]
  rot_a <-h
  num_turns_w <- if (rot_a>posish) (posish + (16-rot_a)) else (posish-rot_a)
  for (J in 1:nrow(safecracker)) {
    x<-if (is.na(safecracker$Wheel_2[[J]])) safecracker$Floor_2[[h]] else safecracker$Wheel_2[[J]]
    rot_b <-J
    num_turns_x <- if (rot_b>posish) (posish + (16-rot_b)) else (posish-rot_b)
    for (K in 1:nrow(safecracker)) {
      y<-if (is.na(safecracker$Wheel_3[[K]])) safecracker$Floor_3[[J]] else safecracker$Wheel_3[[K]]
      rot_c <-K
      num_turns_y <- if (rot_c>posish) (posish + (16-rot_c)) else (posish-rot_c)
      for (L in 1:nrow(safecracker)) {
        z <- if (is.na(safecracker$Wheel_4[[L]])) safecracker$Floor_4[[K]] else safecracker$Wheel_4[[L]]
        rot_d <-L
        num_turns_z <- if (rot_d>posish) (posish + (16-rot_d)) else (posish-rot_d)
        
        summed_val <- constant + w + x + y + z
        print(summed_val)
        works<- if_else(summed_val == 50, "TRUE", "FALSE")
        the_numbers <- c(constant,w,x,y,z)
        print(the_numbers)
        latest <- c(posish,num_turns_w,num_turns_x,num_turns_y,num_turns_z,works)
        latest_df<-data.frame(matrix(latest, 1))
        colnames(latest_df) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
        solutions_round_3<-rbind(solutions_round_3,latest_df)
      }
    }
  }
}

st3 <-solutions_round_3 %>%
  filter(works=="TRUE")

still_running <- inner_join(st3, in_running, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4", "works"))

solutions_round_4 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_round_4) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
posish <-4
constant <-safecracker$Base[[4]]
for (h in 1:nrow(safecracker)) { 
  w<-if (is.na(safecracker$Wheel_1[[h]])) safecracker$Floor_1[[posish]] else safecracker$Wheel_1[[h]]
  rot_a <-h
  num_turns_w <- if (rot_a>posish) (posish + (16-rot_a)) else (posish-rot_a)
  for (J in 1:nrow(safecracker)) {
    x<-if (is.na(safecracker$Wheel_2[[J]])) safecracker$Floor_2[[h]] else safecracker$Wheel_2[[J]]
    rot_b <-J
    num_turns_x <- if (rot_b>posish) (posish + (16-rot_b)) else (posish-rot_b)
    for (K in 1:nrow(safecracker)) {
      y<-if (is.na(safecracker$Wheel_3[[K]])) safecracker$Floor_3[[J]] else safecracker$Wheel_3[[K]]
      rot_c <-K
      num_turns_y <- if (rot_c>posish) (posish + (16-rot_c)) else (posish-rot_c)
      for (L in 1:nrow(safecracker)) {
        z <- if (is.na(safecracker$Wheel_4[[L]])) safecracker$Floor_4[[K]] else safecracker$Wheel_4[[L]]
        rot_d <-L
        num_turns_z <- if (rot_d>posish) (posish + (16-rot_d)) else (posish-rot_d)
        
        
        summed_val <- constant + w + x + y + z
        print(summed_val)
        works<- if_else(summed_val == 50, "TRUE", "FALSE")
        the_numbers <- c(constant,w,x,y,z)
        print(the_numbers)
        latest <- c(posish,num_turns_w,num_turns_x,num_turns_y,num_turns_z,works)
        latest_df<-data.frame(matrix(latest, 1))
        colnames(latest_df) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
        solutions_round_4<-rbind(solutions_round_4,latest_df)
      }
    }
  }
}

st4 <-solutions_round_4 %>%
  filter(works=="TRUE")

stil_runnin <- inner_join(st4, still_running, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4", "works"))



#function test values
posish<-3
constant<-12
w<-10
x<-11
y<-13
z<-4
num_turns_w<-2
num_turns_x<-8
num_turns_y<-1
num_turns_z<-4

####more garbage
solutions_that_work <- solutions_list %>%
  filter(works=="TRUE") %>%
  mutate_at(vars(2:5), 
            ~as.numeric(.)) %>%
  mutate_at(vars(2:5), ~circularize(.))

base_1_solutions <- solutions_that_work %>%
  filter(Base_posish == 1)

base_2_solutions <- solutions_that_work %>%
  filter(Base_posish == 2)

base_3_solutions <- solutions_that_work %>%
  filter(Base_posish == 3)

base_4_solutions <- solutions_that_work %>%
  filter(Base_posish == 4)

base_5_solutions <- solutions_that_work %>%
  filter(Base_posish == 5)

runner <- inner_join(base_1_solutions, base_2_solutions, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4"))
runner <- inner_join(runner, base_3_solutions, by =c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4"))
runner <- inner_join(runner, base_4_solutions, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4"))

circularize <-function(x) {ifelse(x<0, x+15, x)}
## a more elegant solution can prune answers as it goes 
