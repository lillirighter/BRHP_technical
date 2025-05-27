library(readr)
library(tidyverse)
library(glue)
library(janitor)

#read in our safecracker puzzle as a data frame in its initial unrotated state
safecracker <- read_csv("safecracker.csv")
colnames(safecracker) <- colnames(safecracker) %>% 
  str_replace_all(" ","_") #make the table have code-friendly column names

##initiate an empty dataframe where we will record the results of all possible solutions (this will be >1 million, 16^5)
solutions_list <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_list) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')


##nested iteration so that for each position of each wheel, we try all possible combinations of other wheels
for (i in 1:nrow(safecracker)) {
  posish<-i
  constant<-safecracker$Base[[i]]
  for (h in 1:nrow(safecracker)) { 
    w<- ifelse(is.na(safecracker$Wheel_1[[h]]), safecracker$Floor_1[[posish]], safecracker$Wheel_1[[h]])
    rot_a <-h
    num_turns_w <- (rot_a-posish)
    for (J in 1:nrow(safecracker)) {
      x <- ifelse(is.na(safecracker$Wheel_2[[J]]), safecracker$Floor_2[[posish]], safecracker$Wheel_2[[J]])
      rot_b <-J
      num_turns_x <- (rot_b-posish)
      for (K in 1:nrow(safecracker)) {
        y <- ifelse(is.na(safecracker$Wheel_3[[K]]), safecracker$Floor_3[[posish]], safecracker$Wheel_3[[K]])
        rot_c <-K
        num_turns_y <- (rot_c-posish)
        for (L in 1:nrow(safecracker)) {
          z <- ifelse(is.na(safecracker$Wheel_4[[L]]), safecracker$Floor_4[[posish]], safecracker$Wheel_4[[L]])
          rot_d <-L
          num_turns_z <- (rot_d-posish)
    
          summed_val <- constant + w + x + y + z
          print(summed_val)
          works<- if_else(summed_val == 50, "TRUE", "FALSE")
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
}


##now we want to get rid of the combinations that don't add up to 50 and we should be left with only possible rotations for each base column 
solutions_that_work <- solutions_list %>%
  filter(works=="TRUE")

##find the winner, which should be a combination that is present for all Base positions
winner <- get_dupes(solutions_that_work, !Base_posish)
#output instructions to heist team



####in progress ugly version


##nested iteration so that for each position of each wheel, we try all possible combinations of other wheels
posish<-1
constant<-safecracker$Base[[1]]
for (h in 1:nrow(safecracker)) { 
  w<- ifelse(is.na(safecracker$Wheel_1[[h]]), safecracker$Floor_1[[posish]], safecracker$Wheel_1[[h]])
  rot_a <-h
  num_turns_w <- (rot_a-posish)
  for (J in 1:nrow(safecracker)) {
    x <- ifelse(is.na(safecracker$Wheel_2[[J]]), safecracker$Floor_2[[posish]], safecracker$Wheel_2[[J]])
    rot_b <-J
    num_turns_x <- (rot_b-posish)
    for (K in 1:nrow(safecracker)) {
      y <- ifelse(is.na(safecracker$Wheel_3[[K]]), safecracker$Floor_3[[posish]], safecracker$Wheel_3[[K]])
      rot_c <-K
      num_turns_y <- (rot_c-posish)
      for (L in 1:nrow(safecracker)) {
        z <- ifelse(is.na(safecracker$Wheel_4[[L]]), safecracker$Floor_4[[posish]], safecracker$Wheel_4[[L]])
        rot_d <-L
        num_turns_z <- (rot_d-posish)
        
        summed_val <- constant + w + x + y + z
        print(summed_val)
        works<- if_else(summed_val == 50, "TRUE", "FALSE")
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


##now we want to get rid of the combinations that don't add up to 50 and we should be left with only possible rotations for each base column 
solutions_in_running <- solutions_list %>%
  filter(works=="TRUE")

solutions_round_2 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_round_2) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
posish <-2
constant <-safecracker$Base[[2]]
for (h in 1:nrow(safecracker)) { 
  w<- ifelse(is.na(safecracker$Wheel_1[[h]]), safecracker$Floor_1[[posish]], safecracker$Wheel_1[[h]])
  rot_a <-h
  num_turns_w <- (rot_a-posish)
  for (J in 1:nrow(safecracker)) {
    x <- ifelse(is.na(safecracker$Wheel_2[[J]]), safecracker$Floor_2[[posish]], safecracker$Wheel_2[[J]])
    rot_b <-J
    num_turns_x <- (rot_b-posish)
    for (K in 1:nrow(safecracker)) {
      y <- ifelse(is.na(safecracker$Wheel_3[[K]]), safecracker$Floor_3[[posish]], safecracker$Wheel_3[[K]])
      rot_c <-K
      num_turns_y <- (rot_c-posish)
      for (L in 1:nrow(safecracker)) {
        z <- ifelse(is.na(safecracker$Wheel_4[[L]]), safecracker$Floor_4[[posish]], safecracker$Wheel_4[[L]])
        rot_d <-L
        num_turns_z <- (rot_d-posish)
        
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

in_running<- inner_join(solutions_that_work,stw_2, by = c("num_turns_1", "num_turns_2", "num_turns_3", "num_turns_4", "works"))



solutions_round_3 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(solutions_round_2) <- c('Base_posish', 'num_turns_1', 'num_turns_2', 'num_turns_3', 'num_turns_4', 'works')
posish <-3
constant <-safecracker$Base[[3]]
for (h in 1:nrow(safecracker)) { 
  w<- ifelse(is.na(safecracker$Wheel_1[[h]]), safecracker$Floor_1[[posish]], safecracker$Wheel_1[[h]])
  rot_a <-h
  num_turns_w <- (rot_a-posish)
  for (J in 1:nrow(safecracker)) {
    x <- ifelse(is.na(safecracker$Wheel_2[[J]]), safecracker$Floor_2[[posish]], safecracker$Wheel_2[[J]])
    rot_b <-J
    num_turns_x <- (rot_b-posish)
    for (K in 1:nrow(safecracker)) {
      y <- ifelse(is.na(safecracker$Wheel_3[[K]]), safecracker$Floor_3[[posish]], safecracker$Wheel_3[[K]])
      rot_c <-K
      num_turns_y <- (rot_c-posish)
      for (L in 1:nrow(safecracker)) {
        z <- ifelse(is.na(safecracker$Wheel_4[[L]]), safecracker$Floor_4[[posish]], safecracker$Wheel_4[[L]])
        rot_d <-L
        num_turns_z <- (rot_d-posish)
        
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

## a more elegant solution can prune answers as it goes 
