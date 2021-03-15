#2K Analysis
#Avery Ellis

library(tidyverse)
 

clean_2K <- function(filename) {
  #Argument: the name of a csv file.
  #Reads in a CSV file for a 2K outputted by Concept2, 
  #and then cleans the column names and adds new columns
  #so that I can better read and understand my test pieces.
  
  workout <- read.csv(filename)
  workout <- as_tibble(workout)
  colnames(workout) <- c("Stroke.Number", "Time.Elapsed", "Distance.Elapsed",
                            "Pace", "Watts", "Cal.Hr", "Stroke.Rate", 
                            "Heart.Rate")
  workout <- workout %>%
    mutate(Distance.Remaining = 2000 - Distance.Elapsed,
           Pace.Mins = Pace %/% 60) %>%
    mutate(Pace.Sec = Pace - 60 * Pace.Mins) %>%
    mutate(Pace.Per.500 = paste(Pace.Mins, ":", 
                                 ifelse(Pace.Sec > 10, 
                                        round(Pace.Sec, digits = 4), 
                                        paste(0, round(Pace.Sec, digits = 4), 
                                              sep = ""))
                                 , sep = "")) %>%
    select(Stroke.Number, Distance.Remaining, Pace.Per.500,
           Stroke.Rate, Time.Elapsed, Heart.Rate, Watts, Distance.Elapsed, Pace)
}


prev_pr <- clean_c2_workout("11.30.2020.csv")
current_pr <- clean_c2_workout("3.11.2021.csv")



#Plotting time!