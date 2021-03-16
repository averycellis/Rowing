#2K Analysis
#Avery Ellis

library(tidyverse)
#setwd("~/Documents/GitHub/Rowing")
 

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


prev_pr <- clean_2K("11.30.2020.csv")
current_pr <- clean_2K("3.11.2021.csv")
#Plotting time!

prev_pr_hr_plot <- prev_pr %>% 
  ggplot(aes(x = Stroke.Number, y = Heart.Rate))+
  geom_point(color = "#210ecc", alpha = 0.22)+
  geom_line()+
  #HR points and line of best fit
  #geom_point(aes(x = Stroke.Number, y = Watts), 
             #color = "#c40606", alpha = 0.222)+
  theme(#legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  labs(x = "Stroke Number", y = "", title = "Workout Analysis",
       caption = "Plot Design by Avery Ellis
       Data Exported via Concept2")
prev_pr_hr_plot
  