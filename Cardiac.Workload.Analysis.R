#Rowing Steady State Analysis
#Avery Ellis

#Setup
library(tidyverse)
library(patchwork)
setwd("~/GitHub/Rowing/Steady State") #this will vary for you

#Data Import
#All csv files in your given folder
all_workouts <- list.files(pattern = "*.csv")

#Bring them all into one "Workout" df
#and add in a workout ID and piece ID to analyze them
for (file in 1:length(all_workouts)) {
  if (file == 1) {
    workout_id <- 1
    workout <- read.csv(all_workouts[file]) %>%
      as_tibble() %>%
      mutate(Workout.ID = workout_id,
             Is.New.Piece = case_when(
               lag(Time..seconds.) > Time..seconds. ~ 1,
               TRUE ~ 0),
             Piece.Number = cumsum(Is.New.Piece))
    workout_id <- workout_id + 1
  } else {
    new_workout <- read.csv(all_workouts[file]) %>%
      as_tibble() %>%
      mutate(Workout.ID = workout_id,
             Is.New.Piece = case_when(
               lag(Time..seconds.) > Time..seconds. ~ 1,
               TRUE ~ 0),
             Piece.Number = cumsum(Is.New.Piece))
    workout <- rbind(workout, new_workout)
    workout_id <- workout_id + 1
  }
}

#Data Cleaning
workout <- workout %>%
  select(-c(Is.New.Piece)) %>%
  filter({
    Watts != 0 & Stroke.Rate != 0 & Pace..seconds. != 0})
colnames(workout) <- c("Stroke.Number", "Time.Elapsed", "Distance.Elapsed",
                         "Pace", "Watts", "Cal.Hr", "Stroke.Rate", 
                         "Heart.Rate", "Workout.ID", "Piece.Number")
#for easier understandability

workouts <- workout %>%
  select(Workout.ID, Piece.Number, Stroke.Number:Heart.Rate) %>%
  group_by(Workout.ID) %>%
  nest()

pieces <- workout %>%
  select(Workout.ID, Piece.Number, Stroke.Number:Heart.Rate) %>%
  group_by(Workout.ID, Piece.Number) %>%
  nest()

cardiac_workload_piece_plot <- function(df) {
  #Argument: a dataframe of the type exported by Concept2
  #Returns a ggplot of cardiac workload along with a 
  #few notes about the piece.
  df <- df %>%
    mutate(Cardiac.Workload = Watts / Heart.Rate)
  #Cardiac Workload describes power per heartbeat;
  #in effect describing how well the aerobic system
  #functions over the course of a piece or a workout.
  cardiac_workload_plot <- 
    ggplot(df, aes(x = Stroke.Number, y = Cardiac.Workload))+
    geom_point(color = "#210ecc", alpha = 0.22)+
    stat_smooth()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      plot.caption = element_text(hjust = 0.5, face = "italic"))+
    labs(x = "Stroke Number", 
         y = "Cardiac Workload (Power Per Heartbeat)", 
         title = "Cardiac Workload Analysis",
         subtitle = "Exploring Cardiac Drift Through A Piece",
         caption = "Plot Design by Avery Ellis
    Data Exported via Concept2")
}

cardiac_workload_workout_plot <- function(df) {
  #Argument: a dataframe of the type exported by Concept2
  #Returns a ggplot of cardiac workload along with a 
  #few notes about the piece.
  df <- df %>%
    mutate(Cardiac.Workload = Watts / Heart.Rate)
  #Cardiac Workload describes power per heartbeat;
  #in effect describing how well the aerobic system
  #functions over the course of a piece or a workout.
  cardiac_workload_plot <- 
    ggplot(df, aes(x = Stroke.Number, y = Cardiac.Workload, 
                   color = Piece.Number))+
    geom_point(color = "#210ecc", alpha = 0.22)+
    stat_smooth()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          plot.caption = element_text(hjust = 0.5, face = "italic"))+
    labs(x = "Stroke Number", 
         y = "Cardiac Workload (Power Per Heartbeat)", 
         title = "Cardiac Workload Analysis",
         subtitle = "Exploring Cardiac Drift Through A Workout",
         caption = "Plot Design by Avery Ellis
    Data Exported via Concept2")
}

pieces <- pieces %>%
  mutate(Cardiac.Workload.Viz = map(data, 
                                    cardiac_workload_piece_plot))
pieces[[4]][[2]]

workouts <- workouts %>%
  mutate(Cardiac.Workload.Viz = map(data, 
                                    cardiac_workload_workout_plot))
workouts[[3]][[1]]


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
  labs(x = "Stroke Number", y = "Heart Rate", title = "Workout Analysis",
       caption = "Plot Design by Avery Ellis
    Data Exported via Concept2")
prev_pr_hr_plot
  