#PSYC 259 Homework 4 - Writing functions
#Optional, for extra credit if all questions are answered

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

limit_replace <- function(vec) {
  new_vec <- ifelse(vec < 2 | vec > 8, NA, vec)
  return(new_vec)}

x_replace <- limit_replace(x)
x_replace

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower bounary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

limit_replace <- function(vec, lower, upper) {
  new_vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(new_vec)}

y_replace <- limit_replace(y, 4, 6)
y_replace


### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

plus_minus_SD <- function(vec, num_of_SDs = 1) {
  m <- mean(vec)
  s <- sd(vec)
  lower <- m - num_of_SDs * s
  upper <- m + num_of_SDs * s
  c(lower, upper)}

plus_minus_SD(x)
plus_minus_SD(y, 2)


### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

limit_replace <- function(vec, lower = NA, upper = NA) {
  if (is.na(lower) || is.na(upper)) {
    bounds <- plus_minus_SD(vec)
    lower <- bounds[1]
    upper <- bounds[2]}
  vec[vec < lower | vec > upper] <- NA
  return(vec)}

ds_replace <- ds %>%
  mutate(across(where(is.numeric), ~limit_replace(.)))

ds_replace


### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

limit_replace <- function(vec, lower = NA, upper = NA) {
  stopifnot(is.numeric(vec))
  if (is.na(lower) || is.na(upper)) {
    bounds <- plus_minus_SD(vec)
    lower <- bounds[1]
    upper <- bounds[2]}
  
  vec[vec < lower | vec > upper] <- NA
  return(vec)}

print(limit_replace(ds$x))
print(limit_replace(ds$id))


### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

limit_replace <- function(vec, lower = NA, upper = NA) {
  stopifnot(any(!is.na(vec)))
  
  if (!is.na(lower) && !is.na(upper)) {
    stopifnot(is.numeric(lower), is.numeric(upper), lower < upper)} 
  else {bounds <- plus_minus_SD(vec)
  lower <- bounds[1]
  upper <- bounds[2]}
  
  vec[vec < lower | vec > upper] <- NA
  return(vec)}


### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

plus_minus_SD <- function(vec, num_of_SDs = 1) {
  m <- mean(vec, na.rm = TRUE)
  s <- sd(vec, na.rm = TRUE)
  lower_bound <- m - num_of_SDs * s
  upper_bound <- m + num_of_SDs * s
  return(c(lower_bound, upper_bound))}

limit_replace <- function(vec, lower = NA, upper = NA) {
  stopifnot(is.numeric(vec), length(vec) > 0, any(!is.na(vec)))
  if (is.na(lower) || is.na(upper)) {
    bounds <- plus_minus_SD(vec)
    lower <- bounds[1]
    upper <- bounds[2]} 
  else {stopifnot(is.numeric(lower), is.numeric(upper), lower < upper)}
  vec[vec < lower | vec > upper] <- NA
  return(vec)}

rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

list.files()

source("functions.R")

ds_trimmed <- ds_diamonds %>%
  mutate(across(where(is.numeric), ~limit_replace(.))) %>%
  drop_na()

ds_trimmed


### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()


cuts_to_plot <- c("Premium", "Ideal", "Good")

library(purrr)

plot_price <- function(data, chosen_cut, desc) {
  data %>%
    filter(cut == chosen_cut) %>%
    ggplot(aes(x = clarity, y = price)) +
    geom_boxplot() +
    ggtitle(paste(chosen_cut, desc)) +
    theme_minimal()}


map(cuts_to_plot, ~ plot_price(ds_diamonds, .x, "all"))
map(cuts_to_plot, ~ plot_price(ds_trimmed, .x, "trimmed"))


