#1 Run the following lines and study how they work. Then state what they do and output for us. (20 Points)

df1 = data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                        'Richards','George','Ema','Samantha','Catherine'),
                 State=c('Alaska','California','Texas','North Carolina','California','Texas',
                         'Alaska','Texas','North Carolina','Alaska','California','Texas'),
                 Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))
#This line creates a data frame df1 with three columns: Name, State, and Sales. The Name column lists the names of individuals, State shows which state they are associated with, and Sales represents some numeric sales data associated with each person.

aggregate(df1$Sales, by=list(df1$State), FUN=sum)
#This function aggregates the Sales column of the data frame df1 by the State column. It groups the sales by state and then sums them using the sum function. The by parameter takes a list where each element is a factor by which to group the data. The output is a new data frame showing the total sales for each state.

library(dplyr)
#This command loads the dplyr library, which is part of the tidyverse and is used for data manipulation. It provides a more intuitive syntax and additional functionality for manipulating data frames compared to base R.

df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))
#This line does essentially the same thing as the aggregate function but uses the dplyr syntax, which is often clearer and more readable. The %>% operator is used to pass the data frame forward into successive functions:

#2 Use R to read the WorldCupMatches.csv from the DATA folder on Google Drive. Then perform the followings (48 points):
  #(a) Find the size of the data frame. How many rows, how many columns?
library(readr)
world_cup_matches <- read_csv("~/Downloads/CSC 302 /DATA/WorldCupMatches.csv")

num_rows <- nrow(world_cup_matches)
num_cols <- ncol(world_cup_matches)

  #(b) Use summary function to report the statistical summary of your data.
data_summary <- summary(world_cup_matches)


  #(c) Find how many unique locations olympics were held at.
unique_locations <- world_cup_matches %>% 
  distinct(Stadium) %>% 
  nrow()

  #(d) Find the average attendance.
average_attendance <- mean(world_cup_matches$Attendance, na.rm = TRUE)

  #(e) For each Home Team, what is the total number of goals scored? (Hint: Please refer to question 1)
total_goals_by_home_team <- world_cup_matches %>%
  group_by(`Home Team Name`) %>%
  summarise(TotalGoals = sum(`Home Team Goals`, na.rm = TRUE))


  #(f) What is the average number of attendees for each year? Is there a trend or pattern in the data in that sense?
average_attendance_per_year <- world_cup_matches %>%
  group_by(Year) %>%
  summarise(AvgAttendance = mean(Attendance, na.rm = TRUE))
  # Print the results
print(paste("Number of rows: ", num_rows))
print(paste("Number of columns: ", num_cols))
print("Data Summary:")
print(data_summary)

#3 Use R to read the metabolites.csv from the DATA folder on Google Drive. Then perform the followings (32 points):
# Load necessary libraries

library(readr)
library(dplyr)

# Read the data from the CSV file
metabolites <- read_csv("~/Downloads/CSC 302 /DATA/metabolite.csv")
print(colnames(metabolites))

  #(a) Find how many Alzheimers patients there are in the data set. (Hint: Please refer to question 1)
unique_labels <- unique(metabolites$Label)
print(unique_labels)

alzheimers_patients <- metabolites %>%
  filter(Label == "Alzheimers") %>%
  nrow()

  #(b) Determine the number of missing values for each column. (Hint: is.na( ) )
missing_values_per_column <- colSums(is.na(metabolites))

  #(c) Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame.
metabolites_no_na_dopamine <- metabolites %>%
  filter(!is.na(Dopamine))

  #(d) In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same column. (Hint: there is median( ) function.)
median_c4_OH_Pro <- median(metabolites_no_na_dopamine$`c4-OH-Pro`, na.rm = TRUE)
metabolites_no_na_dopamine <- metabolites_no_na_dopamine %>%
  mutate(`c4-OH-Pro` = ifelse(is.na(`c4-OH-Pro`), median_c4_OH_Pro, `c4-OH-Pro`))

  #(e) (Optional) Drop columns which have more than 25% missing values. (Hint: when you slice your data frame, you can use -c(.., ..., ...) where ... represent one column name)
threshold <- 0.25 * nrow(metabolites_no_na_dopamine)
columns_to_keep <- colSums(is.na(metabolites_no_na_dopamine)) <= threshold
metabolites_cleaned <- metabolites_no_na_dopamine[, columns_to_keep]

# Print the results
print(paste("Number of Alzheimer's patients: ", alzheimers_patients))
print("Number of missing values per column:")
print(missing_values_per_column)
print("Data frame after removing rows with missing Dopamine values and replacing missing c4-OH-Pro values with median:")
print(metabolites_no_na_dopamine)
print("Data frame after dropping columns with more than 25% missing values:")
print(metabolites_cleaned)

