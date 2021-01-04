#------------------------------------------------------------------------------#
#!!!!!!DO NOT RUN/KNIT IF RAM LESS THAN 16GB!!!!!!
#!!!!!THIS CODE IS MEMORY-CRITICAL!!!!!
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#INSTALLING PACKAGES NEEDED FOR THE REPORT
if(!require(rmarkdown)) install.packages(
  "rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages(
  "knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages(
  "kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages(
  "tinytex", repos = "http://cran.us.r-project.org")
#for installing TinyTeX (LaTeX distribution) using the 
#tinytex R package
tinytex::install_tinytex() 

#INSTALLING PACKAGES NEEEDED FOR THE CODE
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(caret)) install.packages(
  "caret", repos = "http://cran.us.r-project.org") 
if(!require(data.table)) install.packages(
  "data.table", repos = "http://cran.us.r-project.org") 
if(!require(lubridate)) install.packages(
  "lubridate", repos = "http://cran.us.r-project.org") 
if(!require(ggcorrplot)) install.packages(
  "ggcorrplot", repos = "http://cran.us.r-project.org") 
if(!require(randomcoloR)) install.packages(
  "randomcoloR", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages(
  "ggridges", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages(
  "recosystem", repos = "http://cran.us.r-project.org")

#------------------------------------------------------------------------------#
#LOADING PACKAGES FOR THE REPORT:
library(rmarkdown) #converting R markdown documents into several formats
library(knitr) #a general-purpose package for dynamic report generation
library(kableExtra) #nice table generator
library(tinytex) #for compiling from .Rmd to .pdf

#LOADING PACKAGES FOR THE CODE:
library(tidyverse) #for data processing and analysis
library(caret) #for splitting data for machine learning
library(data.table) #for data wrangling
library(lubridate) #for dealing with date-time attributes
library(ggcorrplot) #for plotting the correlation matrix
library(randomcoloR) #to generate a discrete color palette
library(ggridges) #for making ridges density plots
library(recosystem) #for implementing matrix factorization

#------------------------------------------------------------------------------#
#WRANGLING DATA
#downloading the file into a temporary file
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#.dat files have :: as column separators
#gsub replaces all :: with a tab so that the data can be easily read into a 
#table with the friendly fread 
ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

#reading in movies data and making a data frame
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")),"\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% 
#  mutate(
#    movieId = as.numeric(levels(movieId))[movieId],
#    title = as.character(title),
#    genres = as.character(genres))

#if using R 4.0 or later:
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

#to keep only the movies with corresponding entries in ratings and combine the 
#tables' columns, ratings data is left joined to movie data, resulting into a 
#new 'movielens' data set
movielens <- left_join(ratings, movies, by = "movieId")

#------------------------------------------------------------------------------#
#SPLITTING THE DATA

#validation set will be 10% of movieLens data
set.seed(1, sample.kind="Rounding") 
#if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(
  y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,] #training set
temp <- movielens[test_index,] #a temporary validation set

#to make sure userId and movieId in validation set are also in edx set, 
#semi-join returns rows from the temporary validation set that have a match in 
#the training set 'edx'
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#adding rows removed from the temporary validation set back into the edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#creating new directory within working directory to save the data sets generated
dir.create(file.path(getwd(),"rda")) 

#saving the test set 'validation' to use later
save(validation, file="rda/validation.rda")

#removing variables not needed for now to save memory
rm(dl, ratings, movies, test_index, temp, movielens, removed, validation)
#clearing out memory since the program is memory-critical
gc()

#------------------------------------------------------------------------------#
#PRE-PROCESSING THE EDX DATA SET

#extracting day, month and year of rating from timestamp, 
#extracting movie release year from the title, and
#subtracting year of rating from the movie release year to get years lapsed
#also, removing title and timestamp columns since all info extracted from them
edx <- edx %>% 
  mutate(month_rating = month(as_datetime(timestamp)),
         day_rating = as.numeric(factor(weekdays(as_datetime(timestamp)),
                                        levels = c("Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday",
                                                   "Sunday"))),
         year_movie = as.numeric(str_sub(title,-5,-2)),
         years_lapsed = year(as_datetime(timestamp))-year_movie) %>%
  select(-c("title", "timestamp"))

#to extract genre data, getting each genre in separate row
edx_genre_separated <- edx %>% 
  separate_rows(genres, sep="\\|")

#getting the average rating and count stats for every genre
genres_stats <- edx_genre_separated %>% 
  group_by(genres) %>% 
  summarise(avg_rating=mean(rating), n=n())

#to add genre characteristic rating, adding a column with the maximum of the 
#average ratings of the genres of the movie (rounded)
edx <- edx_genre_separated %>% 
  group_by(userId, movieId) %>% 
  left_join(genres_stats) %>% 
  summarize(genres_rating=round(max(avg_rating),1)) %>% 
  right_join(edx) %>%
  ungroup() %>%
  select(-genres) #genres column not needed anymore

#saving data sets to be used later
save(edx_genre_separated, file="rda/edx_genre_separated.rda")
save(genres_stats, file="rda/genres_stats.rda")
save(edx, file="rda/edx_final.rda")

#removing unnecessary data sets to save memory
#kept genres_stats to use for the validation set
rm(edx_genre_separated, edx)

gc() #clearing out memory

#------------------------------------------------------------------------------#
#PRE-PROCESSING VALIDATION DATA IN ACCORDANCE WITH EDX DATA

#re-loading the validation data set
load("rda/validation.rda")

#the columns that were added in edx are added here too.
#columns removed in edx are removed here too
validation <- validation %>% 
  mutate(
    month_rating = month(as_datetime(timestamp)),
    day_rating = as.numeric(factor(weekdays(
      as_datetime(timestamp)),
      levels=c("Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday",
               "Sunday"))),
    year_movie = as.numeric(str_sub(title,-5,-2)),
    years_lapsed = 
      year(as_datetime(timestamp))-year_movie) %>%
  select(-c("title", "timestamp"))

#extracting the genres by separating rows and adding on the genres_rating stats 
#using the avg_rating from the previously created genre_stats data set
#**The validation ratings have NOT been used in any manner here.
validation <- validation %>% 
  separate_rows(genres, sep="\\|") %>%
  group_by(userId, movieId) %>% 
  left_join(genres_stats) %>% 
  summarize(genres_rating=round(max(avg_rating),1)) %>% 
  right_join(validation) %>%
  ungroup() %>%
  select(-genres)

#saving the new validation data to use for testing later
save(validation, file="rda/validation_final.rda")

#removing validation and genres_stats data sets as they are not needed for now
rm(validation, genres_stats)

gc() #clearing out memory

#------------------------------------------------------------------------------#
#EXPLORATORY DATA ANALYSIS

load("rda/edx_final.rda")

str(edx)
summary(edx)
head(edx)

#barplot of the count of ratings
edx %>% ggplot(aes(rating)) + 
  geom_bar(fill="royalblue4") +
  labs(x="Rating", 
       y="Count",
       title="Distribution of Ratings") +
  theme_bw()

#checking the distribution of the features
edx %>% ggplot(aes(userId)) +
  geom_histogram(fill="royalblue4") +
  labs(x="User ID",
       y="Count",
       title="Distribution of User ID") +
  theme_bw()

edx %>% ggplot(aes(movieId)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Movie ID",
       y="Count",
       title="Distribution of Movie ID") +
  theme_bw()

edx %>% ggplot(aes(month_rating)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Month of Rating (Jan to Dec)",
       y="Count",
       title="Distribution of Month of Rating") +
  theme_bw()

edx %>% ggplot(aes(day_rating)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Day of Rating (Mon to Sun)",
       y="Count",
       title="Distribution of Day of Rating") +
  theme_bw()

edx %>% ggplot(aes(year_movie)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Year Movie Release",
       y="Count",
       title="Distribution of Year of Movie Release") +
  theme_bw()

edx %>% ggplot(aes(years_lapsed)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Years Lapsed",
       y="Count",
       title="Distribution of Years Lapsed") +
  theme_bw()

edx %>% ggplot(aes(genres_rating)) +
  geom_histogram(fill="royalblue4") +
  labs(x="Max Average Rating of Genres",
       y="Count",
       title="Distribution of Max Average Rating of Genres") +
  theme_bw()

#checking correlations of all the variables
corr <- round(cor(edx),1)
p.mat <- cor_pmat(edx)
ggcorrplot(corr, hc.order = TRUE, type = "lower", 
           lab = TRUE, p.mat = p.mat) +
  labs(title="Pearson correlation Heatmap")

rm(corr, p.mat) #removing variables not needed
gc() #clearing the memory

#------------------------------------------------------------------------------#
#ANALYZING HOW RATINGS VARY WITH THE FEATURES

#checking the variation of ratings with time
edx %>% ggplot(aes(x = (years_lapsed + year_movie),
                   y=factor(round(rating)))) + 
  geom_density_ridges(fill="royalblue4", col="grey") +
  labs(x="Year of Rating", 
       y="Rating (rounded)",
       title="Density Plots of Ratings over the Years") +
  theme_bw()

#checking the variation of ratings with the month
edx %>% ggplot(aes(x=rating, y=factor(month_rating))) + 
  geom_density_ridges(fill="royalblue4", col="grey") +
  labs(x="Rating (rounded)", 
       y="Month of Rating (Jan to Dec)",
       title="Density Plots of Month of Rating vs. Rating") +
  theme_bw()

#checking the variation of ratings with the day of the week
edx %>% ggplot(aes(x=rating, y=factor(day_rating))) + 
  geom_density_ridges(fill="royalblue4", col="grey") +
  labs(x="Rating (rounded)", 
       y="Day of Rating (Mon to Sun)",
       title="Density Plots of Day of Rating vs. Rating") +
  theme_bw()

#checking the variation of ratings with movie release year
#fct_rev (factor reverse) is used for 0 ratings at lowest
#direction argument in scale_fill_brewer changes the order of palette colours
edx %>% 
  ggplot(aes(year_movie, fill= fct_rev(factor(round(rating))))) + 
  geom_density(position="fill", adjust=7) + 
  scale_fill_brewer(palette="RdBu", direction=-1) + 
  labs(x="Year of Movie Release", 
       y="Density",
       title="Stacked Density Plot of Movie Release Year, filled by Rating",
       fill="Rating\n(rounded)") +
  theme_bw()

#checking the variation of ratings with years lapsed
#fct_rev (factor reverse) is used for 0 ratings at lowest
#direction argument in scale_fill_brewer changes the order of palette colours
edx %>% 
  ggplot(aes(years_lapsed, fill= fct_rev(factor(round(rating))))) + 
  geom_density(position="fill", adjust=7) + 
  scale_x_reverse() + 
  scale_fill_brewer(palette="RdBu", direction=-1) + 
  labs(x="Years Lapsed since Movie Release", 
       y="Density",
       title="Stacked Density Plot of Years Lapsed, filled by Rating",
       fill="Rating\n(rounded)") +
  theme_bw()

#------------------------------------------------------------------------------#
#GENRE ANALYSIS

#I will temporarily offload the edx data set and use the edx_genre_separated 
#data set that was generated earlier
rm(edx)
gc()
load("rda/edx_genre_separated.rda")

#analysis of distribution of genres
#also analysing the distributions of ratings within genres
edx_genre_separated %>% 
  ggplot(aes(fct_rev(genres), fill=fct_rev(factor(round(rating))))) +
  geom_bar() +
  scale_fill_brewer(palette="RdBu", direction=-1) + 
  labs(x="Genres", 
       y="Count",
       title="Distribution of Genres, filled by Rating",
       fill="Rating\n(rounded)") +
  theme_bw() +
  coord_flip()

palette1 <- distinctColorPalette(20)
#analysis of distribution of genres within ratings
edx_genre_separated %>% 
  ggplot(aes(factor(round(rating)), fill=genres)) +
  geom_bar(position = "fill") + 
  scale_fill_manual(values=palette1) +
  theme(legend.title = element_blank()) +
  labs(x="Rating (rounded)", 
       y="Count",
       title="Distribution of Genres within Ratings",
       fill="Genres") +
  guides(fill = guide_legend(nrow = 11)) +
  theme_bw() 

#analysis of genres vs. month of rating
edx_genre_separated %>% 
  ggplot(aes(x=month_rating, y=genres)) +
  geom_density_ridges(fill="royalblue4", col="grey") +
  labs(x="Month of Rating (Jan to Dec)", 
       y="Genres",
       title="Density Plots of Genres vs. Month of Rating") +
  theme_bw()

#analysis of distribution of genres vs. day of rating
edx_genre_separated %>% 
  ggplot(aes(factor(day_rating), fill=genres)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values=palette1) +
  theme(legend.title = element_blank()) +
  labs(x="Day of Rating (Mon to Sun)", 
       y="Count",
       title="Distribution of Genres within Rating-Days",
       fill="Genres") +
  guides(fill = guide_legend(nrow = 11)) +
  theme_bw() 

#analysis of distribution of genres across movie release years
edx_genre_separated %>% 
  ggplot(aes(year_movie, fill=genres)) + 
  geom_density(position="fill", adjust=7) +  
  scale_fill_manual(values=palette1) +
  theme(legend.title = element_blank()) +
  labs(x="Year of Movie Release", 
       y="Density",
       title="Distribution of Genres across Movie Release Years",
       fill="Genres") +
  guides(fill = guide_legend(nrow = 11)) +
  theme_bw() 

#analysis of genre distribution with rating years
edx_genre_separated %>%
  ggplot(aes((years_lapsed+year_movie), y=..count.., col=genres)) + 
  geom_density(adjust=7, size=1.2) + 
  scale_color_manual(values=palette1) +
  theme(legend.title = element_blank()) +
  labs(x="Rating (rounded)", 
       y="Count",
       title="Distribution of Genres with Year of Rating",
       col="Genres") +
  guides(col = guide_legend(nrow = 11)) +
  theme_bw() 

#I will now remove the edx_genre_separated data set and use the genre_stats data
#set that was generated earlier 
rm(edx_genre_separated)
gc()
load("rda/genres_stats.rda")

#analysis of average ratings per genre and the number of such ratings
genres_stats %>% 
  mutate(genres=reorder(genres,avg_rating)) %>% 
  ggplot(aes(genres,avg_rating, size=round(n,-2))) +
  geom_point(col="royalblue4") +
  labs(x="Genres", 
       y="Average Rating",
       title="Average Ratings of Genres",
       size="Number of\nRatings") +
  theme_bw() +
  coord_flip()

#removing genres_stats and clearing memory to prepare for machine learning
rm(genres_stats)
gc()

#------------------------------------------------------------------------------#
#MACHINE LEARNING

#loading the final training and test sets
load("rda/edx_final.rda") #training set
load("rda/validation_final.rda") #test set

#Defining RMSE:
actual_rating <- validation$rating
RMSE <- function(predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

#------------------------------------------------------------------------------#
#1. Elementary model (using mean)
mu <- mean(edx$rating)

#although this is redundant here, I do it anyway for uniformity across models
predicted_ratings_model1 <- validation %>%
  mutate(pred = mu) %>%
  pull(pred)

#RMSE values for each model are stored for comparison
model1_rmse<-RMSE(predicted_ratings_model1)

#Printing the RMSE
model1_rmse

#removing variable and clearing memory
rm(predicted_ratings_model1)
gc()

#checking the distribution of ratings within movie ids
#fct_rev (factor reverse) is used for 0 ratings at lowest
#direction argument in scale_fill_brewer changes the order of palette colours
edx %>%
  ggplot(aes(movieId, fill=fct_rev(factor(round(rating))))) +
  geom_density(position="fill") + 
  scale_fill_brewer(palette="RdBu", direction=-1) + 
  labs(x="Movie ID", 
       y="Density",
       title="Stacked Density Plot of Movie ID, filled by Rating",
       fill="Rating\n(rounded)") +
  theme_bw()

#------------------------------------------------------------------------------#
#2. Modeling movie effect:
movie_effect <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating-mu))

#predicting ratings for the validation set
predicted_ratings_model2 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model2_rmse <- RMSE(predicted_ratings_model2)
model2_rmse

#removing variable and clearing memory
rm(predicted_ratings_model2)
gc()

#variation of movie effect with movie ID
movie_effect %>% 
  ggplot(aes(movieId, b_i)) + 
  geom_point(alpha=0.2, col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of Movie Effect",
       x = "Movie ID",
       y = "Movie Effect (b_i)",
       caption="Movies affect ratings irregularly.") + 
  scale_x_log10() +
  theme_bw()

#checking the distribution of ratings within user ids
#fct_rev (factor reverse) is used for 0 ratings at lowest
#direction argument in scale_fill_brewer changes the order of palette colours
edx %>%
  ggplot(aes(userId, fill=fct_rev(factor(round(rating))))) +
  geom_density(position="fill") + 
  scale_fill_brewer(palette="RdBu", direction=-1) + 
  labs(x="User ID", 
       y="Density",
       title="Stacked Density Plot of User ID, filled by Rating",
       fill="Rating\n(rounded)") +
  theme_bw()

#------------------------------------------------------------------------------#
#3. Modeling user effect:
user_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating-mu-b_i))

#predicting ratings for the validation set
predicted_ratings_model3 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  mutate(pred = mu + b_i + b_u ) %>%
  pull(pred)

model3_rmse <- RMSE(predicted_ratings_model3)
model3_rmse

#removing variable and clearing memory
rm(predicted_ratings_model3)
gc()

#variation of user effect with user ID
user_effect %>% 
  ggplot(aes(userId, b_u)) + 
  geom_point(alpha=0.2, col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of User Effect",
       x = "User ID",
       y = "User Effect (b_u)",
       caption="Users affect ratings irregularly.") + 
  theme_bw()

#------------------------------------------------------------------------------#
#4. Modeling 'month of rating' effect:
month_rating_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  group_by(month_rating) %>%
  summarise(b_mr = mean(rating-mu-b_i-b_u))

#predicting ratings for the validation set
predicted_ratings_model4 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  mutate(pred = mu + b_i + b_u + b_mr ) %>%
  pull(pred)

model4_rmse <- RMSE(predicted_ratings_model4)
model4_rmse

#removing variable and clearing memory
rm(predicted_ratings_model4)
gc()

#variation of 'month of rating' effect with 
#month of rating
month_rating_effect %>% 
  ggplot(aes(month_rating, b_mr)) + 
  geom_point(col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of 'Month of Rating' Effect",
       x = "Month of Rating (Jan to Dec)",
       y = "Month of Rating Effect (b_mr)",
       caption="Slightly higher ratings are given in later months.") + 
  theme_bw()

#------------------------------------------------------------------------------#
#4. Modeling 'day of rating' effect:
day_rating_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  group_by(day_rating) %>%
  summarise(b_dr = mean(rating-mu-b_i-b_u-b_mr))

#predicting ratings for the validation set
predicted_ratings_model5 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  mutate(pred = mu + b_i + b_u + b_mr + b_dr) %>%
  pull(pred)

model5_rmse <- RMSE(predicted_ratings_model5)
model5_rmse

#removing variable and clearing memory
rm(predicted_ratings_model5)
gc()

#variation of 'day of rating' effect with day of rating
day_rating_effect %>% 
  ggplot(aes(day_rating, b_dr)) + 
  geom_point(col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of 'Day of Rating' Effect",
       x = "Day of Rating (Mon to Sun)",
       y = "Day of Rating Effect (b_dr)",
       caption="Lower ratings are given towards the weekend,") + 
  theme_bw()

#------------------------------------------------------------------------------#
#6. Modeling 'year of movie release' effect:
year_movie_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  group_by(year_movie) %>%
  summarise(b_ym = mean(rating-mu-b_i-b_u-b_mr-b_dr))

#predicting ratings for the validation set
predicted_ratings_model6 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  mutate(pred = mu + b_i + b_u + b_mr + b_dr + b_ym) %>%
  pull(pred)

model6_rmse <- RMSE(predicted_ratings_model6)
model6_rmse

#removing variable and clearing memory
rm(predicted_ratings_model6)
gc()

#variation of 'year of movie release' effect with
#year of movie release
year_movie_effect %>% 
  ggplot(aes(year_movie, b_ym)) + 
  geom_point(col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of 'Year of Movie Release' Effect",
       x = "Year of Movie Release",
       y = "Year of Movie Release Effect (b_ym)",
       caption="Older movies are rated higher than newer ones.") + 
  theme_bw()

#------------------------------------------------------------------------------#
#7. Modeling 'years lapsed' effect:
years_lapsed_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>% 
  group_by(years_lapsed) %>%
  summarise(b_yl = mean(rating-mu-b_i-b_u-b_mr-b_dr-b_ym))

#predicting ratings for the validation set
predicted_ratings_model7 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  mutate(pred = mu + b_i + b_u + b_mr + b_dr + b_ym + b_yl) %>%
  pull(pred)

model7_rmse <- RMSE(predicted_ratings_model7)
model7_rmse

#removing variable and clearing memory
rm(predicted_ratings_model7)
gc()

#variation of 'years lapsed' effect with years lapsed
years_lapsed_effect %>% 
  ggplot(aes(years_lapsed, b_yl)) + 
  geom_point(col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of 'Years Lapsed' Effect",
       x = "Years Lapsed",
       y = "Years Lapsed Effect (b_yl)",
       caption="Years lapsed affect movie ratings irregularly.") + 
  theme_bw()

#------------------------------------------------------------------------------#
#8. Modeling genre effect:
genre_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>% 
  group_by(genres_rating) %>%
  summarise(b_g = mean(rating-mu-b_i-b_u-b_mr-b_dr-b_ym-b_yl))

#predicting ratings for the validation set
predicted_ratings_model8 <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  left_join(genre_effect, by="genres_rating") %>%
  mutate(pred = mu + b_i + b_u + b_mr + b_dr + b_ym + b_yl + b_g) %>%
  pull(pred)

model8_rmse <- RMSE(predicted_ratings_model8)
model8_rmse

#removing variable and clearing memory
rm(predicted_ratings_model8)
gc()

#variation of genre effect with max avg genre rating
genre_effect %>% 
  ggplot(aes(genres_rating, b_g)) + 
  geom_point(col="royalblue4") +
  geom_smooth(col="black", fill="red") +
  labs(title = "Variation of Genre Effect",
       x = "Max Average Genre Rating",
       y = "Genre Effect (b_g)",
       caption="Genres affect movie ratings irregularly.") + 
  theme_bw()

#------------------------------------------------------------------------------#
#PREPARING FOR CROSS VALIDATION
set.seed(1, sample.kind="Rounding") 
#if using R 3.5 or earlier, use `set.seed(1)`

#Creating the test index
edx_test_index <- createDataPartition(
  edx$rating, times=1, p=0.2, list = FALSE)

#Splitting the edx set for cross validation
edx_train <- edx[-edx_test_index,] #used for training
edx_test <- edx[edx_test_index,] #used for tuning

#To ensure only those movies and users are present in edx_test for which there 
#are observations in edx_train
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

#Defining a sequence of values of lambda(l) to check RMSE
l <- seq(1, 10, 1)

#Applying lambda values and checking RMSE in edx_test:
rmses <- sapply(l, function(l){
  #Mean Ratings:
  mu <- mean(edx_train$rating)
  
  #Movie Effect:
  movie_effect <- edx_train %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating-mu)/(n()+l))
  
  #User Effect
  user_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating-mu-b_i)/(n()+l))
  
  #Month of Rating Effect:
  month_rating_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    group_by(month_rating) %>%
    summarise(b_mr = sum(rating-mu-b_i-b_u)/(n()+l))
  
  #Day of Rating Effect:
  day_rating_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    group_by(day_rating) %>%
    summarise(b_dr = sum(rating-mu-b_i-b_u-b_mr)/(n()+l))
  
  #Year of Movie Release Effect:
  year_movie_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(day_rating_effect, by="day_rating") %>%
    group_by(year_movie) %>%
    summarise(b_ym = sum(rating-mu-b_i-b_u-b_mr-b_dr)/(n()+l))
  
  #Years Lapsed Effect:
  years_lapsed_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(day_rating_effect, by="day_rating") %>%
    left_join(year_movie_effect, by="year_movie") %>% 
    group_by(years_lapsed) %>%
    summarise(b_yl = sum(rating-mu-b_i-b_u-b_mr-b_dr-b_ym)/(n()+l))
  
  #Genre Effect:
  genre_effect <- edx_train %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(day_rating_effect, by="day_rating") %>%
    left_join(year_movie_effect, by="year_movie") %>% 
    left_join(years_lapsed_effect, by="years_lapsed") %>%
    group_by(genres_rating) %>%
    summarise(b_g = sum(rating-mu-b_i-b_u-b_mr-b_dr-b_ym-b_yl)/(n()+l))
  
  #Applying the models on edx_test set:
  predicted_ratings <- edx_test %>%
    left_join(movie_effect, by="movieId") %>%
    left_join(user_effect, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(day_rating_effect, by="day_rating") %>%
    left_join(year_movie_effect, by="year_movie") %>%
    left_join(years_lapsed_effect, by="years_lapsed") %>%
    left_join(genre_effect, by="genres_rating") %>%
    mutate(pred = mu + b_i + b_u + b_mr + b_dr + b_ym + b_yl + b_g) %>%
    pull(pred)
  
  #Calculating the RMSE:
  sqrt(mean((edx_test$rating - predicted_ratings)^2))
})

#plotting RMSE against lambda (l)
data.frame(l = l, RMSE = rmses) %>%
  ggplot(aes(l, RMSE)) + 
  geom_point(col="royalblue4") + 
  geom_line(col="royalblue4")  + 
  labs(title = "Variation of RMSE with l (lambda)",
       x = "l (lambda)",
       y = "RMSE") +
  theme_bw()

#Removing variables not needed anymore and clearing memory
rm(edx_test_index, edx_test, edx_train)
gc()

#storing the l which minimized RMSE on cross valdiation
l <- l[which.min(rmses)]

#Mean Ratings:
mu <- mean(edx$rating)

#Movie Effect:
movie_effect <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating-mu)/(n()+l))

#User Effect:
user_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating-mu-b_i)/(n()+l))

#Month of Rating Effect:
month_rating_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  group_by(month_rating) %>%
  summarise(b_mr = sum(rating-mu-b_i-b_u)/(n()+l))

#Day of Rating Effect:
day_rating_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  group_by(day_rating) %>%
  summarise(b_dr = sum(rating-mu-b_i-b_u-b_mr)/(n()+l))

#Year of Movie Release Effect:
year_movie_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  group_by(year_movie) %>%
  summarise(b_ym = sum(rating-mu-b_i-b_u-b_mr-b_dr)/(n()+l))

#Years Lapsed Effect:
years_lapsed_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>% 
  group_by(years_lapsed) %>%
  summarise(b_yl = sum(rating-mu-b_i-b_u-b_mr-b_dr-b_ym)/(n()+l))

#Genre Effect:
genre_effect <- edx %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  group_by(genres_rating) %>%
  summarise(b_g = sum(rating-mu-b_i-b_u-b_mr-b_dr-b_ym-b_yl)/(n()+l))

#Applying the model on the validation set:
predicted_ratings_reg <- validation %>%
  left_join(movie_effect, by="movieId") %>%
  left_join(user_effect, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(day_rating_effect, by="day_rating") %>%
  left_join(year_movie_effect, by="year_movie") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  left_join(genre_effect, by="genres_rating") %>%
  mutate(pred = mu + b_i + b_u + b_mr + b_dr + b_ym + b_yl + b_g) %>%
  pull(pred) 

#Calculating the RMSE:
rmse_reg <- RMSE(predicted_ratings_reg)
rmse_reg

#removing variables not needed anymore and clearing memory
rm(predicted_ratings_reg, movie_effect, user_effect, month_rating_effect, 
   day_rating_effect, year_movie_effect, years_lapsed_effect, genre_effect, mu, 
   l)
gc()

#------------------------------------------------------------------------------#
#COLLABORATIVE FILTERING USING MATRIX FACTROIZATION VIA RECOSYSTEM

set.seed(1, sample.kind="Rounding") 
#if using R 3.5 or earlier, use `set.seed(1)

#converting the 'edx' and 'validation' sets into recosystem-compatible forms 
#data_memory function specifies a data set from R objects
edx_reco <-  with(edx, data_memory(
  user_index = userId,
  item_index = movieId, 
  rating = rating))
validation_reco  <-  with(validation, data_memory(
  user_index = userId,
  item_index = movieId,
  rating = rating))

#creating the recosystem object
r <-  recosystem::Reco()

#training the model with default parameters
r$train(edx_reco)

#getting the predicted ratings
#out_memory is used to return the result as an R object
predicted_ratings_reco <- r$predict(validation_reco, out_memory())

#calculating the RMSE
rmse_reco <- RMSE(predicted_ratings_reco)
rmse_reco

#removing variables not needed anymore and clearing memory 
rm(predicted_ratings_reco, r, edx_reco, validation_reco, true_ratings)
gc()

#------------------------------------------------------------------------------#
#RESULTS

#making a data frame of the RMSEs of all the models
models <- c("Elementary Model", 
            "Movie Effect +",
            "User Effect +",
            "Month of Rating Effect +",
            "Day of Rating Effect +",
            "Year of Movie Release Effect +",
            "Years Lapsed Effect +",
            "Genre Effect +", 
            "Final Hybrid Model", 
            "Recosystem Model")
rmse_values <- c(model1_rmse, model2_rmse, model3_rmse, model4_rmse, 
                 model5_rmse, model6_rmse, model7_rmse, model8_rmse, 
                 rmse_reg, rmse_reco)

rmse_results <- data.frame(models, rmse_values)

#plotting the RMSEs
rmse_results %>% 
  mutate(models=reorder(models,-rmse_values)) %>%
  ggplot(aes(models, rmse_values, label=round(rmse_values,5))) +
  geom_col(fill="royalblue4", width=0.3) +
  geom_text(position = position_nudge(y = 0.126)) +
  coord_flip() +
  labs(title="RMSEs of the Models",
       x="Model",
       y="RMSE")