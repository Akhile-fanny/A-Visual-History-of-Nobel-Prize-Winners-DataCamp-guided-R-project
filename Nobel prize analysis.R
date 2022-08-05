
# loading R packages

library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)


#importing the nobel dataset
nobel <- read_csv("C:\\Users\\USER\\Desktop\\A Visual History of Nobel Prize Winners\\nobel.csv")

# viewing the first 6 data in the nobel dataset.
head(nobel)


# number of prizes received by male and female
nobel %>% 
  group_by(sex) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  drop_na (sex)


# total prizes won

nobel %>% 
  count() %>% 
  
  
  # adding age of the winners
 nobel_age <-  nobel %>% 
mutate(age = year - year( birth_date)) %>% 
  print(nobel_age)


#full name of winners and their age

nobel_age %>% 
  select(full_name, age, year, category) %>% 
  arrange(age)



  # who is the oldest winner

nobel_age %>% 
top_n(1, age)

# who is the youngest winner
nobel_age %>% 
  top_n(1, desc(age))


# prizes won by decades
nobel_age %>%
  mutate(decade = floor(year / 10 )* 10) %>% 
  group_by(decade) %>% 
  count() %>% 
  rename(number_of_prizes_won = n) %>% 
  arrange(desc(number_of_prizes_won))



# number of different laureate_type  won 

nobel_age %>% 
  count(laureate_type) %>% 
  filter(n >1) %>% 
group_by(n)


# age and years prizes was won.

ggplot(nobel_age, aes(year, age, fill = "blue")) + geom_point() + geom_smooth() + facet_wrap(~category) +
  labs( x = "Year prize was won", y = "Age of winners", title = " Age distribution of winners",
        subtitle = "Sub-divided by category")



![image](https://user-images.githubusercontent.com/95649144/183224377-85755696-2161-4753-98be-84b62f2b9a00.png)



# number of prizes won by categories
prize_category<- nobel_age %>% 
  group_by(category) %>% 
  count()  %>% 
  arrange(desc(n))
 


# graphical representation of prizes by category

ggplot(prize_category, aes(category, n, fill = "red")) + 
                          geom_col() + labs(y="number_of_prizes",
                         title = "number of prizes won by categories",
                        subtitle = "Medicine has the highest prizes")





#country and prizes won

nobel %>% 
  group_by(birth_country) %>% 
  count() %>% 
  rename(number_of_prizes = n) %>% 
  arrange(desc(number_of_prizes))


# first woman to win a prize

first_woman <- nobel_age %>% 
  filter(sex == "Female") %>% 
  top_n(1, desc(year))
  
  
  
  #proportion of female winners in decades

nobel_age %>% 
  mutate(decade = floor(year / 10) * 10, 
                female_winner = sex == "Female") %>% 
  group_by(decade, category) %>% 
  summarise(proportion = mean(female_winner, na.rm = TRUE) ) 
 

# who received prizes more than once

nobel_age %>%
  group_by(full_name) %>% 
  count() %>% 
  filter(n >1) %>% 
rename( times_won = n)  

  
          
