setwd("/Users/michaelharper/OMSBA 5112 R Files/week_2")

library(tidyverse)
library(haven)

## Load raw data ##
nfhs <- read_dta("Raw Data/IAHR52FL.dta")
summary(nfhs)
View(nfhs)

# Create a new data frame that selects all variables between the household id
# and hv208 (both included) plus the wealth index.

house_df <- nfhs %>%
  select('hhid':'hv208', 'hv270')
write.csv(house_df, file = "Raw Data/hh_data_df.csv")

#Wealth index
#1  Poorest
#2  Poorer
#3  Middle
#4  Richer
#5  Richest


#create a data frame that holds the household id, the individuals' line numbers,
#and their education in single years.
df_educ <- nfhs %>%
  select('hhid', starts_with('hvidx'), starts_with('hv108'))


## anthropometric data for females and the household id
fem_surv <- nfhs %>%
  select(
    'hhid',
    starts_with('ha0'),
    starts_with('ha1'),
    starts_with('ha2'),
    starts_with('ha3'),
    starts_with('ha4'),
    starts_with('ha5'),
    starts_with('ha6')
  )
## anthropometric data for males and the household id
male_surv <- nfhs %>%
  select(
    'hhid',
    starts_with('hb0'),
    starts_with('hb1'),
    starts_with('hb2'),
    starts_with('hb3'),
    starts_with('hb4'),
    starts_with('hb5'),
    starts_with('hb6')
  )


# Gather - separate - spread for #6
## Education DataFrame
education <- df_educ %>%
  gather(key = "codes", value = "n", 2:71)

education <- education %>%
  separate(codes, c("category", "years"), sep = "_")

education1 <- drop_na(education)
education1 <- spread(education1, key = "category", value = "n")
education1 <- education1 %>% rename("roster_id" = "years")

write.csv(education1, file = "Raw Data/education_df.csv")

## Female Survey
#sample_frac::small_df <- big_df %>% sample_frac(0.1)
# test sample

female <- fem_surv %>% sample_frac(0.1)

female <- female %>%
  gather(key = "category", value = "n", 2:419)

female <- female %>%
  separate(category, c("category", "roster_id"), sep = "_")

female1 <- drop_na(female)

female1 <- spread(female1, key = "category", value = "n")

female1 <-  female1 %>%
  select('hhid':'ha1', 'ha2', 'ha3') %>%
  drop_na()
vector_t <- rep('female', 13942) ## Create TRUE
female1$gender <- vector_t #Add TRUE to DataFrame
female1 <- rename(female1, 'ID' = 'ha0', 'age' = 'ha1', 'weight' = 'ha2', 'height' = 'ha3')
write.csv(female1, file = "Raw Data/female_df.csv")

## Male Survey

male <- male_surv %>% sample_frac(0.1)
  
male <- male %>%
    gather(
      key = "category",
      value = "n",
      2:667)
      
male <- male %>%
  separate(category, c("category", "roster_id"), sep = "_")
      
male1 <- drop_na(male)
      
male1 <- spread(male1, key = "category", value = "n")
      
male1 <-  male1 %>%
  select('hhid':'hb1', 'hb2', 'hb3') %>%
  drop_na()
vector <-rep('male', 9039)
male1$gender <- vector #add FALSE to DataFrame
male1 <- rename(male1, 'ID' = 'hb0', 'age' = 'hb1', 'weight' = 'hb2', 'height' = 'hb3')
write.csv(male1, file = "Raw Data/male_df.csv")
          
#Stack female and male data frame 
gender_df <- bind_rows(female1, male1)
write.csv(gender_df, file = "Raw Data/male_female_df.csv")
          
#merge education data
final_df <- gender_df %>% semi_join(education1, by = "hhid")
final_df <- final_df %>% semi_join(house_df, by = "hhid")
          
#median age of males | females
median_age <- final_df %>%
  select(gender, age) %>%
  group_by(gender)
median_age$age <- as.double(median_age$age) #coerce <chr> into <dbl>
summarise(median_age, age = median(age))       #summarise   
    
          