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
education2 <- spread(education1, key = "category", value = "n")
education2 <- education2 %>% rename("roster_id" = "years")

## Female Survey
#sample_frac::small_df <- big_df %>% sample_frac(0.1)
# test sample

#fe_small <- fem_surv %>% sample_frac(0.1)

female <- fem_surv %>%
  gather(key = "category", value = "n", 2:419)

female <- female %>%
  separate(category, c("category", "roster_id"), sep = "_")

female1 <- drop_na(female)

female1 <- spread(female1, key = "category", value = "n")

female1 <-  female1 %>%
  select('hhid':'ha1', 'ha2', 'ha3') %>%
  drop_na()
vector_t <- rep(TRUE,#<need length>#) ## Create TRUE
female1$gender <- vector_t #Add TRUE to DataFrame

## Male Survey
  
male <- male_surv %>%
    gather(
      key = "category",
      value = "n",
      ##FIX ME how many variables?)
      
male <- male %>%
  separate(category, c("category", "roster_id"), sep = "_")
      
male1 <- drop_na(male)
      
male1 <- spread(male1, key = "category", value = "n")
      
male1 <-  male1 %>%
  elect('hhid':'hb1', 'hb2', 'hb3') %>%
  drop_na()
vector_f <-rep(FALSE, #<need length>#)
male1$gender <- vector_f #add FALSE to DataFrame
          
#Stack female and male data frame 
gender_df <- bind_rows(female1, male1)
          
#merge education data
final_df <- gender_df %>% left_join(df_educ, by = "hhid")
final_df <- final_df %>% left_join(house_df, by = "hhid")
          
#median age of males | females
by_gender <- group_by(final_df, gender)
summarise(by_gender, median = median(gender, na.rm = TRUE))
          
          