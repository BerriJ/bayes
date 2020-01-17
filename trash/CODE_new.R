#Links: 

# https://stats.stackexchange.com/questions/268734/how-to-use-blasso-function-in-r-package-monomvn



#### required packages ####
library(dplyr)
library(readr)
library(ggplot2)
library(permute)
library(stringr)
library(here)
library(purrr)
library(BMS)
library(monomvn)
library(miscTools)
library(tidyverse)

#### reading in data ####  
#data_16 <- readr::read_csv(here::here('00_data/career_mode/players_16.csv'))
#data_17 <- readr::read_csv(here::here('00_data/career_mode/players_17.csv'))
#data_18 <- readr::read_csv(here::here('00_data/career_mode/players_18.csv'))
data_19 <- readr::read_csv(here::here('00_data/career_mode/players_19.csv'))
data_20 <- readr::read_csv(here::here('00_data/career_mode/players_20.csv'))

#### datsa manupilation ####

#creating a yearly variable for the panel data frame 

year <-as.data.frame(c(rep(19, nrow(data_19)),
                       rep(20, nrow(data_20))))
colnames(year)[1] <- "year"

# changing the different classes of the different data frames to merge these  
#data_16 <- data_16 %>%
#  mutate_if(. , is.numeric, as.character)

#data_17 <- data_17 %>%
#  mutate_if(. , is.numeric, as.character)

#data_18 <- data_18 %>%
#  mutate_if(. , is.numeric, as.character)

data_19 <- data_19 %>%
  mutate_if(. , is.numeric, as.character) 

data_20 <- data_20 %>% 
  mutate_if(. , is.numeric, as.character)

# bind the data frames 
fifa_data <- bind_rows(data_19, data_20, .id = NULL)%>%
  bind_cols( year)%>%
  mutate(age = as.numeric(age),
         value_eur = as.numeric(value_eur),
         log_value = log(value_eur),
         wage_eur = as.numeric(wage_eur),
         log_wage = log(wage_eur),
         height_cm = as.numeric(height_cm),
         weight_kg = as.numeric(weight_kg),
         overall = as.numeric(overall),
         potential = as.numeric(potential),
         player_positions = as.factor(player_positions),
         preferred_foot = as.factor(preferred_foot),
         international_reputation = as.factor(international_reputation),
         weak_foot = as.factor(weak_foot),
         skill_moves = as.factor(skill_moves),
         work_rate = as.factor(work_rate),
         body_type = as.factor(body_type),
         real_face  = as.logical(real_face),
         release_clause_eur = as.numeric(release_clause_eur),
         team_position = as.factor(team_position),
         contract_valid_until = as.numeric(contract_valid_until),
         pace = as.numeric(pace),
         shooting = as.numeric(shooting),
         passing = as.numeric(passing),
         gk_diving = as.numeric(gk_diving),                  
         gk_handling = as.numeric(gk_handling),                
         gk_kicking = as.numeric(gk_kicking),                
         gk_reflexes = as.numeric(gk_reflexes),            
         gk_speed = as.numeric(gk_speed),                
         gk_positioning = as.numeric(gk_positioning),
  )%>%
  dplyr::select(
  -(player_traits:rb))%>%
  filter(
    log_value > -Inf,
    log_wage > -Inf,
  )


fifa_data <- fifa_data[, c(1:43, 105:107)]





rm(list = apropos("data_"))

fifa_data%>%
  dplyr::select(log_value,
         log_wage)%>%
  summary()


train <- fifa_data %>%
  filter(year < 20)

train_y <- train$log_value

train_x <- train %>%
  select(log_value)
select(-c(log_value, wage_eur, value_eur, year)) # , player_url, short_name, long_name, club ))


names(fifa_data)

train_x_2 <-cbind(train$log_wage, train$age, year, player_url, short_name, long_name, club)

test <- fifa_data %>%
  filter(year == 20)





#### model 


IT <- 10
BURN <- 5

mod <- blasso(train_x, train_y, T = IT)

mod$beta
coef.lasso <- as.data.frame(cbind(iter = seq(IT), 
                                  beta1 = mod$beta[, "b.1"],
                                  beta1 = mod$beta[, "b.2"],
                                  variance = mod$s2, 
                                  lambda.square = mod$lambda2))
class(coef.lasso$beta1)

colMedians(coef.lasso[-seq(BURN), -1])



summary.blasso(mod)

plot.blasso(mod)
mod$beta
mod