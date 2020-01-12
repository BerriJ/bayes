#Links: 
# functions
#function for class
allClass <- function(x) 
{unlist(lapply(unclass(x),class))}


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
library(tictoc)
library(glmnet)

#### reading in data ####  
#data_16 <- readr::read_csv(here::here('00_data/career_mode/players_16.csv'))
#data_17 <- readr::read_csv(here::here('00_data/career_mode/players_17.csv'))
#data_18 <- readr::read_csv(here::here('00_data/career_mode/players_18.csv'))
data_19 <- readr::read_csv(here::here('00_data/career_mode/players_19.csv'))
data_20 <- readr::read_csv(here::here('00_data/career_mode/players_20.csv'))

#### datsa manupilation ####

#creating a yearly variable  

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
  dplyr::select(-(player_traits:rb))

fifa_data <- fifa_data %>%
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
         defending = as.numeric(defending),
         dribbling = as.numeric(dribbling),
         gk_diving = as.numeric(gk_diving),                  
         gk_handling = as.numeric(gk_handling),                
         gk_kicking = as.numeric(gk_kicking),                
         gk_reflexes = as.numeric(gk_reflexes),            
         gk_speed = as.numeric(gk_speed),                
         gk_positioning = as.numeric(gk_positioning),
         nationality = as.factor(nationality),
         physic = as.numeric(physic),
  )%>% # creating dumies 
  bind_cols(
    sjmisc::to_dummy(fifa_data$team_position, suffix = 'label' ),
    sjmisc::to_dummy(fifa_data$nationality, suffix = 'label')
    )%>%
  filter(
    log_value > -Inf,
    log_wage > -Inf,
  )


#rm(list = apropos("data_"))
# dummies <- fifa_data %>%
#   filter(year == 19)%>%
#   dplyr::select(team_position_CAM:nationality_Zimbabwe)

 names(fifa_data)
 
train <- fifa_data %>%
  filter(year == 19)%>%
  dplyr::select(log_wage,
                log_value,
                age,
                height_cm,
                weight_kg,
                overall, 
                potential, 
                shooting,
                contract_valid_until,
                pace,
                shooting,
                passing,
                dribbling,
                defending,
                team_position_CAM:team_position_ST)%>%
  imputeTS::na_replace(., 0)


test <- fifa_data %>%
  filter(year == 2020)%>%
  dplyr::select(log_wage,
                log_value,
                age,
                height_cm,
                weight_kg,
                overall, 
                potential, 
                shooting,
                contract_valid_until,
                pace,
                shooting,
                passing,
                dribbling,
                defending,
                team_position_CAM:team_position_ST)%>%
  imputeTS::na_replace(., 0)

  

  

#change the form of the data for blasso model
train_y <- train%>%
  dplyr::select(log_value)


train_x <- train %>%
  dplyr::select(-log_value)


test <- fifa_data %>%
  filter(year == 20)



#### model 

?blasso()

IT <- 100
BURN <- IT *0.5

initial.beta <- rep(-500, dim(train_x)[2]) # assigning an extreme initial value for all betas
initial.lambda2 <- 10 # assigning an extreme initial value for lambda (penalty parameter)
initial.variance <- 500 # assigning an extreme initial value for variance parameter


save(bayes_lasso_fit, file = here::here('04_output/bayes_lasso_fit.RData'))



load(file = here::here('04_output/bayes_lasso_fit.RData'))

 
tictoc::tic()
bayes_lasso_fit <- blasso(as.matrix(train_x), as.matrix(train_y), 
              T = IT )

#, RJ = TRUE, 
          
              
#              lambda2 = 10,)
tictoc::toc()


colnames(mod$beta)

AA <- mod$beta
coef.lasso <- as.data.frame(cbind(iter = seq(IT), 
                                  beta1 = bayes_lasso_fit$beta[, "b.1"],
                                  beta2 = bayes_lasso_fit$beta[, "b.2"],
                                  beta3 = bayes_lasso_fit$beta[, "b.3"],
                                  beta4 = bayes_lasso_fit$beta[, "b.4"],
                                  beta5 = bayes_lasso_fit$beta[, "b.5"],
                                  beta6 = bayes_lasso_fit$beta[, "b.6"],
                                  beta7 = bayes_lasso_fit$beta[, "b.7"],
                                  beta8 = bayes_lasso_fit$beta[, "b.8"],
                                  beta9 = bayes_lasso_fit$beta[, "b.9"],
                                  beta10 = bayes_lasso_fit$beta[, "b.10"],                                  beta9 = mod$beta[, "b.1"],
                                  beta11 = bayes_lasso_fit$beta[, "b.11"],                                  beta9 = mod$beta[, "b.1"],
                                  beta12 = bayes_lasso_fit$beta[, "b.12"],
                                  variance = bayes_lasso_fit$s2, 
                                  lambda.square = bayes_lasso_fit$lambda2))
class(coef.lasso$beta1)

s <- summary(mod, burnin = 2)


colMedians(coef.lasso[-seq(BURN), -1])




### normal Lasso 

fit.glmnet <-  glmnet(as.matrix(player_train_x), player_train_y, 
                      lambda=cv.glmnet(as.matrix(player_train_x),
                                       player_train_y)$lambda.1se, alpha=1)
coef.glmnet <- coef(fit.glmnet)
sum(coef.glmnet == 0)

### ml schätzung


fit.lm <- lm(log_value ~ ., data = train)

sum(summary(fit.lm)$coef[,4] < 0.05)

aa <- summary(fit.lm)$coef


fit.glmnet <-  glmnet::glmnet(as.matrix(train_x), as.matrix(train_y), 
                      lambda = cv.glmnet(as.matrix(train_x),
                                         as.matrix(train_y))$lambda.1se, alpha=1)







summary.blasso(mod)

plot.blasso(mod)
mod$beta
mod





dim(glmnet_coef)



  
#prediction LASSO

y_hat_freq_las <- rep(glmnet_coef[1],length(test$log_wage))+
  glmnet_coef[which(row.names(glmnet_coef) == 'log_wage')]*test$log_wage+ 
  glmnet_coef[which(row.names(glmnet_coef) == 'age')]*test$age + 
  glmnet_coef[which(row.names(glmnet_coef) == 'height_cm')]*test$height_cm +
  glmnet_coef[which(row.names(glmnet_coef) == 'weight_kg')]*test$weight_kg +
  glmnet_coef[which(row.names(glmnet_coef) == 'overall')]*test$overall +
  glmnet_coef[which(row.names(glmnet_coef) == 'potential')]*test$potential +
  glmnet_coef[which(row.names(glmnet_coef) == 'shooting')]*test$shooting +
  glmnet_coef[which(row.names(glmnet_coef) == 'contract_valid_until')]*test$contract_valid_until +  
  glmnet_coef[which(row.names(glmnet_coef) == 'pace')]*test$pace +
  glmnet_coef[which(row.names(glmnet_coef) == 'passing')]*test$passing +
  glmnet_coef[which(row.names(glmnet_coef) == 'dribbling')]*test$dribbling +    
  glmnet_coef[which(row.names(glmnet_coef) == 'defending')]*test$defending   

##own RMSE Function
own_rmse  <-  function(y_hat, y) {
   rm_se <- sqrt(sum( ( (y_hat - y)^2)/length(y_hat) ))
   return(rm_se)
}

own_rmse(y_hat_freq_las, test$log_value)


#prediction Bayesian LASSO

HY 

bayes_lasso_fit$beta

rep(glmnet_coef[1],length(test$log_wage))+

    
  median(bayes_lasso_fit$mu)
  median(bayes_lasso_fit_hyper$mu)



  
y_hat_bay_las_hy <- rep(median(bayes_lasso_fit_hyper$mu), length(test$log_wage))+
  BA[1,1]*test$log_wage +
  BA[2,1]*test$age +
  BA[3,1]*test$height_cm +
  BA[4,1]**test$weight_kg +
  BA[5,1]*test$overall +
  BA[6,1]*test$potential +
  BA[7,1]*test$shooting +
  BA[8,1]*test$contract_valid_until +  
  BA[9,1]*test$pace +
  BA[10,1]*test$passing +
  BA[11,1]*test$dribbling +   
  BA[12,1]*test$defending   

own_rmse(y_hat_bay_las_hy , test$log_value)

