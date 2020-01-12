source("packages/packages.R")
# Data wrangling 

data_19 <- readr::read_csv(here::here('00_data/career_mode/players_19.csv'))
data_20 <- readr::read_csv(here::here('00_data/career_mode/players_20.csv'))

#### datsa manupilation ####

# creating a yearly variable dummy 

year <-as.data.frame(as.factor(c(rep(2019, nrow(data_19)),
                                 rep(2020, nrow(data_20)))))
colnames(year)[1] <- "year"

# hanging the different classes of the different data frames to merge these
data_19 <- data_19 %>%
  mutate_if(. , is.numeric, as.character) 

data_20 <- data_20 %>% 
  mutate_if(. , is.numeric, as.character)

# bind the data frames 
fifa_data <- bind_rows(data_19, data_20, .id = NULL)%>%
  bind_cols( year)%>%
  dplyr::select(-(player_traits:rb)) # getting rid of some unnecessary variables 


fifa_data <- fifa_data %>%
  mutate(age = as.numeric(age),                 #changing the class agian 
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
  )%>% 
  filter( # deleting infinite values
    log_value > -Inf,
    log_wage > -Inf,
  )


vars <- c('log_wage', 'log_value', 'age', 'height_cm', 'weight_kg', 'overall',
          'potential', 'shooting', 'contract_valid_until', 'pace', 'shooting', 
          'passing', 'dribbling', 'defending')

train <- fifa_data %>%
  dplyr::filter(year == 2019)%>%
  dplyr::select(vars)%>%
  imputeTS::na_replace(., 0) # replace the NA's  (Bayesian) lasso can not handle NA's


test <- fifa_data %>%
  dplyr::filter(year == 2020)%>%
  dplyr::select(vars)%>%
  imputeTS::na_replace(., 0)

train_y <- train%>%
  dplyr::select(log_value)

train_x <- train %>%
  dplyr::select(-log_value)




#### model 

IT <- 100
BURN <- IT *0.25


tictoc::tic()
bayes_lasso_fit <- blasso(as.matrix(train_x), as.matrix(train_y), 
                          T = IT ,  thin = BURN ,  lambda2 = 10)
tictoc::toc()

tictoc::tic()
bayes_lasso_fit_hyper <- blasso(as.matrix(train_x), as.matrix(train_y), 
                          T = IT ,  thin = BURN, lambda2 = 10, rd = c(1, 1.78))
tictoc::toc()


save(bayes_lasso_fit, bayes_lasso_fit_hyper, file = here::here('04_output/bayes_lasso_fit.RData'))
