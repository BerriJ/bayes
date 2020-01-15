# plot of residuals vs fitted



#linear model
lm_plot_data <- as.data.frame(cbind(fit_lm$residuals, fit_lm$fitted.values))
colnames(lm_plot_data) <- c('residuals' ,'fitted_values')

pl_lin_res <- ggplot(data = lm_plot_data, aes(fitted_values, residuals))+
    geom_point(colour = "blue", fill = "blue", alpha = 0.1, size = 0.5) +
    labs(title = "Scatterplot Residuals vs fitted Values", x = "Fitted Values", y = "Residuals ")+
    theme_minimal()+
    theme(aspect.ratio = 9/16, plot.title = element_text(hjust = 0.5), plot.margin=unit(c(1,1,1.5,1.2),"cm"))+ 
    geom_hline(yintercept = 0, linetype = 'dashed', size = 0.8)

ggsave(here::here('04_output/pl_lin_res.png'), plot = pl_lin_res,
       scale = 1, width = 20, height = 20*(9/16), units = 'cm',
       dpi = 600, limitsize = FALSE)


#in sample predictions


#prediction LASSO

fitted_va_lasso <- rep(glmnet_coef[1],length(train$log_wage))+
  glmnet_coef[which(row.names(glmnet_coef) == 'log_wage')]*train$log_wage+ 
  glmnet_coef[which(row.names(glmnet_coef) == 'age')]*train$age + 
  glmnet_coef[which(row.names(glmnet_coef) == 'height_cm')]*train$height_cm +
  glmnet_coef[which(row.names(glmnet_coef) == 'weight_kg')]*train$weight_kg +
  glmnet_coef[which(row.names(glmnet_coef) == 'overall')]*train$overall +
  glmnet_coef[which(row.names(glmnet_coef) == 'potential')]*train$potential +
  glmnet_coef[which(row.names(glmnet_coef) == 'shooting')]*train$shooting +
  glmnet_coef[which(row.names(glmnet_coef) == 'contract_valid_until')]*train$contract_valid_until +  
  glmnet_coef[which(row.names(glmnet_coef) == 'pace')]*train$pace +
  glmnet_coef[which(row.names(glmnet_coef) == 'passing')]*train$passing +
  glmnet_coef[which(row.names(glmnet_coef) == 'dribbling')]*train$dribbling +    
  glmnet_coef[which(row.names(glmnet_coef) == 'defending')]*train$defending   


###predicting Bayesian Lasso

fitted_va_bayes <- rep(median(bayes_lasso_fit$mu), length(train$log_wage))+
  BA[1,1]*train$log_wage +
  BA[2,1]*train$age +
  BA[3,1]*train$height_cm +
  BA[4,1]**train$weight_kg +
  BA[5,1]*train$overall +
  BA[6,1]*train$potential +
  BA[7,1]*train$shooting +
  BA[8,1]*train$contract_valid_until +  
  BA[9,1]*train$pace +
  BA[10,1]*train$passing +
  BA[11,1]*train$dribbling +   
  BA[12,1]*train$defending   



###predicting Bayesian Lasso with hyper perameter

fitted_va_bayes_hy <- rep(median(bayes_lasso_fit_hyper$mu), length(train$log_wage))+
  HY[1,1]*train$log_wage +
  HY[2,1]*train$age +
  HY[3,1]*train$height_cm +
  HY[4,1]**train$weight_kg +
  HY[5,1]*train$overall +
  HY[6,1]*train$potential +
  HY[7,1]*train$shooting +
  HY[8,1]*train$contract_valid_until +  
  HY[9,1]*train$pace +
  HY[10,1]*train$passing +
  HY[11,1]*train$dribbling +   
  HY[12,1]*train$defending   


save(fitted_va_lasso, fitted_va_bayes, fitted_va_bayes_hy, file = here::here('04_output/fitted.RData'))


#lasso

lasso_plot_data <- as.data.frame(cbind( ( train_y - fitted_va_lasso     ), fitted_va_lasso ))
colnames(lasso_plot_data) <- c('residuals' ,'fitted_values')

pl_las_res <- ggplot(data = lasso_plot_data, aes(fitted_values, residuals))+
    geom_point(colour = "blue", fill = "blue", alpha = 0.1, size = 0.5)+
    labs(title = "Scatterplot Residuals vs fitted Values", x = "Fitted Values", y = "Residuals ")+
    theme_minimal()+
    theme(aspect.ratio = 9/16, plot.title = element_text(hjust = 0.5), plot.margin=unit(c(1,1,1.5,1.2),"cm"))+ 
    geom_hline(yintercept = 0, linetype = 'dashed', size = 0.8)

ggsave(here::here('04_output/pl_las_res.png'), plot = pl_las_res,
       scale = 1, width = 20, height = 20*(9/16), units = 'cm',
       dpi = 600, limitsize = FALSE)



##bayes 

bayes_plot_data <- as.data.frame(cbind((train_y - fitted_va_bayes), fitted_va_bayes))
colnames(bayes_plot_data) <- c('residuals' ,'fitted_values')

pl_bay_res <- ggplot(data = bayes_plot_data, aes(fitted_values, residuals))+
    geom_point(colour = "blue", fill = "blue", alpha = 0.1, size = 0.5)+
    labs(title = "Scatterplot Residuals vs fitted Values", x = "Fitted Values", y = "Residuals ")+
    theme_minimal()+
    theme(aspect.ratio = 9/16, plot.title = element_text(hjust = 0.5), plot.margin=unit(c(1,1,1.5,1.2),"cm"))+ 
    geom_hline(yintercept = 0, linetype = 'dashed', size = 0.8)

ggsave(here::here('04_output/pl_bay_res.png'), plot = pl_bay_res,
       scale = 1, width = 20, height = 20*(9/16), units = 'cm',
       dpi = 600, limitsize = FALSE)


pl_bay_res_dens <- ggplot(data = bayes_plot_data, aes(residuals))+
  geom_density(colour = "blue", fill = "blue", alpha = 0.3)+
  labs(title = "Density distribution of the residuals", x = "Residuals", y = "Density")+
  theme_minimal()+
  theme(aspect.ratio = 9/16, plot.title = element_text(hjust = 0.5), plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave(here::here('04_output/pl_bay_res_dens.png'), plot = pl_bay_res_dens,
       scale = 1, width = 20, height = 20*(9/16), units = 'cm',
       dpi = 600, limitsize = FALSE)

##hyper 

bayes_hy_plot_data <- as.data.frame(cbind((test_y - y_hat_bay_las_hy), y_hat_bay_las_hy ))
colnames(bayes_hy_plot_data) <- c('residuals' ,'fitted_values')

pl_bay_hy_res <- ggplot(data = bayes_hy_plot_data, aes(fitted_values, residuals))+
    geom_point(colour = "blue", fill = "blue", alpha = 0.1, size = 0.5)+
    labs(title = "Scatterplot Residuals vs fitted Values", x = "Fitted Values", y = "Residuals ")+
    theme_minimal()+
    theme(aspect.ratio = 9/16, plot.title = element_text(hjust = 0.5), plot.margin=unit(c(1,1,1.5,1.2),"cm"))+ 
    geom_hline(yintercept = 0, linetype = 'dashed', size = 0.8)

ggsave(here::here('04_output/pl_bay_hy_res.png'), plot = pl_bay_res,
       scale = 1, width = 20, height = 20*(9/16), units = 'cm',
       dpi = 600, limitsize = FALSE)



save(pl_lin_res, pl_las_res, pl_bay_res, pl_bay_hy_res, file = here::here('04_output/plots_residuals.RData'))
