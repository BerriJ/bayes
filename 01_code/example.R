load(url("http://bms.zeugner.eu/blog/images/2011-05/paneldat.rda"))


panelDat=as.matrix(panelDat)
head(panelDat)



library(monomvn); library(lars); library(glmnet); library(miscTools) 
data(diabetes); attach(diabetes)



length()

dim(fifa_data%>%
  filter(year == 19)%>%
  dplyr::select(year))



set.seed(42)
library(magrittr)
library(qwraps2)

# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

data(mtcars)

mtcars2 <-
  dplyr::mutate(mtcars,
                cyl_factor = factor(cyl,
                                    levels = c(6, 4, 8),
                                    labels = paste(c(6, 4, 8), "cylinders")),
                cyl_character = paste(cyl, "cylinders"))

our_summary1 <-
  list("Miles Per Gallon" =
         list("min" = ~ min(.data$mpg),
              "max" = ~ max(.data$mpg),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$mpg)),
       "Displacement" =
         list("min" = ~ min(.data$disp),
              "median" = ~ median(.data$disp),
              "max" = ~ max(.data$disp),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$disp)),
       "Weight (1000 lbs)" =
         list("min" = ~ min(.data$wt),
              "max" = ~ max(.data$wt),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$wt)),
  )


#https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html

AA <- 
  
  
  as.data.frame(fifa_data%>%dplyr::select(value_eur, wage_eur, overall))

class(AA)


sum_var <- papeR::summarise( as.data.frame(fifa_data%>%dplyr::select(value_eur, wage_eur, overall, year)), 
                   group = 'year', p_value = FALSE, quantil = FALSE, )
  
sum_var <- sum_var[,1:((dim(sum_var)[2])-2)]
  
knitr::kable(sum_var, digits = 2, caption = 'Summary', label = 'tab:sum', col.names = c('', 'year', '', 'N', ' ', 'mean', 'sd' ))

xtable(psych::describeBy(fifa_data%>%dplyr::select(value_eur, wage_eur, overall), 
                         fifa_data$year,fast = TRUE, range = FALSE))

?papeR::summarise()
?describe()

xtable(des_by_version)
knitr::kable(des_by_version)

mean(
  fifa_data%>%filter(year == 2019)%>%select(value_eur)
  )

 mean(fifa_data%>%dplyr::select(value_eur) , na.rm = TRUE )

dim(sum_var)

dim( knitr::kable(sum_var, digits = 2, caption = '\\label{tab:sum} Summary', 
              col.names = c('', 'year', '', 'N', ' ', 'mean', 'sd' )))
 
 knitr::kable(sum_var, digits = 2, caption = '\\label{tab:sum} Summary', 
              col.names = c('', 'year', '', 'N', ' ', 'mean', 'sd' ),
              row.names = FALSE )

 

  mean( fifa_data%>%dplyr::filter(year == 2019)%>%dplyr::select(value_eur) , na.rm = TRUE)
 
 class(fifa_data$value_eur )

aa <-  round(mean(fifa_data$value_eur[fifa_data$year ==2020]) -mean(fifa_data$value_eur[fifa_data$year ==2019]),2)  

pro <- aa/mean(fifa_data$value_eur[fifa_data$year ==2020]) 
 
(round(mean(fifa_data$value_eur[fifa_data$year ==2020]) -mean(fifa_data$value_eur[fifa_data$year ==2019]))  / mean(fifa_data$value_eur[fifa_data$year ==2020]),2)  
 
 
round((mean(fifa_data$wage_eur[fifa_data$year ==2020]) -mean(fifa_data$wage_eur[fifa_data$year ==2019]))/ sd(fifa_data$wage_eur[fifa_data$year ==2020]),2)

round((mean(fifa_data$overall[fifa_data$year ==2020]) -mean(fifa_data$overall[fifa_data$year ==2019]))/ sd(fifa_data$overall[fifa_data$year ==2020]),3)

names(fifa_data)
 
(mean(fifa_data$value_eur[fifa_data$year ==2020]) -mean(fifa_data$value_eur[fifa_data$year ==2019]))/ mean(fifa_data$value_eur[fifa_data$year ==2020])*100
 
(mean(fifa_data$value_eur[fifa_data$year ==2020]) -mean(fifa_data$value_eur[fifa_data$year ==2019])) 
 
plot_1 <- ggplot(data = fifa_data%>%filter(year == 2019), aes(value_eur))+
  geom_histogram(bins = 100, alpha = 0.5)+
  labs(title = "Histogram of Player Values", x = "Value in Euro", y = "Quantity")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.75))

plot_2 <- ggplot(data = fifa_data%>%filter(year == 2019), aes(log(value_eur)))+
  geom_histogram(bins = 100, alpha = 0.5)+
  labs(title = "Histogram of Player Values", x = "Value in Euro", y = "Quantity")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.75))

cowplot::plot_grid(plot_1, plot_2, align = "v", nrow = 2, rel_heights = c(1/4, 1/4, 1/2))


require(gridExtra)

plot1 <- qplot(1)
plot2 <- qplot(1)
cowplot::grid.arrange(arrangeGrob (plot_1, plot_2, ncol=1, nrow=2), heights = c(3,1))

install.packages('cowplot')
library(cowplot)

























 
  
