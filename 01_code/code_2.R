#install.packages('rjson')
install.packages('jsonlite')
library(rjson)
library(jsonlite)
library(dplyr)

#

data_18 <- readr::read_csv('00_data/CompleteDataset_fifa_18.csv')
data_19 <- readr::read_csv('00_data/data_fifa_19.csv')

df_list_18.19 <- lapply(data_18.19, as.data.frame)
df_18.19 <- dplyr::bind_rows(df_list_18.19)
df_18.19 <- as.data.frame(cbind(player_name_18.19,df_18.19))

df_list_17.18 <- lapply(data_17.18, as.data.frame)
df_17.18 <- dplyr::bind_rows(df_list_17.18)
df_17.18 <- as.data.frame(cbind(player_name_17.18,df_17.18))
sort(names(df_17.18))


head(df)

sort(names(df))

as.data.frame(data_18.19[[1]])
df[2, ] == data_18.19[[2]]$`Passes per match`
length(names(data_17.18))
length(names(data_18.19))
#data wrangling

arguments_17.18 <- rep(NA, length(data_17.18))

i = 1
for (i in 1:length(data_17.18)){
  arguments_17.18[i] <- length(names(data_17.18[[i]]))
}
max(arguments_17.18)
min(arguments_17.18)


a <- NA
arguments_18.19 <- length(data_18.19)

for (i in 1:length(data_18.19)){
  arguments_18.19[i] <- length(names(data_18.19[[i]]))
  d <- names(data_18.19[[i]])
  a <-c(a,d)
}

ARGUMENTS <- unique(a)
max(arguments_18.19)
min(arguments_18.19)


data_19 <- c(data_18.19[[1]]$Club, data_18.19[[1]]$Position,  data_18.19[[1]]$`Duels Won`)

names_18.19 <- names(data_18.19)[[which.max(arguments_18.19)]]

all_arguments_17.18 <- names(data_17.18[[which.max(arguments_17.18)]])
all_arguments_18.19 <- names(data_18.19[[which.max(arguments_18.19)]])

same <- ifelse(all_arguments_17.18 == all_arguments_18.19, 1,0)

player_name_17.18 <- names(data_17.18)
player_name_18.19 <- names(data_18.19)

##### max of  arguments is the same

##writing a data frame from the json file

mat_data_17.18 <- matrix(NA, nrow = length(player_name_17.18), ncol = (length(arguments)+1)) 
mat_data_17.18[,1] <- player_name_17.18

mat_data_18.19 <- matrix(NA, nrow = length(player_name_18.19), ncol = (length(arguments)+1)) 
mat_data_18.19[,1] <- player_name_18.19


club <- matrix(NA, nrow  = length(player_name_17.18), ncol = length(arguments))


for (j in 1:3) {
  for (i in 1:length(data_17.18)) {
    #IF backward... 
    c <- as.character(data_17.18[[i]][j])
    club[i,j] <- c 
  }
}

colnames(club) <- arguments

#equals
arguments[j] == names(data_17.18[[i]])[j]


###chunk


lst <- list(data_16, data_17, data_18, data_19, data_20)


convert_all_columns_to_character <- function(list_of_dataframes, varname){
  
  varnames <- rep_len(varname, length(list_of_dataframes))
  
  convert_to_character <- function(dataframe, variable){
    dataframe[[variable]] <- as.character(dataframe[[variable]])
    return(dataframe)
  }
  list_of_dataframes <- map2(list_of_dataframes, varnames, convert_to_character)
  return(list_of_dataframes)
}

lst <- convert_all_columns_to_character(lst,  "mentality_composure")

lst <- convert_all_columns_to_character(lst,  "goalkeeping_handling")

dataframes <- bind_rows(list_of_dataframes)


load('00_data/FIFA_bereinigt-4.Rda')

