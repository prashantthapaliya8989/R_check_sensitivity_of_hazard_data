library(sp)
library(rgdal)
library(glue)

wd = setwd("D:/ICIMOD/Additional Data/others/2021/VARA/Shortcut/Recent R")
source("utils/get_column_name.R")
source("utils/get_column_value_cli_geo.R")
source("utils/get_column_value_ac_ex_sen.R")
source("utils/get_normalized_value.R")
source("utils/get_min_max.R")
source("utils/get_avg.R")
source("utils/get_sd_and_compute_climate.R")
source("utils/get_sd_and_compute_geogenic.R")
source("utils/array_rank_transform.R")
source("utils/get_rank_class.R")
source("utils/get_rank_class_cum.R")

########## CLIMATE ############
climate <- ("shp/kailash_sb_climate.shp")

climate_column <- get_column_name(climate)
climate_column_value <- get_column_value_cli_geo(climate, climate_column)
climate_min_max <- get_min_max(climate_column_value)
climate_norm <- get_normalized_value(climate_column_value, climate_min_max)
climate_avg <- get_avg(climate_norm)
climate_min_max1 <- get_min_max(climate_avg)
climate_norm1 <- get_normalized_value(climate_avg, climate_min_max1)
climate_norm1

######### GEOGENIC ############
geogenic <- ("shp/kailash_sb_geogenic.shp")

geogenic_column <- get_column_name(geogenic)
geogenic_column_value <- get_column_value_cli_geo(geogenic, geogenic_column)
#Filter the values that are less than 0.02
filter <- list()
filter[[1]] <- geogenic_column_value[[1]]
for (i in 2:length(geogenic_column_value)) {
  for (index in 1:length((geogenic_column_value[[1]]))) {
    if (geogenic_column_value[[i]][index] < 0.02) {
      geogenic_column_value[[i]][index] <- 0
    }
  }
}
geogenic_min_max <- get_min_max(geogenic_column_value)
geogenic_norm <- get_normalized_value(geogenic_column_value, geogenic_min_max)
geogenic_avg <- get_avg(geogenic_norm)
geogenic_min_max1 <- get_min_max(geogenic_avg)
geogenic_norm1 <- get_normalized_value(geogenic_avg, geogenic_min_max1)
geogenic_norm1

########## ADAPTIVE CAPACITY #########
ac <- ("shp/kailash_sb_AC.shp")

ac_column <- get_column_name(ac)
ac_column_value <- get_column_value_ac_ex_sen(ac, ac_column)

########## Exposure #########
ex <- ("shp/kailash_sb_exposure.shp")

ex_column <- get_column_name(ex)
ex_column_value <- get_column_value_ac_ex_sen(ex, ex_column)

########## Sensitivity #########
sen <- ("shp/kailash_sb_sensitivity.shp")

sen_column <- get_column_name(sen)
sen_column_value <- get_column_value_ac_ex_sen(sen, sen_column)

## To calculate the combined rank
all_combine <- function(cli, geog, ac, ex, sens) {
  final_score <- list()
  final_score[[1]] <- cli[[1]]
  empty <- c()
  for (i in 1:length(cli[[1]])) {
    avg = (unlist(cli[[2]][i]) + unlist(geog[[2]][i]))/2
    product = round( avg * (5-unlist(ac[[2]][i])) * unlist(ex[[2]][i]) * unlist(sens[[2]][i]), digits=3)
    empty <- append(empty, product)
  }
  final_score[[2]] <- empty
  return (final_score)
}
score <- all_combine(climate_norm1, geogenic_norm1, ac_column_value, ex_column_value, sen_column_value)
score_min_max <- get_min_max(score)
score_norm <- get_normalized_value(score, score_min_max)

## Filtering the entity without exposure
score_norm_filter <- list()
for (i in 1:length(score_norm)) {
  empty <- c()
  for (index in 1:length(score_norm[[1]])) {
    if (score_norm[[2]][index] != 0) {
      empty <- append(empty, unlist(score_norm[[i]][index]))
    }
    
    score_norm_filter[[i]] <- empty 
  }
}
## Rank the normalized value
rank <- list()
rank[[1]] <- score_norm_filter[[1]]

rank[[2]] <- unlist(array_rank_transform(score_norm_filter[[2]]))
# write.table(as.data.frame(rank),file="mylist1.csv",sep=",",row.names=F)

###### Computing each original value of climate and geogenic by 1 SD #########
num <- 100#as.integer(readline(prompt = "Enter the number of check: "))

#initialize class variable
clss = c(20,40,60,80)
# clss_input <- as.integer(readline(prompt = paste(
#   "{Example: for 4 input value, there will be 5 classses\n",
#   "[20,30,40,50] = <20, 20-30, 30-40, 40-50, >50}\n",
#   "Enter the number of classses: ", sep="")))
# clss <- c()
# for (i in 1:clss_input) {
#   val = as.integer(readline(prompt = glue("Enter the {i} class: ")))
#   clss <- append(clss, val)
# }
clss <- append(clss, 99999)

clss_val <- list()
clss_val_cum <- list()            #For Cumulative class
empty = c()
empty_cum = c()
for (index in 1:length(rank[[1]])) {
  empty <- append(empty, 0)
  empty_cum <- append(empty_cum, 0)
}
for (i in 1:length(clss)) {
  # em = empty
  # em_cum = duplicated(empty_cum)
  clss_val[[i]] <- empty
  clss_val_cum[[i]] <- empty_cum
}

ptm <- proc.time()
for (j in 1:num) {
  ######## CLIMATE ########
  climate_compute_sd <- get_sd_and_compute_climate(climate_column_value)
  climate_min_max <- get_min_max(climate_compute_sd)
  climate_norm <- get_normalized_value(climate_compute_sd, climate_min_max)
  climate_avg <- get_avg(climate_norm)
  climate_min_max1 <- get_min_max(climate_avg)
  climate_norm1 <- get_normalized_value(climate_avg, climate_min_max1)
  
  
  ####### GEOGENIC #######
  geogenic_compute_sd <-get_sd_and_compute_geogenic(geogenic_column_value)
  geogenic_min_max <- get_min_max(geogenic_compute_sd)
  geogenic_norm <- get_normalized_value(geogenic_compute_sd, geogenic_min_max)
  geogenic_avg <- get_avg(geogenic_norm)
  geogenic_min_max1 <- get_min_max(geogenic_avg)
  geogenic_norm1 <- get_normalized_value(geogenic_avg, geogenic_min_max1)
  
  
  
  ## To calculate the combined rank
  all_combine <- function(cli, geog, ac, ex, sens) {
    final_score <- list()
    final_score[[1]] <- cli[[1]]
    empty <- c()
    for (i in 1:length(cli[[1]])) {
      avg = (unlist(cli[[2]][i]) + unlist(geog[[2]][i]))/2
      product = round( avg * (5-unlist(ac[[2]][i])) * unlist(ex[[2]][i]) * unlist(sens[[2]][i]), digits=3)
      empty <- append(empty, product)
    }
    final_score[[2]] <- empty
    return (final_score)
  }
  score <- all_combine(climate_norm1, geogenic_norm1, ac_column_value, ex_column_value, sen_column_value)
  score_min_max <- get_min_max(score)
  score_norm <- get_normalized_value(score, score_min_max)
  ## Filtering the entity without exposure
  score_norm_filter <- list()
  for (i in 1:length(score_norm)) {
    empty <- c()
    for (index in 1:length(score_norm[[1]])) {
      if (ex_column_value[[2]][index] != 0) {
        empty <- append(empty, unlist(score_norm[[i]][index]))
      }
     }  
    score_norm_filter[[i]] <- empty
  }
  length(score_norm_filter[[2]])
  new_rank = unlist(array_rank_transform(score_norm_filter[[2]]))
  rank[[length(rank)+1]] <- new_rank
  diff <- c()
  for (i in 1:length(score_norm_filter[[2]])) {
    value <- abs(rank[[2]][i] - new_rank[[i]])
    diff <- append(diff, value)
  }
  rank[[length(rank)+1]] <- diff

  for (i in 1:length(diff)) {
    ret_clss <- get_rank_class(diff[[i]],clss)
    clss_val[[ret_clss]][i] <- clss_val[[ret_clss]][i]+1
    
    ret_clss_cum = get_rank_class_cum(diff[[i]],clss)
    for (j in 1:length(ret_clss_cum)) {
      val <- ret_clss_cum[[j]]
      clss_val_cum[[val]][i] <- clss_val_cum[[val]][i]+1
    }
  }
  
}

column_names_class <- c("Subbasin")
for(i in 1:length(clss_val)+1) {
  if (i == 2){
    column_names_class[[i]] <- glue("<{clss[[i-1]]}")
  } else if (i == length(clss_val)+1) {
    column_names_class[[i]] <- glue(">{clss[[i-2]]}")
  } else {
    column_names_class[[i]] <- glue("{clss[[i-2]]}-{clss[[i-1]]}")
  }
}
class_df <- data.frame(rank[[1]], clss_val)
colnames(class_df) <- column_names_class
write.table(class_df,file="class.csv",sep=",",row.names=F)

column_names_class_cum <- c("Subbasin")
for(i in 1:length(clss_val_cum)+1) {
  column_names_class_cum[[i]] <- glue('0-{clss[[i-1]]}')
}
class_cum_df <- data.frame(rank[[1]], clss_val_cum)

colnames(class_cum_df) <- column_names_class_cum
write.table(class_cum_df,file="class_cum.csv",sep=",",row.names=F)

column_names_rank <- c()
for (i in 1:length(rank)) {
  if (i == 1) {
    column_names_rank[[i]] <- "Subbasin"
  } else if (i == 2) {
    column_names_rank[[i]] <- "Original_Rank"
  } else if (i%%2 != 0) {
    column_names_rank[[i]] <- glue('rank{(i-1)/2}')
  } else {
    column_names_rank[[i]] <- glue('diff{(i/2)-1}')
  }
}

rank_df <- data.frame(rank)
colnames(rank_df) <- column_names_rank
write.table(rank_df, file="rank.csv",sep=",",row.names=F)

proc.time() - ptm
