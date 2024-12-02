library(tidyverse)

day2 <- read_tsv("day2/day2.txt", col_names = F)

sequences_list_raw <- str_split(day2$X1, " ")
inc <- list()
dec <- list()
diff <- list()
sum_F_inc <- list()
sum_F_dec <- list()
sum_diff <- list()
for(i in 1:length(sequences_list)){

  int_diff <- list()
  inc_i <- list()
  dec_i <- list()
  sequences_list <- sequences_list_raw
  for(j in 1:length(sequences_list[[i]])-1){
    inc_i[j] <- map2(sequences_list[[i]][j], sequences_list[[i]][j+1], ~ as.numeric(.x) < as.numeric(.y))
    
    if(isFALSE(unlist(inc_i[j]))) {
      sequences_list[[i]][j+1] <- sequences_list[[i]][j]
    }
  }
  sequences_list <- sequences_list_raw
  for(j in 1:length(sequences_list[[i]])-1){
    dec_i[j] <- map2(sequences_list[[i]][j], sequences_list[[i]][j+1], ~ as.numeric(.x) > as.numeric(.y))
   
    if(isFALSE(unlist(dec_i[j]))) {
      sequences_list[[i]][j+1] <- sequences_list[[i]][j]
    }
  }
  sequences_list <- sequences_list_raw
  for(j in 1:length(sequences_list[[i]])-1){
    
    int_diff[j] <- map2(sequences_list[[i]][j], sequences_list[[i]][j+1], ~ abs(as.numeric(.x) - as.numeric(.y)))
  }
  
  inc[[i]] <- all(inc_i == T)
  sum_F_inc[[i]] <- sum(inc_i == F)
  
  dec[[i]] <- all(dec_i == T)
  sum_F_dec[[i]] <- sum(dec_i == F)
  
  diff[[i]] <- any(int_diff > 3) | any(int_diff < 1)
  sum_diff[[i]] <- sum(int_diff > 3) + sum(int_diff < 1)
  
}

day2_mod <-
  tibble(
    values = day2$X1,
    only_inc = unlist(inc),
    inc_fixable = unlist(sum_F_inc), 
    only_dec = unlist(dec),
    dec_fixable = unlist(sum_F_dec), 
    diff_condition = unlist(diff),
    diff_fixable = unlist(sum_diff)
  ) 

result <- 
  day2_mod %>%
  filter((only_inc == T | only_dec == T) & diff_condition == F)

result2 <- 
  day2_mod %>%
  filter(
    (
      (only_inc == T | (only_inc == F & sum_F_inc == 1)) | 
      (only_dec == T | (only_dec == F & sum_F_dec == 1))
    ) & 
    (
      diff_condition == F | (diff_condition == T & diff_fixable == 1)
    )
  )