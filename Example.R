library(rangerts)
# to check the function ranger function helper
?rangerts::ranger

# load consumption data in the package
data <- rangerts::elec_data

# feature engineering
data$Time2 <- data$Time^2
data$TempTime <- data$Time*data$Temp
data$TempChauf <- pmin(0, data$Temp - 15)
data$TempChaufTime <- pmin(0, data$Temp - 15) * data$Time

noel <- which(abs(data$Day - 24) <= 3 & data$Month == 12)
consoNoel = vector("numeric", length(data$Time))
consoNoel[noel] = 1
data$consoNoel <- consoNoel
data$MonthF <- as.factor(data$Month)

# split train and test
df_train <- data %>%
  dplyr::filter(Test == 0) %>%
  dplyr::select(- Test)

df_test <- data %>%
  dplyr::filter(Test == 1) %>%
  dplyr::select(- Test)

# set general parameters
nb_trees <- 1000
mtry <- floor(sqrt(ncol(df_train)))
block_size <- 52

# Use case 1
# the default ranger with bootstrap i.i.d and with replacement
# thus the sample fraction is the default value = 1
rf_iid_rep <- rangerts::ranger(Load ~ ., data = df_train,
                               num.trees = nb_trees,
                               mtry = mtry,
                               replace = T,
                               seed = 1, # for reproductibility
                               activate.ts = F,
                               keep.inbag = T) # to keep trace of in-bag samples
# 679 observations in total
nrow(df_train)
#> [1] 679
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_iid_rep$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 429.52
# 429.52
# total number of inbag samples
purrr::map_dbl(rf_iid_rep$inbag.counts, sum) %>%
  mean()
#> [1] 679
# 679

# Use case 2
# the default ranger with bootstrap i.i.d and with replacement
# thus the sample fraction = 0.632
rf_iid <- rangerts::ranger(Load ~ ., data = df_train,
                           num.trees = nb_trees,
                           mtry = mtry,
                           replace = F,
                           seed = 1,
                           activate.ts = F,
                           keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_iid$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 429
# 429
# total number of inbag samples
purrr::map_dbl(rf_iid$inbag.counts, sum) %>%
  mean()
#> [1] 429
# 429

# Use case 3
# the nonoverlapping mode with replacement
# thus the sample fraction is the default value = 1
rf_no_rep <- rangerts::ranger(Load ~ ., data = df_train,
                              num.trees = nb_trees,
                              mtry = mtry,
                              replace = T, # default = T too
                              seed = 1, 
                              activate.ts = T,
                              block.size = block_size,
                              bootstrap.ts = "nonoverlapping",
                              keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_no_rep$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 439.266
# 439.266
# total number of inbag samples
purrr::map_dbl(rf_no_rep$inbag.counts, sum) %>%
  mean()
#> [1] 679
# 679

# Use case 4
# the nonoverlapping mode with replacement
# thus the sample fraction is the default value = 1
rf_no <- rangerts::ranger(Load ~ ., data = df_train,
                          num.trees = nb_trees,
                          mtry = mtry,
                          replace = F, # in this case, every sample in-bag is taken only once
                          seed = 1,
                          activate.ts = T,
                          block.size = block_size,
                          bootstrap.ts = "nonoverlapping",
                          keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_no$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 429
# 429
# total number of inbag samples
purrr::map_dbl(rf_no$inbag.counts, sum) %>%
  mean()
#> [1] 429
# 429


# Use case 5
# the moving mode with replacement
# thus the sample fraction is the default value = 1
rf_mv <- rangerts::ranger(Load ~ ., data = df_train,
                          num.trees = nb_trees,
                          mtry = mtry,
                          replace = T, # default = T too
                          seed = 1, 
                          activate.ts = T,
                          block.size = block_size,
                          bootstrap.ts = "moving",
                          keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_mv$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 430.157
# 430.157
# total number of inbag samples
purrr::map_dbl(rf_mv$inbag.counts, sum) %>%
  mean()
#> [1] 679
# 679

# Use case 6
# the stationary mode with replacement
# thus the sample fraction is the default value = 1
rf_st <- rangerts::ranger(Load ~ ., data = df_train,
                          num.trees = nb_trees,
                          mtry = mtry,
                          replace = T, # default = T too
                          seed = 1, 
                          activate.ts = T,
                          block.size = block_size,
                          bootstrap.ts = "stationary",
                          keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_st$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 446.788
# 446.788

# Use case 7
# the circular mode with replacement
# thus the sample fraction is the default value = 1
rf_cr <- rangerts::ranger(Load ~ ., data = df_train,
                          num.trees = nb_trees,
                          mtry = mtry,
                          replace = T, # default = T too
                          seed = 1, 
                          activate.ts = T,
                          block.size = block_size,
                          bootstrap.ts = "circular",
                          keep.inbag = T)
# the average number of different observations 
# that are at least taken in-bag once in the trees
purrr::map_int(rf_cr$inbag.counts, 
               ~ length(which(.x != 0))) %>%
  mean()
#> [1] 438.124
# 438.124

# final model list
model_list <- list(rf_iid, rf_iid_rep,
                   rf_no, rf_no_rep,
                   rf_mv, rf_st, rf_cr)

# compare rmse & mape
algo_spec <- c("iid without replacement", 
               "iid with replacement",
               "nonoverlapping without replacement",
               "nonoverlapping with replacement",
               "moving with replacement",
               "stationary with replacement",
               "circular with replacement"
)
rmse <- purrr::map_dbl(model_list, 
                       ~ yardstick::rmse_vec(df_test$Load, 
                                             predict(.x, df_test)$predictions))
cbind(algo_spec, round(rmse, 2))
#>      algo_spec                                     
#> [1,] "iid without replacement"            "2565.22"
#> [2,] "iid with replacement"               "2505.57"
#> [3,] "nonoverlapping without replacement" "2408.51"
#> [4,] "nonoverlapping with replacement"    "2384.21"
#> [5,] "moving with replacement"            "2516.38"
#> [6,] "stationary with replacement"        "2473.84"
#> [7,] "circular with replacement"          "2559.48"

mape <- purrr::map_dbl(model_list, 
                       ~ yardstick::mape_vec(df_test$Load, 
                                             predict(.x, df_test)$predictions))
cbind(algo_spec, round(mape, 2))
#>      algo_spec                                  
#> [1,] "iid without replacement"            "3.18"
#> [2,] "iid with replacement"               "3.13"
#> [3,] "nonoverlapping without replacement" "3.05"
#> [4,] "nonoverlapping with replacement"    "3.02"
#> [5,] "moving with replacement"            "3.11"
#> [6,] "stationary with replacement"        "3.08"
#> [7,] "circular with replacement"          "3.13"