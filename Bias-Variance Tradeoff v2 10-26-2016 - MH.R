#Understanding Variance Bias Tradeoff: 
#10/16/2016 MH
# The goal of this exercise is to better underdstand the variance-bias tradeoff. 
# Conceptually, bias and variance are negatively correlated. Mistakenly, 
# it is tempting to try and eliminate any bias at the expense of high variance. 
# As we see in this exercise, it is sometimes worth introducing (some) bias in 
# exchange for tighter variance, which improves the overall model. 

# 1. Partition the data 
# 2. Build a simple linear regression around the train 
# 3. Test a few betas in the range of the estimated betas on the test dataset 
# 4. observe how the variance, bias, MSE, and beta changes graphically: 

# Load Packages: 
library(reshape2)
library(dplyr)
library(ggplot2)
library(tibble)
library(MASS)
library(plotly)
select <- dplyr::select

#Load and trim dataset: 
library(ggplot2movies)
data(movies)

movies <- 
  movies %>%  
  select(rating, length) %>%
  na.omit() %>% 
  # make row names explicit column
  tibble::rownames_to_column(var="id") %>% 
  # convert to tibble format: data frame, but that limits the number of rows
  # displayed when entered into console
  tbl_df() 

# Split datasets: 
set.seed(76)
train <- sample_frac(movies, 0.7)
test <- anti_join(movies, train, by="id")

# Build linear regression model around train: 
train_lr_model <- lm(rating ~ length, data=train)
coeffs = coefficients(train_lr_model)
coeffs 

# Apply Coefficients to testing dataset: 
p_00<- 
  test %>%  
  mutate (
    beta_0=coeffs[1], 
    beta_1=coeffs[2], 
    p=coeffs[1]+coeffs[2]*length, 
    MSE=ave((rating-p)^2), 
    var=ave((p-ave(p))^2), 
    bias_squared=MSE-var
    # Can't figure out formula for bias. The online formulas 
    # are local (at a given x point) but we need a value for the overall estimator.
  ) %>%  
  slice(1)

# Write up a function that applies different coeff to testing dataset:
test_mse <- 
  function(b_0, b_1) { 
    test  %>%  
      mutate (
        beta_0=b_0, 
        beta_1=b_1, 
        p=beta_0+beta_1*length, 
        MSE=ave((rating-p)^2), 
        var=ave((p-ave(p))^2), 
        bias_squared=MSE-var 
      ) %>%  
      slice (1)
        }

#Apply the function to different values of b_0 and b_1: (would love feedback 
#on how to do this bit in a cleaner way): 
for (j in 0:5) {
  for (i in 4:8) { 
    name <- paste("p", j, sep = "_")
    name <- paste (name, i, sep="")
    assign(name, test_mse(i, j))
  }
} 

  for (i in 4:8) { 
    name <- paste("p", 10, sep = "_")
    name <- paste (name, i, sep="")
    assign(name, test_mse(i, coeffs[2]))
  }

for (j in 0:5) { 
  name <- paste("p", j, sep = "_")
  name <- paste (name, 10, sep="")
  assign(name, test_mse(coeffs[1], j))
}

# Create df with all the possible betas and corr MSE values: 
tradeoff <- 
  bind_rows (p_00, p_04, p_05, p_06, p_07, p_08,
             p_14, p_15, p_16, p_17, p_18, 
             p_24, p_25, p_26, p_27, p_28, 
             p_34, p_35, p_36, p_37, p_38, 
             p_44, p_45, p_46, p_47, p_48, 
             p_54, p_55, p_56, p_57, p_58,  
             p_104, p_105, p_106, p_107, p_108, 
             p_110, p_210, p_310, p_410, p_510) %>%  
  tbl_df() %>% 
  select(-c(id, rating, length, p))

# Create matrices for 3-d plotting from the values: 
mse_matrix <- 
acast(tradeoff, beta_0~beta_1, value.var="MSE")

var_matrix <- 
  acast(tradeoff, beta_0~beta_1, value.var="var")

bias_matrix <- 
  acast(tradeoff, beta_0~beta_1, value.var="bias_squared")

#plot in three dimensions: 
plot_ly(z = ~var_matrix) %>% add_surface()
plot_ly(z = ~bias_matrix) %>% add_surface()
plot_ly(z = ~mse_matrix) %>% add_surface()
