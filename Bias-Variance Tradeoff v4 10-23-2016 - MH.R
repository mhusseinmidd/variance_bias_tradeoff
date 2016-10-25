#Understanding Variance Bias Tradeoff: 
#10/16/2016 MH
# The goal of this exercise is to better underdstand the variance-bias tradeoff. 
# One way to measure error is through MSE, which is the addition of 
# variance and bias squared of an estimator.What we will show is that it is 
# sometimes worth introducing (some) bias in exchange for tighter variance, 
# which, on balance, improves the overall model by having a lower MSE.

#To explore this trade off, we will: 
# 1. Partition the data 
# 2. Build a simple linear regression around the train dataset
# 3. Introduce more flexibility to the model, which increases variances and d
#    decreases bias 
# 4. observe how the bias, variance, and MSE change graphically: 

# Load Packages: ra
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
  select(budget, length) %>%
  na.omit() %>% 
  mutate(length=log(length)) %>% 
  # make row names explicit column
  tibble::rownames_to_column(var="id") %>% 
  # convert to tibble format: data frame, but that limits the number of rows
  # displayed when entered into console
  tbl_df() 

# Split datasets: 
set.seed(76)
train <- sample_frac(movies, 0.7)
test <- anti_join(movies, train, by="id")

# Ignore a linear regression model for the purposes of this exercise. 

# Build second-order polynomial: 
train_second_model <- lm(budget ~ length+I(length^2), data=train)
coeffs2 = coefficients(train_second_model)
coeffs2

# Build third-order polynomial: 
train_third_model <- lm(budget ~ length+I(length^2)+I(length^3), data=train)
coeffs3 = coefficients(train_third_model)
coeffs3

# Build fourth-order polynomial: 
train_fourth_model <- lm(budget ~ length+I(length^2)+I(length^3)+I(length^4),
                         data=train)
coeffs4 = coefficients(train_fourth_model)
coeffs4


# Apply Coefficients to testing dataset: 
# Write up a function that applies different coeff to testing dataset:
test_mse <- 
  function(coeffs) { 
    test  %>%  
      mutate (
        beta_0=coeffs[1], 
        beta_1=coeffs[2], 
        p=beta_0+beta_1*length, 
        MSE=ave((budget-p)^2), 
        var=ave((p-ave(p))^2), 
        bias_squared=MSE-var 
      ) %>%  
      slice (1)
        }

# Apply the function to models:
# Tried this but it did not work. Is there a way you can change a variable name 
# in a manner consistent with an index? For example, we know that the index i 
# takes on the values 1 through 5. And the variable names are coeffs1, coeffs2 ...
# How do you get the name of the variable to change each time the index changes? 
# 
#   for (i in 1:5) { 
#     name <- paste ("name", i, sep="")
#     assign(name, test_mse(paste('coeffs', i, sep="")))
#   }

# Brute force: 
p_02 <- 
  test_mse(coeffs2) 
p_03 <- 
  test_mse(coeffs3) 
p_04 <- 
  test_mse(coeffs4) 


# Create df with all the possible betas and corr MSE values: 
tradeoff <- 
  bind_rows (p_02, p_03, p_04) %>%  
  tbl_df() %>% 
  select(-c(id, budget, length, p)) %>% 
  mutate (flexibility=seq(1:3))

#Graph how MSE, Bias, and variance change with Values of Beta: 
ggplot (tradeoff) +
  geom_line(aes (flexibility, MSE)) + 
  geom_point(aes (flexibility, MSE)) +
  geom_line(aes(flexibility, var, color='red')) + 
  geom_point(aes (flexibility, var, color='red')) +
  geom_line(aes (flexibility, bias_squared, color='green')) +
  geom_point(aes (flexibility, bias_squared, color='green')) +
  labs(title="Bias-Variance Tradeoff in MSE", 
       y="") + 
  scale_x_discrete(limit = c(1, 2, 3),
                   labels = c("Quadratic","Cubic","Fourth-Poly"))
