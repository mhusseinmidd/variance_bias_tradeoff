library(dplyr)
library(ggplot2)

# Define/plot true functional form:
f <- function(x){
  4 + 25*x - 3*x^2
}

# Define irreducible error:
sigma <- 40

# Set up skeleton of training and test sets
n_training <- 10^4
n_test <- 10^4

training_data <- data_frame(
  id = 1:n_training,
  x = seq(1, 10, length=n_training),
  f_x = f(x)
)
test_data <- data_frame(
  id = 1:n_test,
  x = seq(1, 10, length=n_test),
  f_x = f(x)
)

# Plot one example of test set with true curve f(x) in red
test_data <- test_data %>%
  mutate(
    eps = rnorm(n_test, mean=0, sd=sigma),
    y = f_x + eps
  )
ggplot(test_data, aes(x, y)) +
  geom_point() +
  stat_function(fun = f, geom = "line", size=2, col="red")

# Choose the x_0 we are interested in at random. In this case the x value in the
# test set closest to 5
x_0 <- test_data %>%
  mutate(diff = abs(x - 5)) %>%
  filter(diff == min(diff)) %>%
  .[["x"]]

# Save values here
overall <- NULL

for(i in 1:10000){
  # Create new y's based on new irreducible noise
  test_data <- test_data %>%
    mutate(
      eps = rnorm(n_test, mean=0, sd=sigma),
      y = f_x + eps
    )
  training_data <- training_data %>%
    mutate(
      eps = rnorm(n_training, mean=0, sd=sigma),
      y = f_x + eps
    )

  # Train model
  train_first_model <- lm(y~x, data=training_data)

  # Make predictions
  test_data <- test_data %>%
    mutate(f_x_hat = predict(train_first_model, newdata=test_data))

  # Save predictions only for X = x_0
  overall <- test_data %>%
    filter(x == x_0) %>%
    select(-id, -x, -eps) %>%
    bind_rows(overall)

  if(i %% 100 == 0){print(i)}
}

# Look at performance for X = x_0 only!
results <- overall %>%
  summarise(
    MSE = mean( (y-f_x_hat)^2 ),
    bias_squared = ( mean(f_x - f_x_hat) )^2,
    var = mean(f_x_hat^2) - (mean(f_x_hat))^2,
    irreducible = sigma^2
  )

results
results$MSE
results$bias_squared + results$var + results$irreducible
