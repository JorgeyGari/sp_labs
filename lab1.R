## Lab 1 (14/09/2023)
# Laura Belizón Merchán - 100452273
# Jorge Lázaro Ruiz     - 100452172

## Checkpoint 1
# The addition of two independent Poisson random variables, with average λ1 and λ2, respectively, is another Poisson variable, which we denote as Pois(λ)
# Search R’s help to find out how to generate n observations of a Poisson random variable with parameter λ.
  # We found out that the function is rpois(n, lambda)

# Generate 1000 observations from a random variable X∼Pois(1)
x = rpois(1000, 1)

# Generate 1000 observations from a random variable Y∼Pois(3)
y = rpois(1000, 3)

# Compute the addition Z=X+Y and confirm that the mean is close to the expected value.
z = x + y

# Generate the Q-Q plot corresponding to the comparison of Z vs. a Poisson random of the expected average. Comment on the differences observed as compared against the case of continuous random variables.
qqplot(z, rpois(length(z), mean(z)))
mean(z)  # This mean should be 1 + 3 = 4 because Z = X + Y, but it's not.
abline(0, 1, lty=2, col='red')

  # Although there are 1000 observations, the plot appears to have much fewer.
  # This is because our case is discrete, hence the points are overlapping.


## Checkpoint 2

