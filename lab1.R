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
# Assume a random variable, X^M, which is defined as the maximum of three independent random variables, each one uniformly distributed in [0,1]
# X^M=max{U(0,1),U(0,1),U(0,1)}
# Compute the median and plot the density function of X^M, and compare it vs. the corresponding values obtained via analysis. 

N <- 1000
trials <- 1000
out <- sapply(1:trials, function(i) {
  u1 <- runif(N, 0, 1)
  u2 <- runif(N, 0, 1)
  u3 <- runif(N, 0, 1)
  return(pmax(u1, u2, u3))
})
median = median(out)
den = density(out)  # Value calculated by hand is approx. 0.793
plot(den)
f = function(x) 3*x^2  # Function calculated by hand
curve(f(x), add=TRUE, lty=2, col="red")
