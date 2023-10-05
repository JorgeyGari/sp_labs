## Lab 1 (05/10/2023)
# Laura Belizón Merchán - 100452273
# Jorge Lázaro Ruiz     - 100452172

library(markovchain)
library(expm)

## Checkpoint 1
# Consider the following Markov chain.
# Construct a new markovchain object for this diagram.
P1 <- matrix(data = c(0.5, 0.5, 0,   0,
                     0.5, 0.5, 0,   0,
                     0.2, 0.2, 0.2, 0.4,
                     0,   0,   0,   1),
            ncol = 4, byrow = TRUE)

chain <- new("markovchain", name = "Checkpoint 1",
             states = c("1", "2", "3", "4"),
             transitionMatrix = P1)


# Simulation and estimation:
# Generate 1000 random observations from this chain.
obs <- rmarkovchain(1000, chain)

# Using these observations, fit a new markovchain object from the data.
chainFit <- markovchainFit(obs)

# Review the estimation. Plot the fitted Markov chain.
chainFit$estimate
plot(chainFit$estimate)

# What differences are there with respect to the original? Why? (Repeat the generation & estimation process several times if you need more clues about what’s happening).
summary(chain)
summary(chainFit$estimate)
## 


# Convergence:
# Show its stationary distribution using the proper method.
steadyStates(chain)

# How many are there? Why?
## There are two, because the chain is not ergodic and there are two communication classes.

# What is the rate of convergence (i.e. the rate of decay of the log-error)?

# Error function
# @param n is the time (one value or a vector of values)
# @param mc is the Markov chain
err <- function(n, mc) {
  # Check that the input values are positive integers
  if (!isTRUE(all(n > 0 & n == floor(n))))
    stop("'n' must only contain positive integer values")
  
  # Reserve some memory for the result
  res <- numeric(length(n))
  # For each n, calculate err(n)
  for (i in 1:length(n)) {
    Pn <- (mc^n[i])@transitionMatrix
    Pn1 <- (mc^(n[i]-1))@transitionMatrix
    res[i] <- sum(abs(Pn - Pn1))
  }
  return(res)
}

# Showing the error function
x <- 1:10
y <- log(err(x, chain))
plot(x, y, type="o")
fit <- lm(y ~ x)
plot(y)
abline(fit$coefficients, lty=2, col="red")
fit$coefficients
exp(fit$coefficients[2])  # Ratio of convergence

## Checkpoint 2
# The cat and mouse problem. Suppose we have five adjacent boxes, numbered from 1 to 5. Initially, there is a cat in box 1 and a mouse in box 5. For each clock tick, both animals jump into one of the adjacent boxes with uniform probability. When the two of them land in the same box, the game is over…

# Describe this game using a Markov chain with states given by the pair of the animals’ positions.
# Obtain the transition matrix and generate a markovchain object.
P2 <- matrix(data = c(0, 1, 0, 0,
                      1/4, 0, 1/2, 1/4,
                      0, 1/2, 0, 1/2,
                      1, 0 ,0 , 0),
             ncol = 4, byrow = TRUE
             )

cat <- new("markovchain", name = "Cat & Mouse",
          states = c("A", "B", "C", "F"), transitionMatrix = P2)


# Compute the average duration for this game:
# using first-step analysis;
n <- 100; trials <- 100
nF <- replicate(trials, match("F", rmarkovchain(n, cat, t0 = "A")))
c(mean(nF, na.rm=TRUE))

# by simulation (provide a confidence interval using e.g. t.test);
t.test(nF)

# using the proper method provided by the package.
meanFirstPassageTime(cat)
