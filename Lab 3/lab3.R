## Lab 3 (24/10/2023)
# Laura Belizón Merchán - 100452273
# Jorge Lázaro Ruiz     - 100452172
## Checkpoint 1

x <- rexp(1000, 1/5)
y <- rexp(1000, 1/10)
z <- pmin(x, y)

# Analyze the properties and distribution of the new random variable Z

 # Plot the logarithm of the survival function.

Fz <- ecdf(z)
plot(z, log(1-Fz(z)))

 # Estimate the λ parameter with a linear regression,
 # and compare the result against the expected value.

logCCDF <- log(1 - Fz(z))
z <- z[is.finite(logCCDF)]
logCCDF <- logCCDF[is.finite(logCCDF)]

fit <- lm(logCCDF ~ z)
summary(fit)

  # λ is -Estimate(z), we expected λ_1 + λ_2 = 0.3
  # The estimation was accurate

 # Estimate Pr(X<Y) with the generated samples.
 # Hint: it is related to the number of times a value from X is chosen instead
 # of a value of Y when generating Z∼min{X,Y}

xIsMin = 0
for (i in 1:length(z))
  if (z[i] == x[i]) {
    xIsMin = xIsMin + 1
  }
prob_xIsMin = xIsMin / 999

 # Confirm that Z does not have memory, by taking those samples larger than the
 # median, substracting the median from this subset of values, and then
 # characterizing the resulting distribution (with graphical tools).

z2 <- z[z>median(z)] - median(z)
qqplot(z, z2)
abline(0, 1, lty=2, col="red")


## Checkpoint 2

 # Merge two independent Poisson processes of rate 10 and 5 following a similar
 # procedure to the one above. What is the approx. number of elements of the
 # resulting vector? Why?

lambda1 <- 10
inter1 <- rexp(1000, lambda1)
times1 <- cumsum(inter1)

lambda2 <- 5
inter2 <- rexp(1000, lambda2)
times2 <- cumsum(inter2)

times1 <- times1[times1 < max(times2)]
times2 <- times2[times2 < max(times1)]
times_agg <- sort(c(times1, times2))
inter_agg <- diff(times_agg)

length(inter_agg)

  # We expected 1500 because one of the Poisson processes has half the rate
  # of the other one.

 # Sample the vector obtained above so that the resulting vector is of rate 10,
 # and compare it vs. the original arrival process at rate 10.

prob <- 2/3
times_dec <- subset(times_agg, runif(length(times_agg)) < prob)
inter_dec <- diff(times_dec)
qqplot(inter_dec, inter1)
abline(0, 1, lty=2, col="red")

## Checkpoint 3
# A switch connects 64 Voice over IP (VoIP) phones. The time between frames
# generated by a VoIP phone can be modeled with a random variable uniformly
# distributed between 8 and 12 ms. Analyze the frame generation process when
# all phones are active. To this aim:
  
 # Generate a vector that emulates the total arrival process from the 64 active phones.

phones <- matrix(0, nrow = 64, ncol = 100)

for (i in 1:64) {
  phones[i, ] <- cumsum(runif(100, 8, 12))
}

 # Analyze the interarrival times of the generated vector,

phones_agg <- sort(phones)
inter_agg <- diff(phones_agg)

 # and compare it vs. an exponential distribution.

qqplot(inter_agg, rexp(100, 1/mean(inter_agg)))
abline(0, 1, lty = 2, col="red")


 # Check whether it has memory.

inter_agg2 <- inter_agg[inter_agg>median(inter_agg)] - median(inter_agg)
qqplot(inter_agg, inter_agg2)
abline(0, 1, lty=2, col="red")

  # It has memory because the slope of the linear regression doesn't match anymore.

 # Is this a Poisson process? Why? 

  # Yes, because the slope still matches.

 # What is the total arrival rate?

1/mean(phones_agg)

