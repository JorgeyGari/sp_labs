# Lab 5

# Checkpoint 1
# Let t=50 and a=10. We are interested in P(M(t)≥a).
# 1. Calculate this probability theoretically.
## Because we know that P(M(t)≥a) = 2P(B(t)≥a), we can use pnorm.
prob = 2 * (1 - pnorm(10, 0, sqrt(50)))

# 2. Simulate 10 thousand standard Brownian motion processes up to t=50
bm_std <- function(t, nsim=1, n=10000) {
  x <- seq(0, t, length.out=n+1)
  y <- replicate(nsim, c(0, cumsum(rnorm(n, 0, sqrt(t/n)))))
  cbind(x, y)
}
b <- bm_std(50, nsim=10000)
b <- b[, -1]

# 3. Compute P(M(t)≥a) empirically.
maxes = apply(b, 2, max)
length(which(maxes>=10)) / length(maxes)

# 4. Compute P(Ta≤t) empirically.
## Because the minimum and the maximum are the same, the value does not exist.
min(b[nrow(b),] == 10) == max(b[nrow(b),] == 10)

# 5. Compute 2P(B(t)≥a) empirically.
2*mean(b[nrow(b),] >= 10)

# Checkpoint 2
# Implement a function to simulate a Brownian bridge.
bb <- function(nsim=1, n=1000) {
  b <- bm_std(1, nsim, n)
  for (i in 2:ncol(b))
    for (j in 1:nrow(b))
      b[j, i] <- b[j, i] - (b[j, 1] * b[nrow(b), i])
  b
}

# 2. Plot 100 trajectories to verify that the simulation is correct.
plot_trajectories <- function(x, ...) {
  # plot first one in bold
  plot(x[, 1:2], type="l", ylim=range(x[, -1]), lwd=1)
  # plot the rest of them with thinner lines
  for (i in seq_len(ncol(x) - 2) + 2)
    lines(x[, 1], x[, i], type="l", lwd=0.1)
}
bridge <- bb(nsim=100)
plot_trajectories(bridge)

# 3. Using 10 thousand simulations, calculate empirically the probability that
# the process is between 0.5 and 0.75 in the middle of the bridge.

new_bridge <- bb(nsim=10000)
sum(0.5 < new_bridge[501,] & new_bridge[501,] < 0.75) / 10000
