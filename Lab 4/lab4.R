library(simmer)
library(simmer.plot)

# Checkpoint 1

lambda <- 0.4         # Arrival rate
mu <- c(0.1, 1)       # Service rate (car, m/c)
p <- 0.8              # Probability of car

A <- matrix(
  c(-mu[1], 1, 0,       (1-p)*mu[1],     0,
    lambda, 1, mu[1],   0,               0,
    0,      1, -lambda, (1-p)*lambda,    0,
    0,      1, mu[2],   -(lambda + mu[2]), lambda,
    0,      1, 0,       (1-p)*mu[2],     -mu[2]),
  byrow=TRUE, ncol=5)

P <- solve(t(A), c(0, 1, 0, 0, 0))
N_average_theor <- sum(P * c(2, 1, 0, 1, 2))
N_average_theor

car <- trajectory() %>%
  seize("pump", amount=1) %>%
  timeout(function() rexp(1, mu[1])) %>%
  release("pump", amount=1)

mcycle <- trajectory() %>%
  seize("pump", amount=1) %>%
  timeout(function() rexp(1, mu[2])) %>%
  release("pump", amount=1)

gas.station <- simmer() %>%
  # One server, queue with max. 1
  add_resource("pump", capacity=1, queue_size=1) %>%
  # Car generator of rate = p * lambda
  add_generator("car", car, function() rexp(1000, p*lambda)) %>%
  # Motorcycle generator of rate = (1 - p) * lambda
  add_generator("mcycle", mcycle, function() rexp(1000, (1-p)*lambda)) %>%
  run(1000/lambda)

gas.station.res <- get_mon_resources(gas.station)
plot(gas.station.res, metric="usage", names="pump", items="system") + 
  geom_hline(yintercept = N_average_theor)




# Checkpoint 2

lambda <- 0.75
mu <- 1/2

queue <- trajectory() %>%
  seize("atm", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("atm", amount=1)

env <- simmer() %>%
  add_resource("atm", capacity=3, queue_size=5) %>%
  add_generator("arrival", queue, function() rexp(1000, lambda)) %>%
  run(mu*1000/lambda)

df.res <- get_mon_resources(env)
df.arr <- get_mon_arrivals(env)
t_system <- df.arr$end_time - df.arr$start_time


rej_rate <- sum(!df.arr$finished) / nrow(df.arr)
rej_rate

effective = (1 - rej_rate) * lambda
effective
average = effective * mean(t_system)
average

plot(df.res, metric="usage", "atm", items="system") + 
  geom_hline(yintercept = (1 - rej_rate) * lambda * mean(t_system))

N = effective * mean(t_system)

# Checkpoint 3

lambda3 = 200
mu3 = 20

calls <- trajectory() %>%
  seize("channel", amount=1) %>%
  timeout(function() rexp(1, mu3)) %>%
  release("channel", amount=1)

rejection = 1
i = 0

while(rejection > 0.05) {
  i = i +1
  env2 <- simmer() %>%
    add_resource("channel", capacity=2*i, queue_size=0) %>%
    add_generator("arrival", calls, function() rexp(1000, lambda3)) %>%
    run(mu3*1000/lambda3)
  df.res <- get_mon_resources(env2)
  df.arr <- get_mon_arrivals(env2)
  rejection <- sum(!df.arr$finished) / nrow(df.arr)
  i
}

rejection = 1
j = 0
while(rejection > 0.05) {
  j = j +1
  env3 <- simmer() %>%
    add_resource("channel", capacity=3*j, queue_size=0) %>%
    add_generator("arrival", calls, function() rexp(1000, lambda3)) %>%
    run(mu3*1000/lambda3)
  df.res <- get_mon_resources(env3)
  df.arr <- get_mon_arrivals(env3)
  rejection <- sum(!df.arr$finished) / nrow(df.arr)
  j
}

i * 10000
j * 15000

# Lambda keeps decreasing because of surface area
