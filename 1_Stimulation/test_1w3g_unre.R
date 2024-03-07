library('MASS')
target_f <- 0.20

# initial mean
initial_mu <- c(1, 2, 3)

sd <- 1

# 定义计算 f 的函数
calculate_f <- function(mu) {
  sqrt(sum((mu - mean(mu)) ^ 2) / length(mu)) / sd
}
determ_mu <- optim(par = initial_mu, fn = function(mu) abs(calculate_f(mu) - target_f))
mu <- determ_mu$par

calculate_f(mu)



n <- 100

iter <- 1000
f_value <- numeric(iter)
for (i in 1:iter) {

  a <- rnorm(n = n, mean = mu[1], sd = sd)#the initial a group
  b <- rnorm(n = n, mean = mu[2], sd = sd)
  c <- rnorm(n = n, mean = mu[3], sd = sd)
  dv <- c(a, b, c)
  groups <- factor(rep(c('a','b','c'), each = n))
  df <- data.frame(dv, groups)
  m <- aov(dv~groups, df)
  f <- effectsize::cohens_f(m, partial = FALSE)
  f_value[i] <- f$Cohens_f
}
hist(f_value)
