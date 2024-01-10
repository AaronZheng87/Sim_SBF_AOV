library(effectsize)
# 目标 f 值
#target cohen'f value
target_f <- 0.1

# initial mean
initial_mu <- c(1, 2, 3)

sd <- 1

# 定义计算 f 的函数
calculate_f <- function(mu) {
  sqrt(sum((mu - mean(mu)) ^ 2) / length(mu)) / sd
}


# 使用 optim 函数进行优化
# using optim function to update the mu value
determ_mu <- optim(par = initial_mu, fn = function(mu) abs(calculate_f(mu) - target_f))
mu <- determ_mu$par

calculate_f(mu)
N = 1000000
a <- rnorm(n = N, mean = mu[1], sd = sd)
b <- rnorm(n = N, mean = mu[2], sd = sd)
c <- rnorm(n = N, mean = mu[3], sd = sd)
dv <- c(a, b, c)

groups <- factor(rep(c('a','b','c'), each = N))
m <- aov(dv~groups)
effectsize::cohens_f(m)

