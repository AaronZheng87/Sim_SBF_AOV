library('MASS')
library(afex)
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


r <- 0.5

rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))

n <- 100

iter <- 1000
f_value <- numeric(iter)

for (i in 1:iter) {
  data <- mvrnorm(n=n, mu=c(0, 0, 0), Sigma=rho)
  data[,1] <- data[,1] + mu[1]
  data[,2] <- data[,2] + mu[2]
  data[,3] <- data[,3] + mu[3]
  DV <- c(data[,1],data[,2],data[,3])
  subj <- seq(1:n)
  groups <- factor(rep(c('a','b','c'), each = n))
  subj_idx <- factor(rep(subj, times = 3))
  data <-data.frame(subj_idx,groups,DV)
  
  
  m <- afex::aov_ez(id = "subj_idx", dv = "DV", within = "groups", data = data)
  
  f <- effectsize::cohens_f(m, partial = FALSE)
  f_value[i] <- f$Cohens_f
}

hist(f_value)
