library(BayesFactor)
# 目标 f 值
#target cohen'f value
target_f <- 0.15

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

sim_n <- 2000#the simulation number
n <- 5#number of initial participants number
end_n <- 3000 #the largest number of participants
n_update <- 1
a <- rnorm(n = n, mean = mu[1], sd = sd)
b <- rnorm(n = n, mean = mu[2], sd = sd)
c <- rnorm(n = n, mean = mu[3], sd = sd)
dv <- c(a, b, c)
groups <- factor(rep(c('a','b','c'), each = n))
df <- data.frame(dv, groups)
nrow(df)


# a_new <- rnorm(n = n_update, mean = mu[1], sd = sd)
# b_new <- rnorm(n = n_update, mean = mu[2], sd = sd)
# c_new <- rnorm(n = n_update, mean = mu[3], sd = sd)
# dv_new <- c(a_new, b_new, c_new)
# groups_new <- factor(rep(c('a','b','c'), each = n_update))
# df_new <- data.frame(dv_new, groups_new)
# colnames(df_new) <- c("dv", "groups")
# df <- rbind(df_new, df)
bf_val <- c()
final_n <- c()
final_bf <- c()
i <- 0
while(n < end_n){
  i = i + 1
  n = n+n_update
  a_new <- rnorm(n = n_update, mean = mu[1], sd = sd)
  b_new <- rnorm(n = n_update, mean = mu[2], sd = sd)
  c_new <- rnorm(n = n_update, mean = mu[3], sd = sd)
  dv_new <- c(a_new, b_new, c_new)
  
  groups_new <- factor(rep(c('a','b','c'), each = n_update))
  df_new <- data.frame(dv_new, groups_new)
  colnames(df_new) <- c("dv", "groups")
  df <- rbind(df_new, df)
  
  print(paste0("current rows: ", nrow(df)))
  m <- anovaBF(dv~groups, data=df)
  bf_val[i] <- extractBF(m)[,1]

  print(i)
  print(bf_val[i])
  if (bf_val[i] < 1/10 || bf_val[i] > 10) {
    final_bf <- bf_val[i]
    final_n <- n
    break
  }
}







