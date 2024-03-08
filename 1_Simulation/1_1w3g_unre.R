library(BayesFactor)
# 目标 f 值
# target cohen'f value
target_f <- 0.10

# initial mean
initial_mu <- c(1, 2, 3)

sd <- 1

# 定义计算 f 的函数
calculate_f <- function(mu) {
  sqrt(sum((mu - mean(mu))^2) / length(mu)) / sd
}


# 使用 optim 函数进行优化
# using optim function to update the mu value
determ_mu <- optim(par = initial_mu, fn = function(mu) abs(calculate_f(mu) - target_f))
mu <- determ_mu$par

calculate_f(mu)

bf_upper_boundary <- 10 # upper boundary of bf
bf_lower_boundary <- 1 / 10 # lower boundary of bf

sim_n <- 500 # the simulation number




bf <- numeric(sim_n) # an empty list of bf value
sample_size <- numeric(sim_n) # an empty list of sample size of each iteration
final_bf <- NA # initial final_bf in while loop
final_n <- NA # initial final_n in while loop
for (j in 1:sim_n) {
  bf_val <- c()

  i <- 0 # 每次迭代前重置 i
  n <- 5 # number of initial participants number
  end_n <- 100 # the largest number of participants
  n_update <- 1 # add 1 participant per group

  a <- rnorm(n = n, mean = mu[1], sd = sd) # the initial a group
  b <- rnorm(n = n, mean = mu[2], sd = sd)
  c <- rnorm(n = n, mean = mu[3], sd = sd)
  dv <- c(a, b, c)
  groups <- factor(rep(c("a", "b", "c"), each = n))
  df <- data.frame(dv, groups)

  while (n < end_n) {
    i <- i + 1 # update i
    n <- n + n_update # update n
    a_new <- rnorm(n = n_update, mean = mu[1], sd = sd) # add new n_update
    b_new <- rnorm(n = n_update, mean = mu[2], sd = sd)
    c_new <- rnorm(n = n_update, mean = mu[3], sd = sd)
    dv_new <- c(a_new, b_new, c_new)

    groups_new <- factor(rep(c("a", "b", "c"), each = n_update))
    df_new <- data.frame(dv_new, groups_new)
    colnames(df_new) <- c("dv", "groups")
    df <- rbind(df_new, df)

    print(paste0("当前行数: ", nrow(df)))
    m <- anovaBF(dv ~ groups, data = df) # one way anova, same result with lmBF
    bf_val[i] <- extractBF(m)[, 1]

    print(i)
    print(bf_val[i])
    # if lower or higher than the boundaries, break
    if (bf_val[i] < bf_lower_boundary || bf_val[i] > bf_upper_boundary) {
      final_bf <- bf_val[i]
      final_n <- n
      break
    }
  }

  # 修正向量索引
  bf[j] <- final_bf
  sample_size[j] <- final_n
}



sum(na.omit(bf) > 10) / sim_n # power
sample_size
bf
