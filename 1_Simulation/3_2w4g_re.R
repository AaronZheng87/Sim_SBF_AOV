library("MASS")
library(tidyverse)
# install.packages("effectsize", repos = "https://easystats.r-universe.dev/")
library(effectsize)
############ main effect #################
######## 2* 2 with design* #######


sd <- 1
k <- 2
r <- 0.1
d_2x2_w <- function(initial_mu, sd, k, r) {
  mu_A <- (initial_mu[1] + initial_mu[2]) - (initial_mu[3] + initial_mu[4]) # "A1_B1", "A1_B2", "A2_B1", "A2_B2
  var_A <- 2^k * sd^2 * (1 - r)
  d <- mu_A / sqrt(var_A)
  return(d)
}
target_d <- 0.5
# 定义目标函数
objective_function <- function(initial_mu) {
  d_2x2_w(initial_mu, sd, k, r)
}

# 设置初始值
initial_mu <- c(1, 1, 1, 1) # A1B1, A1B2, A2B1, A2B2

# 使用optim函数最大化目标函数
result <- optim(par = initial_mu, fn = function(initial_mu) abs(objective_function(initial_mu) - target_d))
eta_to_d <- function(eta2, N) {
  sqrt(eta2 * (N - 1) / (N - eta2 * N))
}
mu <- result$par

N <- 100
iter <- 500
d_value <- numeric(iter)
d_ef_value <- numeric(iter)
eta2_value <- numeric(iter)
eta2_to_d_value <- numeric(iter)

for (i in 1:iter) {
  rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))
  data <- mvrnorm(n = N, mu = mu, Sigma = rho)
  colnames(data) <- c("A1_B1", "A1_B2", "A2_B1", "A2_B2")
  df <- data.frame(data)
  df["subj_idx"] <- c(1:N)
  df <- df %>%
    pivot_longer(cols = starts_with("A"), names_to = "group") %>%
    tidyr::separate(group, c("A", "B"))


  mu_groups <- df %>%
    group_by(A, B) %>%
    summarise(m = mean(value)) %>%
    ungroup() %>%
    pull()

  d <- d_2x2_w(mu_groups, sd, k, r)
  model <- afex::aov_ez(id = "subj_idx", dv = "value", within = c("A", "B"), data = df)
  eta2 <- effectsize::eta_squared(model, partial = TRUE)$Eta2_partial[1]
  d_ef <- effectsize::repeated_measures_d(df$value[df$A == "A1"], df$value[df$A == "A2"])$d_rm
  d_value[i] <- d
  d_ef_value[i] <- d_ef
  eta2_value[i] <- eta2
  eta2_to_d_value[i] <- eta_to_d(eta2, 100)
}
hist(d_value)
hist(d_ef_value)
hist(eta2_value)
hist(eta2_to_d_value)
mean(eta2_to_d_value)
mean(eta2_value)
mean(d_value)
mean(d_ef_value)


sd <- 1
k <- 2

r <- 0.1
d_2x2_w_inx <- function(initial_mu, sd, k, r) {
  mu_A <- (initial_mu[1] - initial_mu[2]) - (initial_mu[3] - initial_mu[4]) # "A1_B1", "A1_B2", "A2_B1", "A2_B2
  var_A <- 2^k * sd^2 * (1 - r)
  d <- mu_A / sqrt(var_A)
  return(d)
}
target_d <- 0.5
# 定义目标函数
objective_function <- function(initial_mu) {
  d_2x2_w_inx(initial_mu, sd, k, r)
}

# 设置初始值
initial_mu <- c(1, 1, 1, 1) # A1B1, A1B2, A2B1, A2B2

# 使用optim函数最大化目标函数
result <- optim(par = initial_mu, fn = function(initial_mu) abs(objective_function(initial_mu) - target_d))
eta_to_d <- function(eta2, N) {
  sqrt(eta2 * (N - 1) / (N - eta2 * N))
}
mu <- result$par



N <- 100
iter <- 500
d_value <- numeric(iter)
d_ef_value <- numeric(iter)
eta2_value <- numeric(iter)
eta2_to_d_value <- numeric(iter)

for (i in 1:iter) {
  rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))
  data <- mvrnorm(n = N, mu = mu, Sigma = rho)
  colnames(data) <- c("A1_B1", "A1_B2", "A2_B1", "A2_B2")
  df <- data.frame(data)
  df["subj_idx"] <- c(1:N)
  df <- df %>%
    pivot_longer(cols = starts_with("A"), names_to = "group") %>%
    tidyr::separate(group, c("A", "B"))
  
  
  mu_groups <- df %>%
    group_by(A, B) %>%
    summarise(m = mean(value)) %>%
    ungroup() %>%
    pull()
  
  d <- d_2x2_w_inx(mu_groups, sd, k, r)
  model <- afex::aov_ez(id = "subj_idx", dv = "value", within = c("A", "B"), data = df)
  eta2 <- effectsize::eta_squared(model, partial = TRUE)$Eta2_partial[3]
  
  d_value[i] <- d
  eta2_value[i] <- eta2
  eta2_to_d_value[i] <- eta_to_d(eta2, 100)
}
hist(d_value)
hist(eta2_value)
hist(eta2_to_d_value)
mean(eta2_to_d_value)
mean(eta2_value)
mean(d_value)











