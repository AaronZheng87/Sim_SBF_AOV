library('MASS')
library(BayesFactor)
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

bf_upper_boundary <- 10#upper boundary of bf
bf_lower_boundary <- 1/10#lower boundary of bf

sim_n <- 1000#the simulation number
bf <- numeric(sim_n)#an empty list of bf value
sample_size <- numeric(sim_n)#an empty list of sample size of each iteration
final_bf <- NA#initial final_bf in while loop
final_n <- NA#initial final_n in while loop

rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))


for (j in 1:sim_n) {
  
  bf_val <- c()
  
  i <- 0  # 每次迭代前重置 i
  n <- 5#number of initial participants number
  end_n <- 100 #the largest number of participants
  n_update <- 1#add 1 participant per group
  
  data <- mvrnorm(n=n, mu=c(0, 0, 0), Sigma=rho)
  data[,1] <- data[,1] + mu[1]
  data[,2] <- data[,2] + mu[2]
  data[,3] <- data[,3] + mu[3]
  DV <- c(data[,1],data[,2],data[,3])
  subj <- seq(1:n)
  groups <- factor(rep(c('a','b','c'), each = n))
  subj_idx <- factor(rep(subj, times = 3))
  data <-data.frame(subj_idx,groups,DV)
  
  while(n < end_n){
    i = i + 1# update i
    n = n + n_update# update n
    
    
    data_new <- mvrnorm(n=n_update, mu=c(0, 0, 0), Sigma=rho)
    data_new[1] <- data_new[1] + mu[1]
    data_new[2] <- data_new[2] + mu[2]
    data_new[3] <- data_new[3] + mu[3]
    DV_new <- c(data_new[1],data_new[2],data_new[3])
    groups_new <- factor(rep(c('a','b','c'), each = n_update))
    subj_new <- length(unique(data$subj_idx))+n_update
    subj_idx_new <- factor(rep(subj_new, times = 3))
    data_new <-data.frame(subj_idx_new,groups_new,DV_new)
    colnames(data_new) <- c("subj_idx", "groups", "DV")
    data <- rbind(data_new, data)
    print(paste0("当前行数: ", nrow(data)))
    print(i)

    m <- generalTestBF(DV ~ groups*subj_idx-subj_idx:groups, data = data, 
                       whichRandom = 'subj_idx', whichModels = 'top')
    bf_val[i] <- 1/extractBF(m)[1,1]
    
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

bf
sample_size
