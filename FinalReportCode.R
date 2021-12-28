# 碩一下 STAT_computing 期末報告

#跑一次的結果#
#set.seed(12345) 
install.packages("‘fANCOVA") #安裝wild bootstrap 套件
library(fANCOVA)

n = 100 ; nu = 4 ; beta0 = -1 ; beta1 = 1 ; beta2 = 2 ; sig = 1
x1 = rnorm(n, 0, 1)
x2 = rnorm(n, 1, 1)
y <- beta0 + beta1*x1 + beta2*x2 + sig*rt(n,nu)
head(y) #取得資料前6筆資料(預設是 6)
y1 <- lm(y ~ x1 + x2) ; y1

y1.hat <- y1$fitted.values
y1.resid <- y1$residuals
res1 <- sample(y1.resid, replace = T)
y1.star1 <- y1.hat + res1
data1 <- data.frame(y1.star1, x1, x2)
y2 <- lm(y1.star1 ~ x1 + x2) ; y2

res.boot <- wild.boot(y1.resid, nboot=1)
res2 <- sample(res.boot, replace = T)
y1.star2 <- y1.hat + res2
data2 <- data.frame(y1.star2, x1, x2)
y3 <- lm(y1.star2 ~ x1 + x2) ; y3


#殘差重抽
B <- 1000
m.boot <- matrix(NA, B, 3)
for(i in 1:B){
  data123 <- data1[sample(1:n, size = n, replace = T), ]
  model.new <- lm(y1.star1 ~ x1 + x2, data = data123)
  m.boot[i,] <- coef(model.new)
}
head(m.boot)

hist(m.boot[,1], main = expression(beta[0]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.boot[,1], c(0.025,0.975)) )
quantile(m.boot[,1], c(0.025,0.975))

hist(m.boot[,2], main = expression(beta[1]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.boot[,2], c(0.025,0.975)) )
quantile(m.boot[,2], c(0.025,0.975))

hist(m.boot[,3], main = expression(beta[2]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.boot[,3], c(0.025,0.975)) )
quantile(m.boot[,3], c(0.025,0.975))


#wild bootstrap重抽
B <- 1000
m.wildboot <- matrix(NA, B, 3)
for(i in 1:B){
  data123 <- data2[sample(1:n, size = n, replace = T), ]
  model.new <- lm(y1.star2 ~ x1 + x2, data = data123)
  m.wildboot[i,] <- coef(model.new)
}
head(m.wildboot)

hist(m.wildboot[,1], main = expression(beta[0]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.wildboot[,1], c(0.025,0.975)) )
quantile(m.wildboot[,1], c(0.025,0.975))

hist(m.wildboot[,2], main = expression(beta[1]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.wildboot[,2], c(0.025,0.975)) )
quantile(m.wildboot[,2], c(0.025,0.975))

hist(m.wildboot[,3], main = expression(beta[2]), breaks = "scott", prob = T, col = "magenta" )
abline( v = quantile(m.wildboot[,3], c(0.025,0.975)) )
quantile(m.wildboot[,3], c(0.025,0.975))


#跑500次模擬殘差迴圈#
#殘差
res.boot.funtion = function(B){
  m.boot <- matrix(NA, B, 3)
  for(i in 1:B){
    data123 <- data1[sample(1:n, size = n, replace = T), ]
    model.new <- lm(y1.star1 ~ x1 + x2, data = data123)
    m.boot[i,] <- coef(model.new)
  }
  A <- matrix(0, B, 3)
  for (i in 1:3){
    A[, i] <- ifelse((m.boot[, i] >= quantile(m.boot[, i], .025) &
                        m.boot[, i] <= quantile(m.boot[, i], .975)), 1, 0)
  }
  m = matrix(0, 3, 2)
  for (i in 1:3){
    m[i, 1] = quantile(m.boot[, i], .025)
    m[i, 2] = quantile(m.boot[, i], .975)
    colnames(m) <- c("2.5%", "97.5%")
    rownames(m) <- c("beta0", "beta1", "beta2")
  }
  Rate = t(t(apply(A, 2, mean))) ; Rate
  matrix <- cbind(m, Rate)
  colnames(matrix) <- c( "2.5%", "97.5%", "Coverage Rate")
  return(matrix)
}
res.boot.funtion(1000)

simulations_res.boot.funtion = function(n, B){
  z = replicate(n, res.boot.funtion(B))
  mm = matrix(0, 3, 3)
  mm[1, 1] = mean(z[1, 1, ])
  mm[1, 2] = mean(z[1, 2, ])
  mm[1, 3] = mean(z[1, 3, ])
  mm[2, 1] = mean(z[2, 1, ])
  mm[2, 2] = mean(z[2, 2, ])
  mm[2, 3] = mean(z[2, 3, ])
  mm[3, 1] = mean(z[3, 1, ])
  mm[3, 2] = mean(z[3, 2, ])
  mm[3, 3] = mean(z[3, 3, ])
  colnames(mm) <- c("2.5%", "97.5%", "Coverage Rate")
  rownames(mm) <- c("beta0", "beta1", "beta2")
  return(mm)
}
#simulations_res.boot.funtion(500, 1000)


wild.boot.funtion = function(B){
  m.boot <- matrix(NA, B, 3)
  for(i in 1:B){
    data123 <- data2[sample(1:n, size = n, replace = T), ]
    model.new <- lm(y1.star2 ~ x1 + x2, data = data123)
    m.boot[i,] <- coef(model.new)
  }
  A <- matrix(0, B, 3)
  for (i in 1:3){
    A[, i] <- ifelse((m.boot[, i] >= quantile(m.boot[, i], .025) &
                        m.boot[, i] <= quantile(m.boot[, i], .975)), 1, 0)
  }
  m = matrix(0, 3, 2)
  for (i in 1:3){
    m[i, 1] = quantile(m.boot[, i], .025)
    m[i, 2] = quantile(m.boot[, i], .975)
    colnames(m) <- c("2.5%", "97.5%")
    rownames(m) <- c("beta0", "beta1", "beta2")
  }
  Rate = t(t(apply(A, 2, mean))) ; Rate
  matrix <- cbind(m, Rate)
  colnames(matrix) <- c( "2.5%", "97.5%", "Coverage Rate")
  return(matrix)
}
wild.boot.funtion(1000)

simulations_wild.boot.funtion = function(n, B){
  z = replicate(n, wild.boot.funtion(B))
  mm = matrix(0, 3, 3)
  mm[1, 1] = mean(z[1, 1, ])
  mm[1, 2] = mean(z[1, 2, ])
  mm[1, 3] = mean(z[1, 3, ])
  mm[2, 1] = mean(z[2, 1, ])
  mm[2, 2] = mean(z[2, 2, ])
  mm[2, 3] = mean(z[2, 3, ])
  mm[3, 1] = mean(z[3, 1, ])
  mm[3, 2] = mean(z[3, 2, ])
  mm[3, 3] = mean(z[3, 3, ])
  colnames(mm) <- c("2.5%", "97.5%", "Coverage Rate")
  rownames(mm) <- c("beta0", "beta1", "beta2")
  return(mm)
}
#simulations_wild.boot.funtion(500, 1000)

#計算程式執行時間#
test.time <- proc.time()
simulations_res.boot.funtion(500, 1000)
simulations_wild.boot.funtion(500, 1000)
proc.time() - test.time
#約莫執行了17分鐘