# 碩一下 數統作業 EM algorithm

# 第一題:預設真實值 #
set.seed(12345)
n <- 100
beta <- runif(1,1,5) ; beta
tau <- runif(n,1,10)
head(tau) #預設顯示前6筆

# 第二題:產生兩組X與Y服從poisson隨機樣本 #
X <- rpois(n, tau)
head(X)
Y <- rpois(n, beta * tau)
head(Y)

# 第三題:估計beta & tau #
beta.hat <- sum(Y) / sum(X) ; beta.hat
tau.hat = (X + Y) / (1 + beta.hat)
head(tau.hat)

# 第四題:X遺漏第一筆觀察值 #
newX <- X[-1]
head(newX)
length(newX)

# 第五題:用EM演算法估計beta & tau  10^-6停止#
beta <- runif(1) ; beta
tau1 <- runif(1) ; tau1
iter <- 1
repeat{
  # E步驟，根據假設的tau來算beta
  beta = c(beta, sum(Y)/(tau1[iter] + sum(newX)))
  # M步驟，根據上面的beta再來計算tau
  tau1 = c(tau1, (tau1+Y[iter])/(1+beta[iter+1]))
  if (beta[iter+1] - beta[iter] < 1e-06 &
      tau1[iter+1] - tau1[iter] < 1e-06) break
  # 紀錄循環次數
  iter = iter + 1
}
print(beta)
print(tau1)

newY <- Y[-1]
new.beta.hat <- beta[3]
new.tau.hat <- (newX + newY) / (1 + new.beta.hat)#估計tau2~tau100
head(new.tau.hat) #print tau2~tau6
