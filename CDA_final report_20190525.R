# 類別期末報告

# 讀取資料
babies = read.table("D:/babies.txt", header = T) 

# 計算各變數遺失值數目 # 有些觀察值不只有一個NA值 #共83個NA值
sapply(babies, function(x)su = sum(is.na(x)))

babies = na.exclude(babies) #剔除62筆觀察值

# 敘述性統計
boxplot(babies)
summary(babies)

quantile(babies$bwt, .15)

bwt2 = (babies$bwt <= 101) * 1
babies$bwt2 = as.vector(bwt2) #將分類變數轉為 factor
sum(babies$bwt2) #184的1 990個0


# 沒有交互作用項
babies.result = glm(bwt2 ~ gestation + parity + age + height + weight + smoke, data = babies, family = binomial(link = logit) )
# 有交互作用項
# babies.result = glm(bwt2 ~ gestation + parity + age + height + weight + smoke + parity*smoke , data = babies, family = binomial(link = logit) )
# null.babies.result = glm(bwt2 ~ 1, data = babies, family = binomial(link = logit) )
# anova(null.babies.result, babies.result, test = "LRT")
# anova(null.babies.result, babies.result, test = "Chisq")


summary(babies.result)
print(babies.result)
plot(babies.result)

resid = babies.result$residuals
plot(resid)


library(car)
vif(babies.result, digits = 3)
plot(babies[, 2:8] )
cor(babies[, 2:8] )

#如果要使用 ggplot2 繪製相關係數的熱圖，必須先將資料整理成「變數 1 - 變數 2 - 相關係數」的 data frame，我們可以利用 reshape2套件中的melt函數輕鬆把矩陣格式轉換成上述的 data frame。
library(reshape2)
melt(cor(babies[, 2:8] ) ) 


library(ggplot2)
ggplot(melt(cor(babies[, 2:8] ) ),
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())




exp(babies.result$coef)
confint(babies.result)
exp(confint(babies.result))
# exp^0 = 1 ，只要包含1，就不拒絕H0，所以不顯著

# install.packages("epiDisplay")
library(epiDisplay)
logistic.display(babies.result)

predict.prob = predict(babies.result, newdata = babies, type = "response")



# install.packages("InformationValue")
library(InformationValue)

# Calculate cut-off probability minimizing the miscalssification error
opt.cutoff = optimalCutoff(babies$bwt2, predict.prob) 

# Calculate Important Assessment Measure
confusionMatrix(babies$bwt2, predict.prob, threshold = opt.cutoff)

# 誤差
misClassError(babies$bwt2, predict.prob, threshold = opt.cutoff)

# 精確度
precision(babies$bwt2, predict.prob, threshold = opt.cutoff)

# 敏感度
sensitivity(babies$bwt2, predict.prob, threshold = opt.cutoff)

# 明確度
specificity(babies$bwt2, predict.prob, threshold = opt.cutoff)



# install.packages("plotROC")
library(plotROC)
predict.table = data.frame(true_label = babies$bwt2, predict_prob = predict.prob)

# Plot ROC curve and calculate AUC
basic.plot <- ggplot(predict.table, aes(d = true_label, m = predict.prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
basic.plot + style_roc() +
  annotate("text", x = .75, y = .25, size = 5,
           label = paste("AUC =", round(calc_auc(basic.plot)$AUC, 3)))
