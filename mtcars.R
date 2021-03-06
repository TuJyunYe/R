# 202108-12  雲創培訓班  資料科學應用  迴歸分析作業

library(tidyverse) # clean data
# library(knitr) 寫成 Rmarkdown 用
# library(reshape2) #堆疊資料畫 heatmap 用
library(GGally) # corr
library(car) #殘差變異數具有均齊性 & 逐步回歸
library(moments) #畫y & ln y 的圖形
library(plotly) # 視覺化圖形

# Data Set 介紹 =======
RDocumentation：https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
[, 1] mpg ：Miles/(US) gallon 英哩/每加侖
[, 2] cyl ：Number of cylinders 汽缸數  #categorical variable（ordirnal scale）
[, 3] disp：Displacement (cu.in., cubic inch) 排氣量
[, 4] hp  ：Gross horsepower 馬力
[, 5] drat：Rear axle ratio 後輪軸減速比
[, 6] wt  ：Weight (1000 lbs) 車體重量
[, 7] qsec：1/4 mile time 1/4英里加速秒數
[, 8] vs  ：V/S，0代表V型引擎，1代表直立式引擎  #categorical variable（nominal scale）
[, 9] am  ：Transmission (0 = automatic自排, 1 = manual手排)   #categorical variable（nominal scale）
[,10] gear：Number of forward gears 變數箱前進檔 #categorical variable（ordirnal scale）
[,11] carb：Number of carburetors 化油器 #categorical variable（ordirnal scale）
# Data Set 介紹 =======

# Analysis Objectives：
### 1.探討影響 每加侖可行駛的里程數 的重要影響因子與關係
### 2.嘗試以 Linear Regression 建立複迴歸預測模型

# Data: R 內建 dataset - mtcars

#### 主要的資料處理與分析架構會包括以下步驟：
## 1.資料載入與檢視
MtcarsData <- mtcars
attach(MtcarsData) #直接把 data 裡面的 variable 做讀取的動作

# 查看全部資料
kable(MtcarsData, row.names = F, caption = '原始資料')


# 查看資料結構:資料變數、型態
glimpse(MtcarsData)
MtcarsData$cyl <- as.factor(MtcarsData$cyl)
MtcarsData$vs <- as.factor(MtcarsData$vs)
MtcarsData$am <- as.factor(MtcarsData$am)
MtcarsData$gear <- as.factor(MtcarsData$gear)
MtcarsData$carb <- as.factor(MtcarsData$carb)
glimpse(MtcarsData)



## 2.資料探索(Exploratory Data Analysis, EDA)：視覺化分析
# 基本敘述統計
# 檢視資料集各變項(欄位)的mean,median,25th and 75th quartiles,min,max, NA數量
summary(MtcarsData)

#視覺化 相關係數
ggcorr(data = MtcarsData, palette = "RdYlGn",
       label = TRUE, label_color = "black")

#針對類別變數畫 boxplot 來看趨勢

#不同汽缸數的每加侖可行駛哩程數之Boxplot
ggplot(data = MtcarsData) + geom_boxplot( aes( x = cyl, y = mpg, colour = cyl)) + 
  labs( x = 'cyl',
        y = 'mpg',
        title = 'Mpg Boxplot by Cyl')
#觀察的結果，就是汽缸數越多，每加侖可行駛哩程數越少（呈現負相關）
# 樣本數只有 32 筆，不會針對離群值去做 NA 值的設定以及填補


#不同引擎的每加侖可行駛哩程數之Boxplot
ggplot(data = MtcarsData) + geom_boxplot( aes( x = vs, y = mpg, colour = vs)) + 
  labs( x = 'vs',
        y = 'mpg',
        title = 'Mpg Boxplot by Vs')
#直立式引擎的每加侖可行駛哩程數比V型引擎還多


#不同排檔的每加侖可行駛哩程數之Boxplot
ggplot(data = MtcarsData) + geom_boxplot( aes( x = am, y = mpg, colour = am)) + 
  labs( x = 'am',
        y = 'mpg',
        title = 'Mpg Boxplot by Am')
#手排每加侖可行駛哩程數比自排還多



#不同變數箱前進檔的每加侖可行駛哩程數之Boxplot
ggplot(data = MtcarsData) + geom_boxplot( aes( x = gear, y = mpg, colour = gear)) + 
  labs( x = 'gear',
        y = 'mpg',
        title = 'Mpg Boxplot by Gear')
# 5 個前進檔的車一定是手排
# 3 個 或 4 個前進檔的車，可能是手排，也可能是自排
# 不同變數箱前進檔的每加侖可行駛哩程數沒有差太多（相對均勻）


# carb 有些選項的 observation 只有 1 筆，畫不出 boxplot


#連續變數的 boxplot
boxplot(MtcarsData[, c(1, 3:7)])
# 有離群值但沒有要做刪除級填補的動作（樣本數太少）

# 3. 資料標準化(Scaling/Normalization, 又稱正規化)處理
# 目的：可使後續進行變數間相關性衡量時，其分析結果不會因各變項的單位尺規大小不一致而影響

plot(density(mpg, na.rm = TRUE), main = "Density Plot: mpg", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(mpg, na.rm = TRUE), 2)))

# 透過ln轉換，來使 反應變數 mpg 更近似常態分配
# 將所有數值型的解釋變數以 MinMax 標準化方法統一轉換至(0 to 1)的尺規區間
MtcarsData2 <- MtcarsData %>% mutate(
  mpg_ln   = log(mpg),
  disp_nor = (disp - min(disp)) / (max(disp)-min(disp)),
  hp_nor   = (hp - min(hp)) / (max(hp)-min(hp)),
  drat_nor = (drat - min(drat)) / (max(drat)-min(drat)),
  wt_nor   = (wt - min(wt)) / (max(wt)-min(wt)),
  qsec_nor = (qsec - min(qsec)) / (max(qsec)-min(qsec))
  )

attach(MtcarsData2)

plot(density(mpg_ln, na.rm = TRUE), main = "Density Plot: mpg_ln", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(mpg_ln, na.rm = TRUE), 2)))

#將類別變數轉換成虛擬變數（dummy variable）
DummyTable <- model.matrix( ~ cyl + gear + carb, data = MtcarsData2)
kable(DummyTable[1:10, ])

# 最後的資料
MtcarsData3 <- cbind(
  MtcarsData2[, 12:17], 
  MtcarsData2[, c(8, 9)],
  DummyTable[, -1]
)
kable(MtcarsData3[1:10, ])

## 4. 建模 (Modeling) - 以Linear Regression 線性迴歸建立每加侖可行駛哩程數預測模型
set.seed(42)
model1 <- lm(mpg ~ disp + hp + drat + wt + qsec
             + vs + am + cyl + gear + carb, data = MtcarsData)
summary(model1)
vif(model1)


model2 <- lm(mpg_ln ~ disp_nor + hp_nor + drat_nor + wt_nor + qsec_nor + vs + am
             + cyl6 + cyl8 + gear4 + gear5 + carb2 + carb3 + carb4 + carb6 + carb8, data = MtcarsData3)
summary(model2)
vif(model2)

#使用逐步回歸法來降低共線性問題（此 packege 的逐步回歸法建立在 找到最低的 AIC 就停止）
lm.step <- step(model2)
summary(lm.step)


## 5. 線性迴歸模型適合度診斷

# H0：殘差服從常態分配
resid = lm.step$residuals
shapiro.test(resid)


# H0：殘差變異數具有均齊性（均一性）
ncvTest(lm.step)


# H0：殘差之間互相獨立
durbinWatsonTest(lm.step)


# RSE除了顯示在sumary(modelFit2)的底端，亦可透過以下sigma()函數取得
sigma(lm.step)


## 探討模型的準確度及誤差
# 誤差：以 RMSE 衡量(Root-Mean-Square Error)，越低越好
RMSE <- function( predict, actual){
  result <- sqrt(mean((predict - actual) ^ 2))
  return(result)
}

cat('model1模型的RMSE：\n',RMSE(model1$fitted.values, MtcarsData$mpg),'\n',sep = '')
cat('lm.step模型的RMSE：\n',RMSE(lm.step$fitted.values, MtcarsData3$mpg_ln),'\n',sep = '')


# 誤差：以MAPE衡量(Mean Absolute Percentage Error)，越低越好
MAPE <- function( predict, actual){
  result <- mean(abs((predict - actual)/actual)) %>% round(3) * 100
  return(result)
}
cat('model1模型的MAPE：\n',MAPE(model1$fitted.values, MtcarsData$mpg), '%', '\n',sep = '')
cat('lm.step模型的MAPE：\n',MAPE(lm.step$fitted.values, MtcarsData3$mpg_ln), '%' ,'\n',sep = '')


## 針對模型部分做個小結 ===============================================================
# 1. 因樣本數（32）少的關係，因此針對離群值不做 NA 值設定及填補
# 2. 反應變數 y （mpg）做 ln 變數變換
# 3. 針對 5 個 categorical variables 轉換成 dummy variables （虛擬變數）
# 4. 模型適合度檢定皆有達到（殘差為常態分配、殘差分配變異數皆相同、殘差彼此之間互為獨立）
# 5. 透過資料前處理以及特徵工程，模型解釋力（adj R square）上升 10 %

# Model、MSE (越低越好)、adj R square(越高越好)、RMSE(越低越好) 、MAPE(越低越好)
# mode1   、 2.833 、     0.779    、 1.940 、 8%
# lm.step 、 0.106 、     0.874    、 0.097 、 2.7%

# 6. 透過逐步回歸法所選出的變數有：hp_nor、drat_nor、wt_nor 以及 carb4
## 針對模型部分做個小結 ===============================================================


## Insight
# 重修建一個 tibble，把原本被轉為虛擬變數的 carb ，再轉回原本的變數
# carb 6 以及 carb 8 只有 1 筆 observation，畫不出 Boxplot
MtcarsData4 <- MtcarsData %>%
  filter(
    carb == 1 | carb == 2 | carb == 3 | carb == 4
  ) %>%
  select(
    carb
  )

MtcarsData5 <- MtcarsData3 %>%
  filter(
    carb == 1 | carb == 2 | carb == 3 | carb == 4
  ) %>% 
  select(
    mpg_ln, hp_nor, drat_nor, wt_nor
    ) %>%
  cbind(
    MtcarsData4
    )


ggplot(MtcarsData5, aes(x = carb, y = mpg_ln)) +
  geom_boxplot() +
  xlab("Carb") + ylab("mpg_ln") +
  theme_bw()


# 想要知道為甚麼 Carb 4 對每加侖可行駛哩程數有解釋能力。
# 透過 兩母體平均數之差的假設檢定，去檢定不同的 carb，其每加侖平均可行駛里程數是否有顯著的差異
MeanCarb1 <- MtcarsData5 %>% filter(
  carb == 1) %>%
  select(
    mpg_ln
    )

MeanCarb2 <- MtcarsData5 %>% filter(
  carb == 2
  ) %>% 
  select(
    mpg_ln
    )

MeanCarb3 <- MtcarsData5 %>% filter(
  carb == 3
) %>% 
  select(
    mpg_ln
  )

MeanCarb4 <- MtcarsData5 %>% filter(
  carb == 4
) %>% 
  select(
    mpg_ln
  )

#雙尾檢定，母體平均數假設值定為 0
# H0：mu1 - mu2 = 0

# carb 1 跟 carb 4 有顯著差異
t.test(MeanCarb1, MeanCarb4, alternative = "two.sided",
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)  

# carb 2 跟 carb 4 有顯著差異
t.test(MeanCarb2, MeanCarb4, alternative = "two.sided",
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)  

# carb 3 跟 carb 4 有顯著差異
t.test(MeanCarb3, MeanCarb4, alternative = "two.sided",
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)  


# hp  ：Gross horsepower 馬力
# 化油器越多，馬力越好
ggplot(MtcarsData5, aes(x = carb, y = hp_nor)) +
  geom_boxplot() +
  xlab("Carb") + ylab("hp_nor") +
  theme_bw()

# drat：Rear axle ratio 後輪軸減速比
# 化油器跟減速比沒什麼差異
ggplot(MtcarsData5, aes(x = carb, y = drat_nor)) +
  geom_boxplot() +
  xlab("Carb") + ylab("drat_nor") +
  theme_bw()

# wt  ：Weight (1000 lbs) 車體重量
# 化油器越多，車體重量越重
ggplot(MtcarsData5, aes(x = carb, y = wt_nor)) +
  geom_boxplot() +
  xlab("Carb") + ylab("wt_nor") +
  theme_bw()


#視覺化 相關係數
ggcorr(data = MtcarsData5, palette = "RdYlGn",
       label = TRUE, label_color = "black")


# 散佈圖 馬力 vs 每加侖可行使哩程數 group by carb
# 馬力 與 每加侖可行使哩程數 呈現負相關
ggplot(MtcarsData5, aes(x = hp_nor, y = mpg_ln)) + geom_point(aes(shape = carb))

# 散佈圖 後輪軸減速比 vs 每加侖可行使哩程數 group by carb
# 後輪軸減速比 與 每加侖可行使哩程數 呈現正相關
ggplot(MtcarsData5, aes(x = drat_nor, y = mpg_ln)) + geom_point(aes(shape = carb))

# 散佈圖 車體重量 vs 每加侖可行使哩程數 group by carb
# 車體重量 與 每加侖可行使哩程數 呈現負相關
ggplot(MtcarsData5, aes(x = wt_nor, y = mpg_ln)) + geom_point(aes(shape = carb))


#散佈圖 車體重量 vs 馬力 group by carb
ggplot(MtcarsData5, aes(x = wt_nor, y = hp_nor)) + geom_point(aes(shape = carb))



#針對元數資料做整理
#沒有 carb6 & carb8
MtcarsData6 <- MtcarsData %>%
  filter(
    carb == 1 | carb == 2 | carb == 3 | carb == 4
  )

#新增一變數，當作標籤用
MtcarsData6["CarLabel"] = c(1:30)


MeanTable <- MtcarsData6 %>%
  group_by(
    carb
    ) %>%
  summarise(
    mpg_mean = mean(mpg),
    hp_mean = mean(hp),
    drat_mean = mean(drat),
    wt_mean = mean(wt)
    )

print(MeanTable)


MtcarsDataPlot <- ggplot( data = MtcarsData6,
                      aes( x = hp,
                           y = wt,
                           colour = carb) ) + 
  geom_point(alpha = 0.9) +
  geom_point(aes(size = mpg)) + 
  geom_text( aes( label = CarLabel), vjust = -3, size = 2, colour = 'black') +
  geom_vline(aes( xintercept = mean(hp))) + 
  geom_hline(aes( yintercept = mean(wt))) + 
  
  labs( title = 'hp, wt and carb') + 
  
  theme_bw()


ggplotly(MtcarsDataPlot)


## 針對 Insight 做個小結 ===============================================================
# 1. carb 4 跟 carb 1 & carb2 其 平均每加侖可行駛哩程數具有顯著的差異
# 2. 當化油器越多，馬力會越好；但當化油器越多，其車重也隨之越重
# 3. 因此在化油器為 4 個的車子當中，其馬力並不能因車重隨之增加而有突破性發展，
#    以至於在化油器為 4 個的車子當中，
#    每加侖可行駛哩程數顯著地低於化油器為 1 個及 2 個的車子，
#    而被逐步回歸法選進為重要變數之一。
# 4. 可建議的 6 種車型（or 3 種車型）
## 針對 Insight 做個小結 ===============================================================


## 總結 ==========================================================
# 1. 小樣本的推論不一定是準的，可以當作一個參考的方向
# （32 筆資料，carb = 4 的樣本數就佔了 10 筆，也就是 3成12 數量）
# 2. 建議增加樣本數
# 3. 亦可新增其他重要的變數，如車齡、車價格等
## 總結 ==========================================================



