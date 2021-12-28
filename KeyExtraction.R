# 202108-12 雲端培訓班期末報告 關鍵字萃取：詞袋表達 & TF-IDF algorithm

################### 讀入library & 設定工作目錄 ####################
# install.packages("widyr")
# library(text2vec)
# library(tm) # remomve numbers
# library(purrr) # 迴圈用
# library(tuber)
library(tidyverse)
library(tidytext)
library(jiebaR)
library(widyr)
library(RColorBrewer)
library(wordcloud)

# getwd() #查詢目前工作目錄位置
# setwd("D:\R business training") # 設定工作目錄位置

#################### 讀入library & 設定工作目錄 ####################



#################### 透過爬蟲所得的資料 ####################

# library(tuber)
# ID & key 是屬個資，不公開
ClientID <- " "
ClientKey <- " "
yt_oauth(ClientID, ClientKey, token = "")

# 第三屆走鐘獎 最會講獎

#Inokawa Hajime井川一
Hajime <- get_all_channel_video_stats(channel_id = "UCcHVKeT_5Ta-gTa-sgooQxQ")


#Psyman 塞門
Psyman <- get_all_channel_video_stats(channel_id = "UCETKJquzRBMqvRPJru5_SHw")

#	Taiwan Bar
Bar <- get_all_channel_video_stats(channel_id = "UCRNsHFT7BFoAPBcuAa5sgEQ")

# 中指通
Finger <- get_all_channel_video_stats(channel_id = "UCYjB6uufPeHSwuHs8wovLjg")

# 超粒方
Cube <- get_all_channel_video_stats(channel_id = "UC0Q-fBheHysYWz9ObSEzMdA")

#################### 透過爬蟲所得的資料 ####################


#################### 資料輸入與前處理 ####################

video.info <- as.tbl(rbind(Psyman, Bar, Finger, Cube))
# write.csv(video.info, "E:/video.info.csv")


# Remove symbols
my.symbols <- c("《", "》", "【", "】", "｜", "(",")",
                "®", "\n", "？", "@", "#", "?", "！", "!")
video.info$video.description <- gsub(
  paste(my.symbols, collapse = "|"),
  "",
  video.info$description
)

# remove stopwords
my.stop.words <- c(
  "成為這個頻道的會員並獲得獎勵", "小書籤", "Psyman副頻道", "Facebook", "IG", "Mail","開頭", "開場", "遊戲頻道",
  "蘆洲", "本日祭品", "地址", "份量", "口味", "食量小慎入", "上菜速度", "阿軒", "超級",  
  "參考資料與延伸閱讀", "想了解", "多一點點", "訂閱YouTube，新片不漏追", "瞧瞧Facebook，會有YT沒有的東西",
  "追蹤Instagram，限動看個夠", "加入 LINE官方帳號，訊息直接傳給你", "黑啤", "很好買慎入", "來酒吧聊聊吧", 
  "影片授權出處", "特別感謝", "特別感謝", "本集關鍵字", "一起", "客客客棧", "客家人", "酒保", "播放", "貼文", 
  "臺灣吧", "線上賣場", "合作邀約", "歡迎光臨", "訂閱", "專頁", "參考資料", "遠流", "出版", "集資", "成為", "粉絲", "國立", 
  "中指通一下", " Instagram", "工商、包養、合作、仰慕者請洽", "中指通", "平台開張了", "其他也歡迎捕捉野生指通", 
  "大家好", "我是中指通", "感謝", "歡迎使用以下方式當我們爸爸", "中指", "指通", 
  "還是", "謝謝", "爸爸", "這個", "其實",
  "雖然", "這支", "留言",
  "個人", "挑戰市場最低價", "最大", "推出", "所以", "今天", "一樣", "全台", "因為", 
  "的", "你", "我", "他", "影片", "頻道", "訂閱",
  "可以", "什麼", "小心", "大家", "人去", 
  "來看", "支持",
  "歡迎", "這次", "沒有", "希望", "各位", "本集", 
  "還有", "會員", "喜歡", "這裡", "看到", "非常", "來到", "如果", "各種", "必看", "加入", "一個",
  "有各種影視迷因", "加入會員", "如果你剛來", "請看這裡", "經典", "探討", "分享", "大片", "實況", 
  "主要", "探究", "有時", "盡量", "吝嗇", "此", "話", 
  "除了", "哪些", "指教", "改進", "超粒方", "以外", "會", "黃豪瑞", "觀點"
)


video.info$video.description <- gsub(
  paste(my.stop.words, collapse = "|"),
  " ",
  video.info$video.description
)

video.info$video.description <- str_replace_all(video.info$video.description, "[[A-z0-9][:punct:][\\W]]", "")


video.info$video.description <- sapply(video.info$video.description,
                      function(x){
                        return(str_replace_all(x,"台灣","臺灣"))}
)


# Check the result
video.info$video.description[1:2]


#################### 資料輸入與前處理 ####################



#################### 結巴斷詞 ####################

# library(jiebaR)
# Initialize a JiebaR worker
wk <- worker(stop_word = jiebaR::STOPPATH)


# Add customized terms
customized.terms <- c(
  "決戰時裝伸展台", "地獄廚房", "廚神當道", "那些人去哪了", "雙層公寓", "小小廚神", "實境秀", "天橋風雲", 
  "情趣百貨", "黃標", "經營不易", "抖內", "素人", "番號", "無碼", "有碼", "安齋拉拉", "田中寧寧", 
  "年代百大", 
  "大抓周"
)
new_user_word(wk, customized.terms)


# segment terms and separate by blank
video.description <- tibble(
  channel_title = video.info$channel_title, # Youtube channel title
  video_id = video.info$id, # video id
  description = sapply(
    as.character(video.info$video.description), # vector to be segmented
    function(char) segment(char, wk) %>% paste(collapse = " ") # segment function
  )
)

head(video.description, 5)


# library(tidytext)
tok99 <- function(t) str_split(t,"[ ]{1,}")
tidy.description <- video.description %>%
  unnest_tokens(word, description, token = tok99)
tidy.description <- tidy.description[nchar(tidy.description$word) > 1, ]

head(tidy.description, 5)


#################### 結巴斷詞 ####################


#################### 詞頻分析 ####################
# Count words by channels
channel.words <- tidy.description %>%
  group_by(channel_title , word) %>% 
  summarise(word_frequency =  n())

# Visualize
channel.words %>%
  group_by(channel_title) %>%
  top_n(10, word_frequency) %>% 
  arrange(word_frequency) %>%
  ggplot(aes(word, word_frequency, fill = channel_title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~channel_title, ncol = 2, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1))


#################### 詞頻分析 ####################


#################### TF-IDF 演算法 ####################

# derive tf-idf based on channel title 
description.tfidf <- channel.words %>%
  bind_tf_idf(word, channel_title, word_frequency)
head(description.tfidf)

# draw tf-idf bar charts
description.tfidf %>%
  group_by(channel_title) %>% # summarize by channel (= youtuber)
  top_n(10, tf_idf) %>% # select top 10 keywords for each channel
  arrange(desc(tf_idf)) %>% # order by tf-idf
  ggplot(aes(word, tf_idf, fill = channel_title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~channel_title, ncol = 2, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(family="黑體-繁 中黑", size=14),
        axis.text.x = element_text(angle = 60, hjust = 1))


#################### TF-IDF 演算法 ####################


#################### 製作文字雲 ####################

# define a nice color palette
# library(RColorBrewer)
pal <- brewer.pal(8,"Dark2")

# plot the 50 most common words
# library(wordcloud)
description.tfidf %>%
  group_by(channel_title) %>%
  top_n(10, tf_idf) %>%
  with(wordcloud(word, tf_idf, random.order = FALSE, max.words = 50,
                 colors=pal[factor(channel_title)], family="黑體-繁 中黑"))


#################### 製作文字雲 ####################

