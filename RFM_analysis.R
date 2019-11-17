library(data.table)
library(dplyr)
library(ggplot2)
library(prettyR)
library(kableExtra)
library(pander)
library(scales)
library(lubridate)
library(skimr)
library(extrafont)
loadfonts()
library(ggpubr)
library(knitr)
library(DT)

# File load

df_trans <- fread("Retail_Data_Transactions.csv", stringsAsFactors = FALSE)
df_respo <- fread("Retail_Data_Response.csv", stringsAsFactors = FALSE)

str(df_trans)
str(df_respo)

df_trans %>% arrange(customer_id) %>% head(20) %>% View()
df_respo %>% arrange(customer_id) %>% head() %>% View()

summary(df_trans)
summary(df_respo)

# Data Type
df_trans$trans_date <- dmy(df_trans$trans_date)

# EDA

ggplot(data = df_trans, aes(x = tran_amount, y = ..density..)) +
  geom_histogram(fill = "cornsilk", color = "grey60", size = 0.2) +
  geom_density() +
  xlim(0, 105)

ggplot(data = df_trans, aes(x = tran_amount)) +
  geom_density(fill = "skyblue", size = 0.8, alpha = 0.6) +
  ggtitle("Density plot of Transaction data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif"))
ggsave("densityPlot.jpg", dpi = 300)
  # theme(plot.title = element_text(family = "serif", face = "bold"))

# RFM 분석을 위한 Customer_ID 기준 테이블 

cust_df <- df_trans %>%
  group_by(customer_id) %>%
  summarise(Recency = max(trans_date),
            Freq = n(),
            Money = sum(tran_amount))

# 구매기간 경과 컬럼 추가
analy_day <- max(cust_df$Recency)+1

cust_df <- cust_df %>%
  mutate(Recency_days = analy_day-Recency)

View(head(cust_df))

# Quantile

result <- c()
j <- 0
for(i in 1:5){
  j = j + i
  result[i] = 1 -(j/(1+2+3+4+5))
  print(result)
}
result

R_level <- quantile(cust_df$Recency_days, probs = result)
F_level <- quantile(cust_df$Freq, probs = result)
M_level <- quantile(cust_df$Money, probs = result)

cust_df <- cust_df %>%
  mutate(R_score = case_when(.$Recency_days >= R_level[1] ~ 1,
                             .$Recency_days >= R_level[2] ~ 2,
                             .$Recency_days >= R_level[3] ~ 3,
                             .$Recency_days >= R_level[4] ~ 4,
                             TRUE ~ 5),
         F_score = case_when(.$Freq >= F_level[1] ~ 5,
                             .$Freq >= F_level[2] ~ 4,
                             .$Freq >= F_level[3] ~ 3,
                             .$Freq >= F_level[4] ~ 2,
                             TRUE ~ 1),
         M_score = case_when(.$Money >= M_level[1] ~ 5,
                             .$Money >= M_level[2] ~ 4,
                             .$Money >= M_level[3] ~ 3,
                             .$Money >= M_level[4] ~ 2,
                             TRUE ~ 1))

cust_df %>% head()

str(cust_df)
str(df_respo)         

length(unique(cust_df$customer_id))
length(unique(df_respo$customer_id))

# Campaign Response Colounm

cust_df2 <- left_join(cust_df, df_respo, by="customer_id")

# NA 0으로 처리 
summary(cust_df2)
table(cust_df2$response)
cust_df2$response[is.na(cust_df2$response)] <- 0
summary(cust_df2)
table(cust_df2$response)

# 캠페인 반응여부 컬럼 Y/N 추가

cust_df2$Campaign <- ifelse(cust_df2$response == 1, "Y", "N")

str(cust_df2)

# 가중치

R_table <- cust_df2 %>%
  group_by(R_score) %>%
  summarise(Cust = n_distinct(customer_id),
            tot_Money = sum(Money))
R_table <- R_table %>%
  mutate(Cust_prop = Cust/sum(Cust),
         Money_prop = tot_Money/sum(tot_Money),
         Effect = Money_prop/Cust_prop) %>%
  arrange(desc(R_score))
R_effect <- sum(R_table$Effect)

F_table <- cust_df2 %>%
  group_by(F_score) %>%
  summarise(Cust = n_distinct(customer_id),
            tot_Money = sum(Money))
F_table <- F_table %>%
  mutate(Cust_prop = Cust/sum(Cust),
         Money_prop = tot_Money/sum(tot_Money),
         Effect = Money_prop/Cust_prop) %>%
  arrange(desc(F_score))
F_effect <- sum(F_table$Effect)


M_table <- cust_df2 %>%
  group_by(M_score) %>%
  summarise(Cust = n_distinct(customer_id),
            tot_Money = sum(Money))
M_table <- M_table %>%
  mutate(Cust_prop = Cust/sum(Cust),
         Money_prop = tot_Money/sum(tot_Money),
         Effect = Money_prop/Cust_prop) %>%
  arrange(desc(M_score))
M_effect <- sum(M_table$Effect)

# 가중치
sum_effect <- sum(R_effect,F_effect,M_effect)
Weight <- c(R_effect/sum_effect, F_effect/sum_effect, M_effect/sum_effect)

# RFM Scoring Function

RFM_function <- function(x, y, z, w){
  RFM_Score  <- x*w[1]+y*w[2]+z*w[3]
  return(RFM_Score)
}

# RFM Scoring

cust_df3 <- cust_df2 %>%
  mutate(RFM_Score = RFM_function(cust_df2$R_score, cust_df2$F_score, cust_df2$M_score, w = Weight))
cust_df3 %>% arrange(desc(RFM_Score), desc(Money))

# Find Customer

## Retention

risk_cust <- cust_df3 %>%
  filter(RFM_Score >= 3 & R_score <= 2)
risk_cust

## Up-Sell

upsell_cust <- cust_df3 %>%
  filter(F_score >= 4 & M_score <= 2)
upsell_cust

## Cross-Sell

crosssell_cust <- cust_df3 %>%
  filter(M_score >= 3 & F_score <= 2)
crosssell_cust


# 시각화
a_plot <- ggplot(data = cust_df3, aes(x = RFM_Score, y = Money, color = Campaign)) +
  geom_point(position = "jitter") +
  theme_bw()
a_plot

b_plot <- ggplot(data = cust_df3, aes(x = Recency_days, y = Money, color = Campaign)) +
  geom_point(position = "jitter") +
  theme_bw()
b_plot

c_plot <- ggplot(data = cust_df3, aes(x = Freq, y = Money, color = Campaign)) +
  geom_point(position = "jitter") +
  theme_bw()
c_plot

d_plot <- ggplot(data = cust_df3, aes(x = Recency_days, y = Freq, color = Campaign)) +
  geom_point(position = "jitter") +
  theme_bw()
d_plot

