setwd("C:/Users/Administrator/Nutstore/2/data/ukbA")
library(dplyr)

# 读取原始数据表（假设是CSV格式）
Obese_SA <- read.csv("Obese_SA.csv",row.names = 1)

# 步骤1：筛选初始条件并排除NA值
## 保留流产次数>=的样本
filtered_data<- subset(Obese_SA,participant.p3839_i0>=0 & participant.p3839_i1 >=0)   #p3839为流产次数

# 排除关键变量中的NA值
filtered_data <- filtered_data[,c("participant.p21001_i0", 
                                  "participant.p3839_i0",
                                  "participant.p21001_i1",
                                  "participant.p3839_i1"),drop=F]

# 步骤2：计算变化量
# 计算BMI变化量
filtered_data$BMI_change <- filtered_data$participant.p21001_i1 - filtered_data$participant.p21001_i0
# 计算SA变化量
filtered_data$SA_change <- filtered_data$participant.p3839_i1 - filtered_data$participant.p3839_i0

## 去除回答流产次数不符合逻辑的样本
filtered_data <- subset(filtered_data,SA_change>=0)
table(filtered_data$SA_change)

filtered_data$BMI<-ifelse(filtered_data$BMI_change>0,"increase","decrease") # BMI变化大于0即increase
table(filtered_data$BMI, filtered_data$SA_change)

fisher.test(table(filtered_data$BMI, filtered_data$SA_change))

### 以上是i1和i0比较，
### 下次i2和i0比较，i3和i0比较


### 以上比较完，请试着回答BMI变化量是多少 跟流产次数有关。