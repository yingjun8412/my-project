setwd("D:/360极速浏览器X下载/肥胖与RPL/")

# 读取数据
Obese_SA <- read.csv("Obese_SA.csv")

# 数据预处理
## 创建新变量：肥胖 (BMI ≥ 30) 和 RPL (SA ≥ 3)
Obese_SA$obese <- ifelse(Obese_SA$participant.p21001_i0 >= 30, 1, 0)
Obese_SA$rpl <- ifelse(Obese_SA$participant.p3839_i0 >= 3, 1, 0)
## 排除缺失值
Obese_SA <- na.omit(Obese_SA[, c("obese", "rpl")])

# 检查变量分布
cat("肥胖状态分布:\n")
print(table(Obese_SA$obese))

cat("\nRPL状态分布:\n")
print(table(Obese_SA$rpl))

cat("\n肥胖与RPL交叉分布:\n")
print(table(Obese_SA$obese, Obese_SA$rpl))

# Logistic回归分析
model <- glm(rpl ~ obese, 
             data = Obese_SA, 
             family = binomial(link = "logit"))

# 提取回归结果
results <- summary(model)
coefficients <- results$coefficients

# 计算OR值和95%置信区间
or <- exp(coefficients["obese", "Estimate"])
ci <- exp(confint(model)["obese", ])

# 创建结果表格
result_table <- data.frame(
  Variable = "肥胖",
  Beta = coefficients["obese", "Estimate"],
  SE = coefficients["obese", "Std. Error"],
  OR = or,
  CI_lower = ci[1],
  CI_upper = ci[2],
  p_value = coefficients["obese", "Pr(>|z|)"]
)

# 格式化结果
result_table$Beta <- round(result_table$Beta, 3)
result_table$SE <- round(result_table$SE, 3)
result_table$OR <- round(result_table$OR, 3)
result_table$CI_lower <- round(result_table$CI_lower, 3)
result_table$CI_upper <- round(result_table$CI_upper, 3)
result_table$p_value <- ifelse(result_table$p_value < 0.001, "<0.001", 
                               format(round(result_table$p_value, 4), nsmall = 4))

# 显示完整结果
cat("\nLogistic回归分析结果:\n")
print(result_table)

# 将95%置信区间格式化为字符串
result_table$CI <- paste0(result_table$CI_lower, "-", result_table$CI_upper)

# 创建简洁结果表格用于报告
report_table <- result_table[, c("Variable", "OR", "CI", "p_value")]
colnames(report_table) <- c("变量", "OR值", "95%CI", "p值")

# 显示报告表格
cat("\n肥胖与RPL关系分析结果:\n")
print(report_table)

# 保存结果到CSV文件
write.csv(report_table, "肥胖与RPL_logistic分析结果.csv", row.names = FALSE)
library(ggplot2)
library(showtext)
font_add(family = "MicrosoftYaHei", 
         regular = "msyh.ttc",  # 常规字体
         bold = "msyhbd.ttc")   # 粗体字体
showtext_auto()
ggplot(result_table, aes(x = Variable, y = OR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "", y = "OR值", 
       title = "肥胖与RPL关系的OR值及95%置信区间",
       caption = "红线表示无效应(OR=1)") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(family = "MicrosoftYaHei", size = 12)) 
