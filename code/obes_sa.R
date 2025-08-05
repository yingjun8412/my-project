# 加载必要的包
library(tidyverse)
library(survival)
library(survminer)  # 用于ggsurvplot
library(ggpubr)

# 步骤1: 加载数据
data_wide <- read.csv("D:/360极速浏览器X下载/肥胖与RPL/Obese_SA.csv", stringsAsFactors = FALSE)

# 步骤2: 直接构建Cox模型所需的数据结构
cox_data <- data.frame(
  id = integer(),
  start = numeric(),
  stop = numeric(),
  event = integer(),
  obese = factor(),
  bmi = numeric(),
  sa = numeric(),
  visit = integer(),
  stringsAsFactors = FALSE
)

# 步骤3: 循环处理每个参与者
for (i in 1:nrow(data_wide)) {
  eid <- data_wide$participant.eid[i]
  
  # 提取所有随访点的日期、BMI和SA值
  dates <- as.Date(c(data_wide$participant.p53_i0[i],
                     data_wide$participant.p53_i1[i],
                     data_wide$participant.p53_i2[i],
                     data_wide$participant.p53_i3[i]), 
                   format = "%Y/%m/%d")
  
  bmis <- c(data_wide$participant.p21001_i0[i],
            data_wide$participant.p21001_i1[i],
            data_wide$participant.p21001_i2[i],
            data_wide$participant.p21001_i3[i])
  
  sas <- c(data_wide$participant.p3839_i0[i],
           data_wide$participant.p3839_i1[i],
           data_wide$participant.p3839_i2[i],
           data_wide$participant.p3839_i3[i])
  
  # 过滤有效随访点
  valid_visits <- which(!is.na(dates) & !is.na(bmis) & !is.na(sas))
  
  if (length(valid_visits) < 2) next  # 至少需要2个有效随访点
  
  # 按时间排序随访点
  visit_order <- order(dates[valid_visits])
  dates <- dates[valid_visits][visit_order]
  bmis <- bmis[valid_visits][visit_order]
  sas <- sas[valid_visits][visit_order]
  
  # 创建时间区间
  for (j in 1:(length(dates) - 1)) {
    start_time <- as.numeric(difftime(dates[j], dates[1], units = "days")) / 365.25
    stop_time <- as.numeric(difftime(dates[j + 1], dates[1], units = "days")) / 365.25
    
    # 计算事件增量
    event_count <- max(0, sas[j + 1] - sas[j], na.rm = TRUE)
    
    # 定义肥胖状态
    obese_status <- ifelse(bmis[j] >= 30, "Obese", "Non-obese")
    
    # 添加到数据框
    cox_data <- rbind(cox_data, data.frame(
      id = eid,
      start = start_time,
      stop = stop_time,
      event = as.integer(event_count > 0),
      obese = factor(obese_status, levels = c("Non-obese", "Obese")),
      bmi = bmis[j],
      sa_start = sas[j],
      sa_end = sas[j + 1],
      visit = j
    ))
  }
}

# 步骤4: 检查数据
cat("总观察区间数:", nrow(cox_data), "\n")
cat("事件分布:\n")
print(table(cox_data$event))
cat("肥胖状态分布:\n")
print(table(cox_data$obese))
cat("时间区间摘要:\n")
print(summary(cox_data$stop - cox_data$start))

# 步骤5: 拟合Cox比例风险模型
if (nrow(cox_data) > 0 && sum(cox_data$event) > 0) {
  model <- coxph(
    Surv(start, stop, event) ~ obese + cluster(id),
    data = cox_data
  )
  
  # 输出结果
  cat("\n===== Cox比例风险模型结果 =====\n")
  print(summary(model))
  
  # 计算风险比和置信区间
  hr <- exp(coef(model))
  ci <- exp(confint(model))
  cat("\n风险比 (Obese vs Non-obese):", round(hr, 3))
  cat("\n95% 置信区间:", round(ci[1], 3), "-", round(ci[2], 3), "\n")
  
  # 使用ggsurvplot绘制累积风险曲线
  fit <- survfit(Surv(start, stop, event) ~ obese, data = cox_data)
  
  # 创建专业累积风险曲线
  cumhaz_plot <- ggsurvplot(
    fit,
    fun = "cumhaz",          # 绘制累积风险
    conf.int = TRUE,          # 显示置信区间
    risk.table = TRUE,        # 添加风险表
    risk.table.height = 0.3,  # 风险表高度
    break.x.by = 5,           # x轴刻度间隔
    legend.labs = c("Non-obese", "Obese"),  # 图例标签
    title = "Cumulative Hazard of Spontaneous Abortion",  # 标题
    xlab = "Years since baseline",  # x轴标签
    ylab = "Cumulative Hazard",     # y轴标签
    palette = c("#1F77B4", "#FF7F0E"),  # 自定义颜色
    ggtheme = theme_minimal(),      # 使用简洁主题
    font.main = 14,                 # 标题字体大小
    font.x = 12,                    # x轴字体大小
    font.y = 12,                    # y轴字体大小
    font.tickslab = 10,             # 刻度标签字体大小
    tables.theme = theme_cleantable()  # 风险表主题
  )
  
  # 打印图形
  print(cumhaz_plot)
  
  # 保存图形（可选）
  # ggsave("cumulative_hazard_plot.png", plot = cumhaz_plot$plot, width = 10, height = 8, dpi = 300)
  
} else {
  cat("\n无法拟合Cox模型：无有效数据或事件\n")
  
  # 尝试Poisson回归作为替代方案
  if (nrow(cox_data) > 0) {
    cat("\n尝试Poisson回归...\n")
    model_poisson <- glm(event ~ obese, 
                         family = poisson(link = "log"),
                         data = cox_data)
    
    # 输出结果
    cat("\n===== Poisson回归结果 =====\n")
    print(summary(model_poisson))
    
    # 计算风险比
    cat("\n风险比 (Obese vs Non-obese):", exp(coef(model_poisson)["obeseObese"]))
  }
}

