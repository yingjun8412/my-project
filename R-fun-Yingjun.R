1.1
a <- data.frame(
  a1 = c(2，4，6）
  a2 = c(3，5，8）
  a3 = c(4，1，9）
)
头(a)
1.2
pmin(a$a1, a$a2, a$a3)
1.3
pmin_apply <- 函数(...) {
  mat <- do.call(cbind, list(...))
  应用矩阵，1，最小值
}
结果 <- pmin_apply(a$a1, a$a2, a$a3)
cat(
1.4
pmin_for <- 函数(...) {
  vecs <- list(...)
  n <- length(vecs[[1]])
  res <- numeric(n)
  对于 (i 在 1:n) {
    temp <- numeric(length(vecs))
    对于 (j 在 1:长度(vecs)) {
      temp[j] <- vecs[[j]][i]
    }
    res[i] <- min(temp)
  }
  结果
}
1.5
结果 <- 基准::标记(
  + 基础 = pmin(a$a1, a$a2, a$a3),
  +     pmin_apply = pmin_apply(a),
  + pmin_for = pmin_for(a)
打印结果
2.1
b1 <- data.frame(   name = c("文", "颜", "唐", "黄"),   score = c(95, 96, 97, 98) )
  打印(b1)
  b2 <- data.frame(
  name_b2 = c("文", "颜")
  github = c("uuu1016", "yanyutong111")
)
  打印(b2)
2.2
merged_data <- merge(
  x = b1,
  y = b2
  by.x = "name_b1"
  by.y = "name_b2"
  all.y = TRUE
)
打印(合并数据)
2.3
inner_join_result <- b1 %>%
  内连接(b2, 按照 = c("name_b1" = "name_b2"))
打印(内连接结果)
2.4
inner_merge <- `%merge%`(b1, b2, by.x = "name_b1", by.y = "name_b2")
打印(内合并)
