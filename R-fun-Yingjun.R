1.1
a <- data.frame(
  a1 = c(2, 4, 6),
  a2 = c(3, 5, 8),
  a3 = c(4, 1, 9)
)
head(a)
1.2
pmin(a$a1, a$a2, a$a3)
1.3
pmin_apply <- function(...) {
  mat <- do.call(cbind, list(...))
  apply(mat, 1, min)
}
result <- pmin_apply(a$a1, a$a2, a$a3)
cat("pmin_apply的结果：", result, "\n") 
1.4
pmin_for <- function(...) {
  vecs <- list(...)
  n <- length(vecs[[1]])
  res <- numeric(n)
  for (i in 1:n) {
    temp <- numeric(length(vecs))
    for (j in 1:length(vecs)) {
      temp[j] <- vecs[[j]][i]
    }
    res[i] <- min(temp)
  }
  res
}
1.5
result <- bench::mark(
  +     base = pmin(a$a1, a$a2, a$a3),
  +     pmin_apply = pmin_apply(a),
  +     pmin_for = pmin_for(a) )
print(result)
2.1
b1 <- data.frame(   name = c("文", "颜", "唐", "黄"),   score = c(95, 96, 97, 98) )
  print(b1)
  b2 <- data.frame(   name_b2 = c("文", "颜"),   github = c("uuu1016","yanyutong111") )
  print(b2)
2.2
merged_data <- merge(
  x = b1,
  y = b2,
  by.x = "name_b1",
  by.y = "name_b2",
  all.y = TRUE
)
print(merged_data)
2.3
inner_join_result <- b1 %>%
  inner_join(b2, by = c("name_b1" = "name_b2"))
print(inner_join_result)
2.4
inner_merge <- `%merge%`(b1, b2, by.x = "name_b1", by.y = "name_b2")
print(inner_merge)
