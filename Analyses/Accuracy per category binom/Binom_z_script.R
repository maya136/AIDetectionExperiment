
z_test_independent_proportions <- function(k1, n1, k2, n2) {
  p1 <- k1 / n1
  p2 <- k2 / n2
  
  # pooled variance
  p_pooled <- (k1 + k2) / (n1 + n2)

  se_diff <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
  
  # Z
  z_stat <- (p1 - p2) / se_diff
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  return(list(z_statistic = z_stat,p_value = p_value))
}

k1 <- 369  #group1
n1 <- 688  #group1
k2 <- 319 # group 2
n2 <- 688  # group 2

result <- z_test_independent_proportions(k1, n1, k2, n2)
print(result)
