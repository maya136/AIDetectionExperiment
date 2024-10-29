library(glmtoolbox)
data <- read.csv('rt_data_4gee.csv')
#subset of edited
#fit model
fit <- glmgee(
  formula = "correct ~ rt", 
  family = binomial(logit), 
  id = subject_num, 
  corstr = "exchangeable",
  data = df
)

summary (fit)
write.csv(tidy (fit), "results.csv", row.names = FALSE)
