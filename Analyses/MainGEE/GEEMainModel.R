library(glmtoolbox)
df <- read.csv('GEE_data.csv')
#subset of edited
df <- df[df$edited == 1,]
#fit model
fit <- glmgee(
              formula = "correct ~ human + neg:human:israel + israel + logDe", 
              family = binomial(logit), 
              id = subject_num, 
              corstr = "exchangeable",
              data = df
              )
summary (fit)
write.csv(tidy (fit), "results.csv", row.names = FALSE)
#W: DF = 4 
#beta StD -> StD = SE

# split israel into two predictors
fit_intr<- glmgee(
  formula = "correct ~ object:israel + human:israel + human + logDe", 
  family = binomial(logit), 
  id = subject_num, 
  corstr = "exchangeable",
  data = df
)
summary (fit_intr)
write.csv(tidy (fit), "resultsItr.csv", row.names = FALSE)
#W: DF = 5
