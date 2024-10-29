library(glmtoolbox)
df <- read.csv('GEE_data.csv')
#fit model
fit <- glmgee(
              formula = "correct ~ conf", 
              family = binomial(logit), 
              id = subject_num, 
              corstr = "exchangeable",
              data = df
              )
summary (fit)
#df 1
