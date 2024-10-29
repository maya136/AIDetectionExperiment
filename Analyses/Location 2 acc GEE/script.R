data <- read.csv("data.csv", header = TRUE)
un_df <- data[ , grepl("Q2", names(data))]
#subset_data[ , grepl("_unedited_", names(subset_data))]

stringtrue = "התמונה אמיתית"
stringfalse1 = "התמונה ערוכה אך אני לא יודע.ת היכן בדיוק"
stringfalse2="התמונה ערוכה ואני יודע.ת היכן בדיוק"
un_df <- as.data.frame(lapply(un_df, function(x) {
  x <- as.character(x)
  x[x == stringtrue] <- ""  
  x[x == stringfalse1 ] <- 1  
  x[x == stringfalse2 ] <- 2 
  return(as.numeric(x))  
}))
df <- data.frame( subject_num = integer(), picture = character(),
                  ans = numeric(),correct = numeric(), stringsAsFactors = FALSE)
subset_data <- un_df
for (i in 1:nrow(subset_data)) {
  for (j in 1:ncol(subset_data)) {
    if (!is.na(subset_data[i, j])) {
      col_name <- colnames(subset_data)[j]
      new_row <- data.frame(
        subject_num = i,
        picture = col_name,
        ans = as.numeric(subset_data[i, j]),
        correct = 1* (grepl("_edited_",col_name) == (as.numeric(subset_data[i, j])>0))
      )
      df <- rbind(df, new_row)
    }
  }
}
write.csv(df, "un_df.csv", row.names = FALSE)

#fit model
fit <- glmgee(
  formula = "correct ~ ans", 
  family = binomial(logit), 
  id = subject_num, 
  corstr = "exchangeable",
  data = df
)
summary (fit)
write.csv(tidy (fit), "results.csv", row.names = FALSE)
#W: DF = 1