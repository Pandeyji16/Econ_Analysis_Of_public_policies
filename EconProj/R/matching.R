library(MatchIt)

fmatch <- as.formula(paste("df$Same_Gov ~", paste0("df$`",names(df)[c(6:9,11:12,31)],"`",collapse = "+")))

glm_model <- glm(fmatch, data = df, family = "binomial")

prop_score = predict(glm_model, newdata = df, type = "response")
df2 <- cbind(df, prop_score)

m_out <- matchit(Same_Gov ~ prop_score, data = df2, method = "nearest", caliper = 0.05)
m.data <- match.data(m_out)

