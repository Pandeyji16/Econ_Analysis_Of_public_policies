library(dplyr)
library(tidyr)
library(plm)
library(MatchIt)
library(stringr)
selected_columns <- c("State_UT", "fiscal_year", "State_Govt", "Central_Govt", "Same_Gov", "Population",
                     "I: DEVELOPMENTAL EXPENDITURE (A + B)", "I: TAX REVENUE (A+B)",
                     "I.1: Development (a + b)", "I.1.a.2: Medical and Public Health",
                     "I.1.a.6: Urban Development", "I.1.b.2: Rural Development",
                     "I.1.b.10: General Economic Services (i + ii)", "I.1.b.7.i: Roads and Bridges",
                     "I.6: WMA from RBI", "I.A.1: Taxes on Income (i+ii)",
                     "I.A.3.i.a: Central Sales Tax", "I.A.3.i.b: State Sales Tax/VAT",
                     "I.A.3.vii: State Goods and Services Tax", "I.B: Share in Central Taxes (i to ix)",
                     "I.B.i: Central Goods and Services Tax (CGST)", "I.B.iii: Income Tax",
                     "II: NON-DEVELOPMENTAL EXPENDITURE (General Services) (A to F)",
                     "II: NON-TAX REVENUE (C+D)", "II.2: Central Plan Schemes",
                     "II.3: Centrally Sponsored Schemes", "II.D.5.c.1: GST compensation",
                     "II.D.5.c.2: VAT compensation", "II.D.5.c.3: Central sales tax compensation",
                     "Total: TOTAL EXPENDITURE (I+II+III)", "Total: TOTAL REVENUE (I+II)")

df <- Sheet2[, selected_columns]
names(df) <- str_replace_all(names(df), c(" " = "", "." = "", ""))
cleaned_columns <- make.names(gsub("[[:punct:]]", "", tolower(selected_columns)))

# Rename the columns
colnames(df) <- cleaned_columns

fmatch <- as.formula(paste("df$Same_Gov ~", paste0("df$`",names(df)[c(6:8,10:12,16)],"`",collapse = "+")))

glm_model <- glm(fmatch, data = df, family = "binomial")
prop_score = predict(glm_model, newdata = df, type = "response")
df2 <- cbind(df, prop_score)

m_out <- matchit(Same_Gov ~ prop_score, data = df2, method = "nearest", caliper = 0.05)
m.data <- match.data(m_out)

f1 <- as.formula(paste("df$`II.3: Centrally Sponsored Schemes` ~", paste0("df$`",names(df)[c(5,8)],"`",collapse = "+")))
fixed <- plm(f1, data = df, index = c("State_UT", "fiscal_year"), model = "within")
random <- plm(f1, data = df, index = c("State_UT", "fiscal_year"), model = "random")
summary(fixed)
summary(random)
phtest(fixed, random)

