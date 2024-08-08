library(plm)
library(gt)

f1 <- as.formula(paste("df$`II.3: Centrally Sponsored Schemes` ~", paste0("df$`",names(df)[c(5,8)],"`",collapse = "+")))
f2 <- as.formula(paste("df$`II.2: Central Plan Schemes` ~", paste0("df$`",names(df)[c(5,8)],"`",collapse = "+")))
f3 <- as.formula(paste("df$`I.6: WMA from RBI` ~", paste0("df$`",names(df)[c(5,8)],"`",collapse = "+")))
f4 <- as.formula(paste("df$`II.D.5.c.1: GST compensation` ~", paste0("df$`",names(df)[c(5,8)],"`",collapse = "+")))

column_names <- c("Centrally Sponsored Schemes(Rs. Crore)", "Central Plan Schemes(Rs. Crore)", "WMA from RBI(Rs. Crore)", "GST compensation(Rs. Crore)")
row_names <- c("Same_Gov(Fixed)", "Tax_Revenue(Fixed)", "Same_Gov(Random)", "Tax_Revenue(Random)", "pval_Hausman")
f_array <- c(f1, f2, f3, f4)
Same_Gov_coeffs_random <- c()
Tax_coeffs_random <- c()
Same_Gov_coeffs_fixed <- c()
Tax_coeffs_fixed <- c()
model_phtest_pvalue <- c()

for (i in 1:length(f_array)) {
  fixed <- plm(f_array[[i]], data = df, index = c("State_UT", "fiscal_year"), model = "within")
  random <- plm(f_array[[i]], data = df, index = c("State_UT", "fiscal_year"), model = "random")
  coefi <- coef(fixed)[1]
  Same_Gov_coeffs_fixed <- c(Same_Gov_coeffs_fixed, coefi[[1]])
  coefi <- coef(fixed)[2]
  Tax_coeffs_fixed <- c(Tax_coeffs_fixed, coefi[[1]])
  coer <- coef(random)[2]
  Same_Gov_coeffs_random <- c(Same_Gov_coeffs_random, coer[[1]])
  coer <- coef(random)[3]
  Tax_coeffs_random <- c(Tax_coeffs_random, coer[[1]])
  rm(coefi)
  rm(coer)
  print(summary(fixed))
  print(summary(random))
  model_phtest_pvalue <- c(model_phtest_pvalue, phtest(fixed, random)$p.value)
}


table <- matrix(cbind(Same_Gov_coeffs_fixed, Tax_coeffs_fixed,
                      Tax_coeffs_random, Same_Gov_coeffs_random, model_phtest_pvalue),
                nrow = 5, byrow = TRUE)
table1 <- table
colnames(table1) <- column_names
rownames(table1) <- row_names
table1 <- as.table(table1)
table1

table2 <- as.data.frame(table)
colnames(table2) <- column_names
rownames(table2) <- row_names
table2 %>% gt(
  rownames_to_stub = TRUE,
)

rm(f1, f2, f3, f4, table, f_array, row_names, column_names, Same_Gov_coeffs_fixed, Same_Gov_coeffs_random,
   model_phtest_pvalue, Tax_coeffs_fixed, Tax_coeffs_random)

# result and description
