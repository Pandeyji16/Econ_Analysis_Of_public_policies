
library(ggplot2)

df$Same_Gov <- as.factor(df$Same_Gov)
# Wage comparison among Gender
ggplot(df, aes(x = log(df$`II.3: Centrally Sponsored Schemes` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-4, 11)

ggplot(df, aes(x = log(df$`II.2: Central Plan Schemes` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-4, 9)

ggplot(df, aes(x = log(df$`I.6: WMA from RBI` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-5, 9)

ggplot(df, aes(x = log(df$`II.D.5.c.1: GST compensation` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-5, 9)

df$Same_Gov <- as.double(df$Same_Gov)
