
library(ggplot2)

m.data$Same_Gov <- as.factor(m.data$Same_Gov)
# Wage comparison among Gender
ggplot(m.data, aes(x = log(m.data$`II.3: Centrally Sponsored Schemes` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-4, 11)

ggplot(m.data, aes(x = log(m.data$`II.2: Central Plan Schemes` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-4, 9)

ggplot(m.data, aes(x = log(m.data$`I.6: WMA from RBI` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-5, 9)

ggplot(m.data, aes(x = log(m.data$`II.D.5.c.1: GST compensation` + 0.1), color = Same_Gov, fill = Same_Gov)) +
  geom_density(alpha = 0.3) +
  xlim(-5, 9)

m.data$Same_Gov <- as.double(m.data$Same_Gov)
