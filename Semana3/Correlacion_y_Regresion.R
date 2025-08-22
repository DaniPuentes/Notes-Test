# Libraries

library(tidyverse)


# Pearson correlation -----------------------------------------------------

## Create normal data

data = tibble(
  x = rnorm(500, mean = 175, sd = 10),
  y = rnorm(500, mean = 70, sd = 10)
)

## Plot density

plot(density(data$x))
plot(density(data$y))

## Normality test

shapiro.test(data$x)
shapiro.test(data$y)

## Covariance calculation

data = data %>% 
  mutate(x_diff = x - mean(x), y_diff = y - mean(y))

(covariance = (sum(data$x_diff * data$y_diff)) / (nrow(data) - 1))

## Pearson's correlation (r)

desv_x = sqrt((sum(data$x_diff^2)) / (nrow(data) - 1))

desv_y = sqrt((sum(data$y_diff^2)) / (nrow(data) - 1))

(correlation = covariance / (desv_x * desv_y))

cor.test(data$x, data$y, method = "pearson")


# Spearman correlation ----------------------------------------------------

## Data

data(penguins)

penguins = na.omit(penguins)

data_spearman = penguins %>% 
  select(bill_len, body_mass)

## Plot density

plot(density(data_spearman$bill_len))
plot(density(data_spearman$body_mass))

## Normality test

shapiro.test(data_spearman$bill_len)
shapiro.test(data_spearman$body_mass)

## Ranks differences

data_spearman = data_spearman %>% 
  mutate(d = rank(bill_len) - rank(body_mass), d2 = d^2)

## Spearman's correlation

(rho = 1 - ((6 * sum(data_spearman$d2)) / 
             (nrow(data_spearman) * (nrow(data_spearman)^2 - 1))))

cor.test(data_spearman$bill_len, data_spearman$body_mass,
         method = "spearman")


# Linear regression -------------------------------------------------------

model = lm(bill_len ~ body_mass, data = penguins)

summary(model)

### Formula: bill_len = 27.15 + 0.004 * body_mass ###

r2 = summary(model)$r.squared

ggplot(data = penguins, aes(x = body_mass, y = bill_len)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm",
              color = "red", linetype = "dashed", se = TRUE) +
  annotate("text", x = 6000, y = 40,
           label = bquote(R^2 == .(round(r2, 3)))) +
  labs(x = "Body mass (g)", y = "Bill length (mm)") +
  theme_bw()


# Logistic regression -----------------------------------------------------

data(mtcars) # am - 0 = automatic, 1 = manual

model = glm(am ~ wt, data = mtcars, family = binomial(link = "logit"))

summary(model)

exp(model$coefficients) # Coeff b1 < 1, restar a 1 

1 - exp(model$coefficients)[2]

### Se reduce en un 98% los odds de ser manual por cada aumento de peso ###  

## Plot

ggplot(data = mtcars, aes(x = wt, y = am))+
  geom_point(aes(color = factor(am)), size = 2, alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE, color = "grey50")+
  labs(x = "Weight", y = "Automatic (0) - Manual (1)",
       color = "Car type")+
  theme_bw()
