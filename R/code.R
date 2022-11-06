#!/usr/bin/R

## ==============================================================================
## authors         :Clément Bourgoin, Ghislain Vieilledent
## emails          :clement.bourgoin@ec.europa.eu, ghislain.vieilledent@cirad.fr
## web             :https://www.researchgate.net/profile/Clement-Bourgoin
##                  https://ghislainv.github.io
## license         :GPLv3
## ==============================================================================

## Load libraries
library(readr)
library(here)
library(dplyr)
library(ggplot2)

## Load data
df_rh50_SAM <- readr::read_csv(
  here("data", "Fig2DegradationRegRH50.csv")) %>%
  filter(Continent_F == "Americas") %>%
  dplyr::select(years, Ratio1_t1)

## Plot to look at the data
df_rh50_SAM %>%
  ggplot(aes(years, Ratio1_t1)) +
  geom_point()

## Transforming variables
df <- df_rh50_SAM %>%
  mutate(lratio=log(Ratio1_t1), lyears=log(years))
df %>%
  ggplot(aes(lyears, lratio)) +
  geom_point()
## OK, relationship is nearly linear

## Model 1
## You need a power model, that you can fit with a log-log model
## log(y) = a + b * log(x) <=> y = exp(a) * x^b
mod1 <- lm(lratio ~ lyears, data=df)
summary(mod1)
coef(mod1)
## (Intercept)     lyears 
##  3.1481644   0.2849068
a <- as.numeric(coef(mod1)[1])
b <- as.numeric(coef(mod1)[2])
## Here, the power b equals 0.2849068 and is much lower than 1
## so that the ratio will increase slowly with years.

## Model 2 with (log(x))^2
df <- df %>% mutate(lyears2=lyears^2)
mod2 <- lm(lratio ~ lyears + lyears2, data=df)
summary(mod2)
## Here the effect of lyears2 is not significant
## so no need to use a polynomial.

## Predictions
## We need a correction for the intercept when using a log-log model
c <- var(residuals(mod1)) / 2
x_pred <- seq(min(df$years), max(df$years), length.out=100)
y_pred <- exp(c + a + b * log(x_seq))
df_pred <- data.frame(x_pred, y_pred)

## Plot observations vs. model predictions
p <- ggplot(data=df, aes(years, Ratio1_t1)) +
  geom_point() +
  geom_line(data=df_pred, aes(x_pred, y_pred))

## Time to reach 100%
year_ratio100 <- round(exp((log(100) - c - a) / b))
## [1] 166

# End of file