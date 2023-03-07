knitr::opts_chunk$set(echo = TRUE)


#install.packages("tidyverse")
#install.packages("broom")
#install.packages("pROC")
#install.packages("dplyr")
#install.packages("forestmangr")
#install.packages("ggplot2")
#install.packages("ggmosaic")
#install.packages("ggpubr")

library(tidyverse)
library(forestmangr)
library(broom)
library(dplyr)
library(pROC)
library(ggplot2)
library(ggmosaic)
library(ggpubr)

ecommerce <- read.csv("ecommerce.csv")
summary(ecommerce)
glimpse(ecommerce)

ecommerce <- ecommerce %>% mutate(conversion = factor(conversion), discount = factor(discount), source = factor(source), country = factor(country))

m1 <- glm(conversion ~ discount, data = ecommerce, family = binomial)

summary(m1)

exp(coef(m1))

exp(confint(m1))

ggplot(data = ecommerce) + 
  geom_mosaic(aes( 
    x = product(discount),  
    fill = conversion), 
    offset = 0.02,  
    divider = ddecker()) + 
  facet_grid(~source,
             scales = "free") + 
  theme_pubr() + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  labs(x = "", y = "") 

m2 <- glm(conversion ~ discount + source, data = ecommerce, family = binomial)
summary(m2)

exp(coef(m2))

m3 <- glm(conversion~source + discount + discount:source, data = ecommerce, family = binomial)
summary(m3)

exp(coef(m3))
exp(confint(m3))

m4 <- glm(conversion ~ discount + source + visit_duration + total_pages_visited + country + discount:source, data = ecommerce, family = binomial)
summary(m4)

exp(coef(m4))
exp(confint(m4))

cor.test(ecommerce$total_pages_visited, ecommerce$visit_duration, method ="pearson")

m5 <- glm(conversion ~ discount + source + total_pages_visited + country + discount:source, data = ecommerce,  family = binomial)
summary(m5)

exp(coef(m5))
exp(confint(m5))

tidy(m5) %>% 
  mutate(exp_beta_llci = exp(confint(m5))[, 1], 
         exp_beta = exp(estimate), 
         exp_beta_ulci = exp(confint(m5))[, 2]) %>% 
  select(term, estimate, exp_beta_llci, exp_beta, exp_beta_ulci) %>% 
  ggplot(aes(x = term, 
             y = exp_beta,
             ymin = exp_beta_llci,
             ymax = exp_beta_ulci)) +
  geom_point(size = 4) + 
  geom_errorbar(width = 0.25) +

  geom_hline(yintercept = 1, linetype = "dashed", 
                    size = 1, color = "dodgerblue") + 
  labs(title = "95% CI: Pass sign up odds by factor",
       x = NULL,
       y = "Likehood by Factor (odds ratio, main effect)") + 
  coord_flip() + 
  theme_pubr()


ecommerce$base_probability <- predict(
  m5,
  ecommerce,
  type = "response"
)

head(ecommerce)

mean(ecommerce$base_probability)

ecommerce$pred_conversion <- 1 * (ecommerce$base_probability >= 0.5)
head(ecommerce)

table(ecommerce$conversion, ecommerce$pred_conversion)


(20190+908)/(20190+450+908+3498)

roccurve <- roc(ecommerce$conversion, 
              ecommerce$base_probability) 


roccurve$auc


ggroc(roccurve, 
      color = "dodgerblue", 
      size = 1,
      legacy.axes = T) + 
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, alpha  =  0.01) + 
  theme_pubr() +
  labs(title = sprintf("ROC Curve (AUC = %.5f)", roccurve$auc), 
       x = "1 - Specificity (or fpr)", 
       y = "Sensitivity (or tpr)")

ecommerce_new <- ecommerce
ecommerce_new$total_pages_visited <- ecommerce_new$total_pages_visited+1
ecommerce_new$new_prob <- predict(m5, ecommerce_new, type = "response")

mean(ecommerce_new$new_prob)
mean(ecommerce_new$base_probability)

(mean(ecommerce_new$new_prob) - mean(ecommerce_new$base_probability))/mean(ecommerce_new$base_probability)
