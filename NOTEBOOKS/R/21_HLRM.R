# https://ourcodingclub.github.io/tutorials/mixed-models/

library(tidyverse)  # load the package containing both ggplot2 and dplyr
library(lme4)
library(ggeffects)
library(sjPlot)

githubURL="https://github.com/ourcodingclub/CC-Linear-mixed-models/raw/master/dragons.RData"
download.file(githubURL,"dragons")
load("dragons")
head(dragons)
hist(dragons$testScore)
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) + geom_point() + geom_smooth(method = "lm"))

plot(basic.lm, which = 1)  # plot residuals
plot(basic.lm, which = 2)  # QQ plot
boxplot(testScore ~ mountainRange, data = dragons)
(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))


(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

#We can see the variance for mountainRange = 339.7. Mountain ranges are clearly important: they explain a lot of 
#variation. How do we know that? We can take the variance for the mountainRange and divide it by the total variance:

339.7/(339.7 + 223.8)

#So the differences between mountain ranges explain ~60% of the variance that’s “left over” after the variance
#explained by our fixed effects.

plot(mixed.lmer)

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange/site), data = dragons)  
summary(mixed.lmer2)
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")))

#Random slopes
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 
summary(mixed.ranslope)
### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")))

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal())

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))

# show summary
summary(mixed.ranslope)

full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange/sample),data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange/sample),data = dragons, REML = FALSE)
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 
