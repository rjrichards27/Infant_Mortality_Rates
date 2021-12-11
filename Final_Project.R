library("readxl")
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(cobalt)
library(MatchIt)
library(randomForest)

infant <-read_excel('/Users/rachelrichards/Desktop/702/Final_Project_Data.xls')

#infant$mort <- (infant$`Infant_Mortality_Rate (%)` / 100)
#infant$abort <- (infant$`Abortion_Rate(%)` / 100)
infant$pov <- (infant$`Poverty_Rate (%)` / 100)
infant$mort <- infant$`Infant_Mortality_Rate (%)`
infant$abort <- infant$`Abortion_Rate(%)`

infant$bible <- as.factor(infant$Bible_Belt)
infant$state <- as.factor(infant$State)
infant$region <- as.factor(infant$Region)
infant$control <- as.factor(infant$State_Control)

#Centering variables
infant$mort_c <-c(scale(infant$mort, scale=F))
infant$abort_c <-c(scale(infant$abort, scale=F))
infant$pov_c <-c(scale(infant$pov, scale=F))
infant$rpi_c <-c(scale(infant$`Real_Per_Capita_Personal_Income ($)`, scale=F))

#EDA
hist(infant$mort)

boxplot(mort~state, data= infant, main='Infant Mortality vs State')
boxplot(mort~region, data= infant, main='Infant Mortality vs Region')
boxplot(mort~bible, data= infant, main='Infant Mortality vs Bible Belt')
boxplot(mort~control, data= infant, main='Infant Mortality vs State Political Control')

plot(infant$abort_c, infant$mort, main='Infant Mortality vs Abortion Rate')
plot(infant$pov_c, infant$mort, main='Infant Mortality vs Poverty Rate')
plot(infant$rpi_c, infant$mort, main='Infant Mortality vs Personal Income per capita')

#Interactions with Region
ggplot(infant,aes(x= bible, y=mort, fill=bible)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="bet.oz vs inc by med", x = "inc", y = "bwt.oz") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ region,ncol=3)

ggplot(infant,aes(x= control, y=mort, fill=control)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="bet.oz vs inc by med", x = "inc", y = "bwt.oz") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ region,ncol=3)

ggplot(infant,aes(x=pov_c,y=mort, fill=pov_c)) +
  geom_point() + 
  labs(title="Turnout vs Age Groups by County", x = 'Age Groups', y='Turnout')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~region)

ggplot(infant,aes(x=rpi_c,y=mort, fill=rpi_c)) +
  geom_point() + 
  labs(title="Turnout vs Age Groups by County", x = 'Age Groups', y='Turnout')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~region)

ggplot(infant,aes(x=abort_c,y=mort, fill=abort_c)) +
  geom_point() + 
  labs(title="Infant Mortality Rate vs Abortion Rate by REgion", x = 'Abortion Rate', y='Infant Mortality')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~region)

#Interactions with State
ggplot(infant,aes(x= bible, y=mort, fill=bible)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="bet.oz vs inc by med", x = "inc", y = "bwt.oz") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=3)

ggplot(infant,aes(x= control, y=mort, fill=control)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="bet.oz vs inc by med", x = "inc", y = "bwt.oz") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ state,ncol=3)

ggplot(infant,aes(x=pov_c,y=mort, fill=pov_c)) +
  geom_point() + 
  labs(title="Turnout vs Age Groups by County", x = 'Age Groups', y='Turnout')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~state)

ggplot(infant,aes(x=rpi_c,y=mort, fill=rpi_c)) +
  geom_point() + 
  labs(title="Turnout vs Age Groups by County", x = 'Age Groups', y='Turnout')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~state)

ggplot(infant,aes(x=abort_c,y=mort, fill=abort_c)) +
  geom_point() + 
  labs(title="Infant Mortality Rate vs Abortion Rate by Region", x = 'Abortion Rate', y='Infant Mortality')+
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~state)

#Other possible interactions
ggplot(infant,aes(x= control, y=mort, fill=control)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="bet.oz vs inc by med", x = "inc", y = "bwt.oz") +
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ bible,ncol=3)

#Model Selection
null <- lm(mort~abort_c, data=infant)
full <- lm(mort~abort_c + pov_c + rpi_c + bible + control + bible + bible:control, data=infant)
summary(full)

Model_forward <- step(null, scope = formula(full), direction="forward", trace=0)
Model_forward$call

model1 <- lm(formula = mort ~ abort_c + pov_c + bible + control, data = infant)
AIC(model1)

#Linaerity with Residuals
library(ggplot2)
ggplot(infant,aes(x=abort_c, y=model1$residuals))+ geom_point(alpha = 0.7)+geom_hline(yintercept=0,col="red3") + theme_classic() + labs(title ="Residuals vs Age", x="Age", y= "residuals")
ggplot(infant,aes(x=pov_c, y=model1$residuals))+ geom_point(alpha = 0.7)+geom_hline(yintercept=0,col="red3") + theme_classic() + labs(title ="Residuals vs Age", x="Age", y= "residuals")
#Independence and Equal Variance
plot(model1, which=1,col=c("blue4"))
#Normality
plot(model1, which=2,col=c("blue4"))

mort2 <- log(infant$mort)
abort2 <- log(infant$abort_c)
pov2 <- log(infant$pov_c)

model9 <- lm(formula = mort2 ~ abort_c + pov_c + bible + control, data = infant)
plot(model9, which=2,col=c("blue4"))

#Predictions- All are above 0 so we are good
new1inft <- data.frame(abort_c=0.32, pov_c=0.12, bible="1", control="Rep")
pred1 <- predict(model1, new1inft,interval="prediction"); pred1
new2inft <- data.frame(abort_c=0.11, pov_c=0.23, bible="1", control="Dem")
pred2 <- predict(model1, new2inft,interval="prediction"); pred2
new3inft <- data.frame(abort_c=0.04, pov_c=0.02, bible="0", control="Dem")
pred3 <- predict(model1, new3inft,interval="prediction"); pred3

#Hierarchical Modeling
model2 <- lmer(mort ~ abort_c + pov_c + bible + control + (1 | State), data=infant) 
AIC(model2)

model3 <- lmer(mort ~ abort_c + pov_c + bible + control + (1 | Region), data=infant) 
AIC(model3)

model4 <- lmer(mort ~ abort_c + pov_c + bible + control + (1 | Region) + (1 | State), data=infant) 
AIC(model4) 
#Lowest AIC so final model

model5 <- lmer(mort ~ abort_c + pov_c + bible + control + (bible| Region) + (1 | State), data=infant) 
AIC(model5)

model6 <- lmer(mort ~ abort_c + pov_c + bible + control + (1 | Region) + (control| State), data=infant) 
AIC(model6)

summary(model4)

model10 <- lmer(mort2 ~ abort_c + pov_c + bible + control + (1 | Region) + (1 | State), data=infant) 
AIC(model4) 

#Assumptions

library(ggplot2)
ggplot(infant, aes(x = abort_c, y = residuals(model4))) + geom_point(alpha=.07)+
  # geom_hline(yintercept = 0, col='red3')+
  geom_smooth(method="lm",col="red3") +
  theme(text = element_text(size=20)) + labs(title = 'Residuals vs Abortion Rate', x= 'Abortion Rate', y='residuals')

ggplot(infant, aes(x = pov_c, y = residuals(model4))) + geom_point(alpha=.07)+
  # geom_hline(yintercept = 0, col='red3')+
  geom_smooth(method="lm",col="red3") +
  theme(text = element_text(size=20)) + labs(title = 'Residuals vs Poverty Rate', x= 'Poverty Rate', y='residuals')

plot(model4)

#Normality
qqnorm(residuals(model4), cex.main=1.7, cex.lab = 1.5, cex.axis = 1.2);
qqline(residuals(model4))

qqnorm(residuals(model10), cex.main=1.7, cex.lab = 1.5, cex.axis = 1.2);
qqline(residuals(model10))

#VIFs
library(car)
vif(model4)

#Checking for outliers
install.packages("influence.ME")
library(influence.ME)

infl <- influence(model4, obs = TRUE)
cook_val = cooks.distance(infl, sort=TRUE)
cook_val_greatest = tail(cook_val, n =20)
plot( cook_val_greatest, ylim= c(0, 0.05),ylab="Cook’s Distance", main = "Top 20 greatest cook’s distance", cex.main = 1.7, cex.lab = 1.5, cex.axis = 1.2)

#Interpreation Plots
install.packages("ggthemes")
library(ggthemes)

ggplot(data = infant, aes(x = abort_c, y = predict(model4),color=state)) +
  geom_smooth(method = "lm", fullrange = TRUE, size = 0.3) +
  geom_jitter(aes(x = abort_c, y =mort, group = state, color=state),
              alpha = 0.2) +
  labs(x = "Abortion Rate", y = "Infant Mortality Rate") +
  ggtitle("Random Intercept Model") +
  scale_colour_discrete('State') +
  theme_tufte()+
  theme(text = element_text(size=20))

ggplot(data = infant, aes(x = abort_c, y = predict(model4),color=Region)) +
  geom_smooth(method = "lm", fullrange = TRUE, size = 0.3) +
  geom_jitter(aes(x = abort_c, y =mort, group = Region, color=Region),
              alpha = 0.2) +
  labs(x = "Abortion Rate", y = "Infant Mortality Rate") +
  ggtitle("Random Intercept Model") +
  scale_colour_discrete('Region') +
  theme_tufte()+
  theme(text = element_text(size=20))

dotplot(ranef(model4, condVar=TRUE), cex=1.5)$Region
dotplot(ranef(model4, condVar=TRUE), cex=1.5)$State

(ranef(model4))$Region
(ranef(model4))$State

#Confidence Intervals
confint(model4, level = 0.95)