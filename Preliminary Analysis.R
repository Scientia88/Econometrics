
library(tidyverse)
library(stargazer)
library(magrittr)
library(moments)
library(gapminder)

# Data sets exploration
View(msleep)
glimpse(msleep)
head(msleep)
class(msleep)
length(msleep)
length(msleep$name)
names(msleep)
unique(msleep$vore)
missing <- !complete.cases(msleep)
msleep[missing,]

#Data sets cleaning
starwars %>% 
  select(name,height,mass)
# select(1:3)
# Changing variable orders
starwars %>% 
  select(name,mass,height,everything()) %>% 
  View()
#Changing variable name
starwars %>% 
  rename('characters'='name') %>% 
  head()
class(starwars$hair_color)
starwars$hair_color <- as.factor(starwars$hair_color)
class(starwars$hair_color)
levels(starwars$hair_color)

starwars %>% 
  mutate(hair_color = as.character(hair_color)) %>% 
  glimpse()
#Changing factor levels
df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)
df <- df %>% 
  mutate(sex = factor(sex,
                      levels=c("male","female","hermaphroditic","none")))
#Filter rows
starwars %>% 
  select(mass,sex) %>% 
  filter(mass < 55&
           sex =='male')
#Re-code data
starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex,'male'='man',
                      'female'='woman'))
#Dealing with missing data
mean(starwars$height,na.rm = TRUE)
starwars %>% 
  drop_na(hair_color)
#Dealing with duplicates
Names <- c('Peter','John','Andrew','Peter')
Age <- c(22,33,44,22)
friends <- data.frame(Names,Age)
friends %>% 
  distinct()



#Data manipulation 
starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name,height,height_m) %>% 
  mutate(tallness = if_else(height_m<1,'short',
                            'tall'))
View(gapminder)
data <- select(gapminder,country,year,lifeExp) 

wide_data <- data %>% 
  pivot_wider(names_from = year,values_from = lifeExp) 
View(wide_data)



long_data <- wide_data %>% 
  pivot_longer(2:13,names_to = 'year',values_to = 'lifeExp')
View(long_data)


#Describing of data 
glimpse(msleep)
#Describe the spread, centrality and variance 
attach(msleep)
min(awake)
max(awake)
range(awake)
IQR(awake)
mean(awake)
median(awake)
var(awake)
detach(msleep)

summary(msleep)
summary(msleep$sleep_total)


# Loading of cross-sectional data
wage <- read.csv('wage1.csv')

#Summerise the data
stargazer(wage,type = 'text')

#Summerise the data for selected variables
wage %>% 
  select(wage,lwage,educ,exper,expersq,tenure,married,female) %>% 
  stargazer(type = 'text')

wagepan <- read.csv('wagepan.csv')
wagepan %<>% select(nr,year,lwage,exper,educ,hours)
str(wagepan)
stargazer(wagepan,type = 'text')
head(wagepan,10)
table(wagepan$year)

#ggplot for plots and graphs. An introduction to data visualization using R programming
View(CO2)
CO2 %>% 
  ggplot(aes(conc,uptake,
             colour = Treatment))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~Type)+
  labs(title = 'Consentration of CO2')+
  theme_bw()

CO2 %>% 
  ggplot(aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(alpha = 0.5,
             aes(size = conc,
                 colour = Plant))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(title = 'Chilled vs Non-chilled')

#Create barchart and histogram

View(msleep)
names(msleep)
msleep %>% 
  drop_na(vore) %>% 
  #ggplot(aes(x = vore))+
  ggplot(aes(fct_infreq(vore)))+
  # Get the colour code from htmlcolorcodes.com
  geom_bar(fill = "#97B3C6")+
  coord_flip()+
  theme_bw()+
  labs(x = "Vor",
       y = NULL,
       title = "Number of observation per order")
ggsave('Barchart.jpg')


msleep %>% 
  ggplot(aes(awake))+
  geom_histogram(binwidth = 2, fill = "blue")+
  theme_bw()+
  labs(x = "Total sleep",
       y = NULL,
       title = "Histogram of total sleep")


# Data loading and visualization (CO2)
CO2data <- co2
glimpse(CO2data)
View(CO2data)
class(CO2data)
plot(co2,main='Atmospheric CO2 Consentration')

# Run the linear regression on Co2
co2.linear.model = lm(co2~time(co2))
abline(co2.linear.model)
(co2.residuals = resid(co2.linear.model))

# Model evaluation
summary(co2.linear.model)


par(mfrow=c(1,3))
hist(co2.linear.model$residuals,main = 'Histogram of Co2 Residuals',xlab = 'Residuals')
qqnorm(co2.linear.model$residuals,main= 'Normal Probability Plot of Residuals')
qqline(co2.linear.model$residuals)
plot(co2.linear.model$residuals~time(co2),main='Residuals on Time',xlab = 'Time',ylab = 'Residuals')

par(mfrow=c(1,1))
plot(co2.linear.model$residuals~time(co2),
     xlim = c(1960,1963),main='Zoom in Residuals on Time',xlab = 'Time',ylab = 'Residuals')

# Data loading and visualization (Sleep)
Sleepdata <- sleep
glimpse(Sleepdata)
View(Sleepdata)
class(Sleepdata)
attach(Sleepdata)
plot(extra~group,main='Extra Sleep in Gossett Data by Group')


# Paired T-test
extra.1 = extra[group==1]
extra.2 = extra[group==2]
t.test(extra.1,extra.2,paired = TRUE,alternative = 'two.sided')
diffs = extra.1 - extra.2
qqnorm(diffs,main = 'Normal Probability Plot')
qqline(diffs)

# Regression analysis
plot(extra.2~extra.1,xlab='Extra sleep with drag 1',ylab = 'Extra sleep with drag 2',
     main='Extra Sleep Drug 2 against Extra Sleep Drug 1')
sleep.linear.model = lm(extra.2~extra.1)
abline(sleep.linear.model)
sumary(sleep.linear.model)
detach(Sleepdata)

# Using pairs command for visual correlation analysis
pairs(trees, pch = 21, bg = c("red"))
