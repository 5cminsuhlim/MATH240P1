## project 1 scripts ##

## overarching Q:
## How does GDP affect life span globally (2014)? (by region)
## 
## More specific Q:
## How does GDP affect life span of women globally (2014)?
## How does GDP affect life span of men globally (2014)?
##
##

## REFERENCES
## 
## stat_poly_eq SOURCE: 
## https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
##
## cuberoot function SOURCE:
## https://stat.ethz.ch/pipermail/r-help/2004-April/050165.html
##
##

## packages
library(tidyverse)
library(readxl)
library(ggpmisc)


##gdp %>%
## select(everything()) %>% 
## summarize_all(funs(sum(is.na(.))))
##
## Country X2000 X2001 X2002 X2003 X2004 X2005 X2006 X2007 X2008 X2009 X2010 X2011 X2012 X2013
## 0       19    18    13    13    13    13    12    12    11    11    10    8     9     8
## X2014 X2015 X2016
## 8     9     10



## clean life -- adult mortality rate
life <- read.csv('./data/life.csv')

life <- select(life, Indicator, ParentLocationCode:FactValueNumeric) %>%
        select(-c(Location.type, Period.type, IsLatestYear:Dim1, 
                  Dim2.type:FactValueNumericPrefix)) %>%
        rename(Country = Location, Mort.Per.1k = FactValueNumeric,
               Country.Code = SpatialDimValueCode, Region = ParentLocation,
               Region.Code = ParentLocationCode, Sex.Code = Dim1ValueCode) %>%
        mutate(Mort.Rate = Mort.Per.1k / 1000) %>%
        select(-Mort.Per.1k) %>%
        filter(Period == 2014)


## clean gdp -- annual gdp growth
#gdp <- read.csv('./data/GDP.csv', skip = 4)
gdp <- read.csv('./data/GDP_Updated.csv', skip = 4)

gdp <- select(gdp, Country.Name, X2000:X2016) %>%
        rename(Country = Country.Name)

gdp2014 <- select(gdp, Country, X2014)



## merge
merged2014 <- merge(life, gdp2014, by = 'Country')


## split based on sex
both <- filter(merged2014, Sex.Code == 'BTSX') %>%
        drop_na()

male <- filter(merged2014, Sex.Code == 'MLE') %>%
        drop_na()

female <- filter(merged2014, Sex.Code == 'FMLE') %>%
        drop_na()


## analysis both
both.lm <- lm(Mort.Rate ~ X2014, data = both)
summary(both.lm)

## linear plot
ggplot(data = both, aes(x = X2014, y = Mort.Rate)) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(both.lm$fitted.values, both.lm$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(both.lm$residuals)
qqnorm(both.lm$residuals)
qqline(both.lm$residuals, col = 'red')

## TRANSFORMATION 
## LOG_10 X

## 
## cuberoot <- function(x)sign(x)*abs(x)^(1/3)

both <- mutate(both, X2014.log10 = log10(X2014)) #cuberoot(X2014))
both.lm.logX <- lm(Mort.Rate ~ X2014.log10, data = both)
summary(both.lm.logX)

## linear plot
ggplot(data = both, aes(x = X2014.log10, y = Mort.Rate)) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(both.lm.logX$fitted.values, both.lm.logX$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(both.lm.logX$residuals)
qqnorm(both.lm.logX$residuals)
qqline(both.lm.logX$residuals, col = 'red')


## TRANSFORMATION 
## CUBEROOT X, SQRT Y
both.lm.cubertX.sqrtY <- lm(sqrt(Mort.Rate) ~ X2014.CubeRoot, data = both)
summary(both.lm.cubertX.sqrtY)

## linear plot
ggplot(data = both, aes(x = X2014.CubeRoot, y = sqrt(Mort.Rate))) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(both.lm.cubertX.sqrtY$fitted.values, both.lm.cubertX.sqrtY$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(both.lm.cubertX.sqrtY$residuals)
qqnorm(both.lm.cubertX.sqrtY$residuals)
qqline(both.lm.cubertX.sqrtY$residuals, col = 'red')



## analysis male
male.lm = lm(Mort.Rate ~ X2014, data = male)
summary(male.lm)

## linear plot
ggplot(data = male, aes(x = X2014, y = Mort.Rate)) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Male Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Male Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(male.lm$fitted.values, male.lm$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(male.lm$residuals)
qqnorm(male.lm$residuals)
qqline(male.lm$residuals, col = 'red')


## TRANSFORMATION 
## CUBEROOT X
male <- mutate(both, X2014.CubeRoot = cuberoot(X2014))
male.lm.cubertX <- lm(Mort.Rate ~ X2014.CubeRoot, data = male)
summary(male.lm.cubertX)

## linear plot
ggplot(data = male, aes(x = X2014.CubeRoot, y = Mort.Rate)) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Male Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Male Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(male.lm.cubertX$fitted.values, male.lm.cubertX$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(male.lm.cubertX$residuals)
qqnorm(male.lm.cubertX$residuals)
qqline(male.lm.cubertX$residuals, col = 'red')


## TRANSFORMATION 
## CUBEROOT X, SQRT Y
male.lm.cubertX.sqrtY <- lm(sqrt(Mort.Rate) ~ X2014.CubeRoot, data = male)
summary(male.lm.cubertX.sqrtY)

## linear plot
ggplot(data = male, aes(x = X2014.CubeRoot, y = sqrt(Mort.Rate))) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Male Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Male Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(male.lm.cubertX.sqrtY$fitted.values, male.lm.cubertX.sqrtY$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(male.lm.cubertX.sqrtY$residuals)
qqnorm(male.lm.cubertX.sqrtY$residuals)
qqline(male.lm.cubertX.sqrtY$residuals, col = 'red')



## analysis female
female.lm = lm(Mort.Rate ~ X2014, data = female)
summary(female.lm)

## linear plot
ggplot(data = female, aes(x = X2014, y = Mort.Rate)) +
        geom_point(color = 'darkgreen', size = 0.7) +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE, level = 0.95,
                    color = 'darkred', fill = 'blue') +
        stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
                     aes(label = paste(..eq.label.., ..rr.label.., 
                                       sep = "*plain(\",\")~~~")),
                     parse = TRUE) +
        labs(title = 'Adult Female Mortality Rate according to Annual GDP Growth (2014)', 
             x = 'Annual GDP Growth',
             y = 'Adult Female Mortality Rate') + 
        theme(axis.title.x = element_text(), 
              axis.title.y.left = element_text(vjust = 3),
              axis.title.y.right = element_text(vjust = 3),
              plot.title = element_text(hjust = 0.5))

## residuals vs fitted values
plot(female.lm$fitted.values, female.lm$residuals)
abline(0,0, col = 'red')

## histogram and qqplot of residuals
hist(female.lm$residuals)
qqnorm(female.lm$residuals)
qqline(female.lm$residuals, col = 'red')
