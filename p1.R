## project 1 scripts ##

## overarching Q:
## How does GDP affect life span globally (2014)? (by region)
## 
## More specific Q:
## How does GDP affect life span of women globally (2014)?
## How does GDP affect life span of men globally (2014)?
##
##


## packages
library(tidyverse)
library(readxl)
library(maps)
library(scales)
library(ggpmisc)


##gdp %>%
## select(everything()) %>% 
## summarise_all(funs(sum(is.na(.))))
##
## Country X2000 X2001 X2002 X2003 X2004 X2005 X2006 X2007 X2008 X2009 X2010 X2011 X2012 X2013
## 0    27    22    21    16    16    15    15    14    15    13    13    13    13    13
## X2014 X2015 X2016
## 11    12    12



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
gdp <- read.csv('./data/GDP.csv', skip = 4)

gdp <- select(gdp, Country.Name, X2000:X2016) %>%
        rename(Country = Country.Name)

gdp2014 <- select(gdp, Country, X2014)


## merge
merged2014 <- merge(life, gdp2014, by = 'Country')


## split based on sex
both <- filter(merged2014, Sex.Code == 'BTSX')

male <- filter(merged2014, Sex.Code == 'MLE')

female <- filter(merged2014, Sex.Code == 'FMLE')


## stat_poly_eq SOURCE: 
## https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph

## analysis both
lm.both = lm(Mort.Rate ~ X2014, data = both)
summary(lm.both)

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

## residual plot
ggplot(lm.both, aes(x = .fitted, y = .resid)) + 
        geom_point() +
        geom_hline(yintercept = 0, color = 'red')

## qq plot
ggplot(both, aes(sample = Mort.Rate)) + 
        stat_qq() +
        stat_qq_line(alpha = .7, color = 'red')


## analysis male
lm.male = lm(Mort.Rate ~ X2014, data = male)
summary(lm.male)

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

## residual plot
ggplot(lm.male, aes(x = .fitted, y = .resid)) + 
        geom_point() +
        geom_hline(yintercept = 0, color = 'red')

## qq plot
ggplot(male, aes(sample = Mort.Rate)) + 
        stat_qq() +
        stat_qq_line(alpha = .7, color = 'red')


## analysis female
lm.female = lm(Mort.Rate ~ X2014, data = female)
summary(lm.female)

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

## residual plot
ggplot(lm.female, aes(x = .fitted, y = .resid)) + 
        geom_point() +
        geom_hline(yintercept = 0, color = 'red')

## qq plot
ggplot(female, aes(sample = Mort.Rate)) + 
        stat_qq() +
        stat_qq_line(alpha = .7, color = 'red')


## all together to see trends / differences side-by-side
## linear plot
ggplot(data = merged2014, aes(x = X2014, y = Mort.Rate)) +
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
              plot.title = element_text(hjust = 0.5)) +
        facet_wrap(~Sex.Code)

## qq plot
ggplot(merged2014, aes(sample = Mort.Rate)) + 
        stat_qq() +
        stat_qq_line(alpha = .7, color = 'red') +
        facet_wrap(~Sex.Code)
