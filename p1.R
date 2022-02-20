## project 1 scripts ##

## overarching Q:
## How does GDP affect life span globally (2000-2016)? (by region)
## 
## More specific Q:
## How does GDP affect life span of women globally (2000-2016)?
## How does GDP affect life span of men globally (2000-2016)?
##
##


## packages
library(tidyverse)
library(readxl)
library(maps)
library(scales)

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
