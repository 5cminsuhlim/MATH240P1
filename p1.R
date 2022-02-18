## project 1 scripts ##

## overarching Q:
## How does GDP affect life span globally (2000-2016)?
## 
## More specific Q:
## How does GDP affect life span of women globally (2000-2016)?
## How does GDP affect life span of men globally (2000-2016)?


## packages
library(tidyverse)
library(readxl)
library(maps)
library(scales)


## gdp affect life
life <- read.csv('./data/life.csv')
gdp <- read.csv('./data/GDP.csv', skip = 4)

## clean life
life <- select(life, Location:FactValueNumeric) %>%
        mutate(Country = Location, Life.Span = FactValueNumeric)
