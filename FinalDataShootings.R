library(readxl)
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(modelr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(purrr)
library(shiny)
library(DT)
library(rsconnect)
library(tidyverse)
library(readr)

rm(list=ls())

setwd("~/Testing")

options(scipen = 999)
column_classes <- c("numeric", "numeric", "numeric", "character", "character")

census <- read.csv('nyc_census_tracts.csv')
censusBlock <- read.csv('census_block_loc.csv', stringsAsFactors = FALSE, colClasses = column_classes)

censusBlock$BlockCode <- as.character(censusBlock$BlockCode)
censusBlock$BlockCode <- substr(censusBlock$BlockCode, 1, nchar(censusBlock$BlockCode) - 4)
names(censusBlock)[names(censusBlock) == "BlockCode"] <- "CensusTract"


censusBlock <- subset(censusBlock, State == "NY")
mergedData <- merge(census, censusBlock, by = "CensusTract", all.x = TRUE)
mergedData <- mergedData[complete.cases(mergedData), ]



