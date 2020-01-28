####################################################
# Employment
####################################################
#Last updated: 01/27/2020
#Created by: Jubing Ge
#Data source: ACS 2010-2017
#
####################################################
# 1. Load libraries,identify variables
# 2. Define variables, merge datasets 
####################################################

# 1. Load libraries


rm(list=ls())
#
#library
library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot)

# 2. Define variables, merge data sets

# Table codes of interest -
# https://censusreporter.org/topics/table-codes/
# S2301 Employment Status

# Variable of interest -
##  - Population 16 years and over-- HC01_EST_VC01
##  - Labor Force Participation Rate 16 years and over-- HC02_EST_VC01
##  - Employment/Population Ratio 16 years and over-- HC03_EST_VC01
##  - Unemployment Population 16 years and over--HC04_EST_VC01

rm(list=ls())
# Set the working directory
setwd("~/Desktop/Class/2020W/Policy Lab/Data/S2301_2010_2017")

# Read data
# initialize lists 
olist <- list(NULL)
nfile = 8
length(olist)<-nfile

# start to read data
for (i in 1:nfile)
{
  olist[[i]]<- fread(paste("ACS_", as.character(i+9), "_5YR_S2301_with_ann.csv", sep=""))
}

# Read Geo ID data
Geocode <- read.csv("Geocode.csv")

censustractselect <- function(df) {
  df<-subset(df, GEO.id2 %in% Geocode$censustract)
  df<-subset(df, select = c("GEO.id2", "HC01_EST_VC01",
                            "HC02_EST_VC01","HC03_EST_VC01",
                            "HC04_EST_VC01")) 	
  # convert all numerical data to numeric data type
  df[, 'HC01_EST_VC01':= lapply(df[, 'HC01_EST_VC01'], as.numeric)] 
  df[, 'HC02_EST_VC01':= lapply(df[, 'HC02_EST_VC01'], as.numeric)]
  df[, 'HC03_EST_VC01':= lapply(df[, 'HC03_EST_VC01'], as.numeric)]
  df[, 'HC04_EST_VC01':= lapply(df[, 'HC04_EST_VC01'], as.numeric)]
  df[ , Total_laborforcerate := unlist(df[, 'HC01_EST_VC01'])*unlist(df[, 'HC02_EST_VC01'])/100 ]
  df[ , Total_employmentrate := unlist(df[, 'HC01_EST_VC01'])*unlist(df[, 'HC03_EST_VC01'])/100 ]
  df[ , Total_unemploymentrate := unlist(df[, 'HC01_EST_VC01'])*unlist(df[, 'HC04_EST_VC01'])/100 ]
}

for (i in 1:nfile) 
{
  olist[[i]]<-censustractselect(olist[[i]])
  olist[[i]]$year<-2009+i
}

all <- olist[[1]]
for (i in 2:nfile)
{
  all <- rbind(all, olist[[i]])
}

setnames(all, "HC01_EST_VC01", "Population")
setnames(all, "HC02_EST_VC01", "Labor_Force_Participation_Rate")
setnames(all, "HC03_EST_VC01", "Employment/Population_Ratio")
setnames(all, "HC04_EST_VC01", "Unemployment_Population")

ppl_o16 <- list(data.table())
for (i in 1:nfile)
{
  b = olist[[i]]
  ppl_o16[, c(as.character(i+2009)) :=  b[, 1]]
}

Employment<-all
rm(list=c("all","b","Geocode","olist","ppl_o16"))
rm(i,nfile)
