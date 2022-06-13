---
title: "BATs ocean chemistry processing"
author: "LJ Baker"
date: "6/13/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Ocean Chemistry
Ocean-chemistry-data-processing; a continuation of the data processed in doi:10.5194/bg-12-6389-2015 with additional data publically available from the Bermuda Atlantic Time Series <http://bats.bios.edu/bats-data/>.This tutorial was used in the "Data Management and Representation" graduate course in Marine Ecology and Biology Department at the Rosenstiel School of Marine and Atmospheric Science. 


```{r libraries needed}
library(dplyr)
library(ggplot2)
library(reshape2)
```

## Upload and clean the data

Need to turn -999 to NA so as not to confuse the data processing
Also want to subset to work with data just above the mixed layer

```{r}
data = read.csv("~/Dropbox/RSMAS/classes/2022_Spring/Data M&P for TME/Week 2/Bats_data_process.csv")
data[data==-999]=NA
data_sub=subset(data,yyyymmdd>=20040000 & Depth<=25)
```

#Calculate TOP and TON
TOP and TON are not given from the data, so they need to be calculated from the data provided.Will need to  convert because phosphorus is in nmol/kg.

```{r}
data_sub$TOP = (data_sub$TDP-data_sub$SRP)*0.001
data_sub$TON = data_sub$TN - data_sub$NO31
```

#Calculate ratios
Only calculating TOC:TON and POC:PON, but similar data is presented in Figure 2 of the original paper, but this year continues data into present 
```{r}
data_sub$TOC_TON =data_sub$TOC/data_sub$TON
data_sub$POC_PON = data_sub$POC/data_sub$PON
```

#Have R recognize the date format
Else each of the numbers are going to be scaled as if they were true numbers (for example 20161203 instead of December 3, 2016)
```{r}
data_sub$yyyymmdd= as.Date(as.character(data_sub$yyyymmdd), format="%Y%m%d")
```

##Averages and SDs for TOCTON and POCPON
Need to get the averages for these to recreate Figure 2
```{r}
data_sub1= data_sub %>%
  group_by(yyyymmdd) %>%
  summarize(TOCTON_mean=mean(TOCTON, na.rm=TRUE),
            TOCTON_sd=sd(TOCTON, na.rm = TRUE),
            POCPON_mean=mean(POCPON, na.rm=TRUE),
            POCPON_sd=sd(POCPON, na.rm = TRUE))
```

#Plot TOCTON data
Recreating a part of Figure 2a
```{r}
p = ggplot(data_sub1, aes(yyyymmdd))+
  geom_point(aes(y=TOCTON_mean, color="TOC:TON"),shape=1, size=2)+
  geom_errorbar(aes(ymin=TOCTON_mean-TOCTON_sd,
                    ymax=TOCTON_mean+TOCTON_sd, color="TOC:TON"), width=.2)+
  geom_point(aes(y=POCPON_mean,color="POC:PON"), shape=12, size=2)+
  geom_errorbar(aes(ymin=POCPON_mean-POCPON_sd,
                ymax=POCPON_mean+POCPON_sd,color="POC:PON"), width=.2)+
  labs(x = "Year",
       y = "TOC:TON & POC:PON",
       color="Legend")
```

```{r, echo =FALSE}
plot(p)
```

##To create a boxplot similar to the data presented in Figure 4
```{r}
q=ggplot(data_sub, aes(x=month, y=TOC))+
  geom_boxplot()+
  labs(x = "Month",
       y = "TOC (?mol/kg)")
```

```{r, echo=FALSE}
plot(q)


##Replicating Figure 2b
```{r}
data_sub$TOCTOP=data_sub$TOC/data_sub$TOP
data_sub$POCPOP=data_sub$POC/data_sub$POP

data_sub2= data_sub %>%
  group_by(yyyymmdd) %>%
  summarize(TOCTOP_mean=mean(TOCTOP, na.rm=TRUE),
            TOCTOP_sd=sd(TOCTOP, na.rm = TRUE),
            POCPOP_mean=mean(POCPOP, na.rm=TRUE),
            POCPOP_sd=sd(POCPOP, na.rm = TRUE))


ggplot(data_sub2, aes(x=yyyymmdd, y=TOCTOP_mean))+
  geom_point()+
  geom_errorbar(aes(ymin=TOCTOP_mean-TOCTOP_sd,
                    ymax=TOCTOP_mean+TOCTOP_sd), width=.2,
                position = position_dodge(0.05))

ggplot(data_sub2, aes(yyyymmdd))+
  geom_point(aes(y=TOCTOP_mean, color="TOC:TOP"),shape=1, size=2)+
  geom_errorbar(aes(ymin=TOCTOP_mean-TOCTOP_sd,
                    ymax=TOCTOP_mean+TOCTOP_sd, color="TOC:TOP"), width=.2)+
  geom_point(aes(y=POCPOP_mean, color="POC:POP"), shape=12, size=2)+
  geom_errorbar(aes(ymin=POCPOP_mean-POCPOP_sd,
                    ymax=POCPOP_mean+POCPOP_sd, color="POC:POP"), width=.2)+
  labs(x = "Year",
       y = "TOC:TOP & POC:POP",
       color="Legend")
```

##Replicating Figure 4c
```{r}
b=ggplot(data_sub, aes(x=month, y=TON))+
  geom_boxplot()
b

b=ggplot(data_sub, aes(x=month, y=TON))+
  geom_boxplot()+
  labs(x = "Month",
     y = "TON (?mol/kg)")
b
```

