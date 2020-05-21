#first load the CPI dataset into the R session using file/import dataset/from text(readr)

#loads ggplot2, forcast, and other packages useful for time-series
install.packages("fpp3")
install.packages("urca")
install.packages("forecast") #don't install latest versinon
install.packages("ggpubr")
library("ggplot2")
library("tidyverse")
library("urca")
library("fUnitRoots")
library("ggfortify")
library("forecast")
library("ggpubr")