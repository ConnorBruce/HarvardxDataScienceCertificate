install.packages("tidyverse", "dslabs", "NHANES","ggthemes", "ggrepel","gridExtra", "dplyr","RColorBrewer","readrxl")
library(tidyverse)
library(dslabs)
library(NHANES)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(gtools)
library(readxl)

getwd()
newpath = "C:/Users/conno/OneDrive/Documents/Data Science Certificate/HarvardxDataScienceCertificate/Course 6"
setwd(newpath)

path = system.file("extdata", package = "dslabs")
list.files(path)
filename = "murders.csv"
fullpath = file.path(path, filename)
fullpath

file.copy(fullpath, getwd())

file.exists(filename)
