library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)


data_mobil123<-as.data.frame(read_excel("file.xlsx"))
data_mobil123<-data_mobil123[-1]

data_mobil123<-na.omit(data_mobil123)

summary(data_mobil123)

ggplot(data=data_mobil123,aes(x=harga))+geom_histogram(binwidth=20)
