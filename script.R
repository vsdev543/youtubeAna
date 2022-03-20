yt<-readRDS("data/yt.Rds")

library(dplyr)
library(lubridate)

yt<-as_tibble(yt)

yt$trending_date<-ydm(yt$trending_date)
