#################################################
#2018-MAY-19 
  #HKGOV HKDSE 2017 result summary
#################################################

library(tidyverse)
v.url <- "http://www.hkeaa.edu.hk/DocLibrary/HKDSE/Exam_Report/Examination_Statistics/2017_HKDSE_analysis_of_results_of_candidates_table5a_en.csv"
df.DSE <- read.csv(v.url)

#performance distribution across subject
v.cols <- c('Grade5**','Grade5*','Grade5','Grade4','Grade3','Grade2','Grade1','GradeU','Subject')

df.rst1 <- df.DSE %>%
  filter(Gender=='Total',Type=='Percentage') %>%
  mutate(subject = paste0(Subject, Subject_2))%>%
  select(-(1:8))
colnames(df.rst1) <- v.cols
df.rst1.raw <- df.rst1 %>% 
  gather(Grade, Percentage, colnames(df.rst1)[1:8]) %>%
  mutate(Percentage=as.numeric(gsub('%','',Percentage))) %>%
  filter(Grade=='Grade5**') %>%
  arrange(desc(Percentage))

#View(df.rst1)
ggplot(df.rst1.raw, aes(x=reorder(Subject,Percentage), y=Percentage)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Subject") +
  ylab("Percentage obtain 5**") +
  ggtitle("Percentage with grade 5** in 2017 by Subject")


#   #################################################
#     #Package management
#   #################################################
# list.of.packages <- c("lubridate", "ggplot2", "plyr", "dplyr")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages, require, character.only = TRUE)
# 
#   #################################################
#   #Data Import
#   #################################################
# # source table
# v.url <- paste0(rep('http://www.hkeaa.edu.hk/DocLibrary/HKDSE/Exam_Report/Examination_Statistics/',3),
#                 seq(2017,2015,-1),
#                 rep('_HKDSE_analysis_of_results_of_candidates_table5a_en.csv',3))
# v.year <- seq(2017,2015,-1)
# df.source <- cbind.data.frame(url=v.url,year=v.year)
# df.source$url <- as.character(df.source$url)
# 
# #read.csv function
# readHKDSEdata <- function (v.url) {
#   v.year.match <- filter(df.source,url==v.url)$v.year
#   df <- read.csv(url(v.url)) %>%
#           mutate(year=v.year.match)
#   return(df)
# }
# 
# #read table
# 
# df.result <- data.frame()
# for (i in v.url) {
#   df.result <- rbind(df.result,readHKDSEdata(i))
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
