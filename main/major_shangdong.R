library(openxlsx)
library(data.table)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glue)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
source('/Users/sousekilyu/Documents/R/GaoKao/分专业位次/major.R')

#################
##### 2021&2020 ######
dt2021 <- read.xlsx('/Users/sousekilyu/Documents/R/GaoKao/data/2021年山东省普通一批投档线.xlsx')
colnames(dt2021) <- c('专业', '院校', '计划数', '位次')
dt2021 %<>%
  dplyr::filter(!grepl('定向|中外合作|预科', 专业))
dt2021$院校 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2021$专业 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2021$位次 %<>% 
  str_replace_all('前50名', '50') %>%
  as.numeric()
dt2021_school <-  dt2021 %>%
  dplyr::filter(!is.infinite(位次),
                !is.na(位次)) %>%
  dplyr::group_by(`院校`) %>%
  dplyr::summarise(rank2021_all = max(`位次`, na.rm = TRUE)) %>%
  ungroup()

dt2020 <- read.xlsx('/Users/sousekilyu/Documents/R/GaoKao/data/2020年山东省普通一批投档线.xlsx')
colnames(dt2020) <- c('专业', '院校', '计划数', '位次')
dt2020 %<>%
  dplyr::filter(!grepl('定向|中外合作|预科', 专业))
dt2020$院校 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2020$专业 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2020$位次 %<>% 
  str_replace_all('前50名', '50') %>%
  as.numeric()
dt2020_school <-  dt2020 %>%
  dplyr::filter(!is.infinite(位次),
                !is.na(位次)) %>%
  dplyr::group_by(`院校`) %>%
  dplyr::summarise(rank2020_all = max(`位次`, na.rm = TRUE)) %>%
  ungroup()
#################
##### 2022 ######
dt2022 <- read.xlsx('/Users/sousekilyu/Documents/R/GaoKao/data/2022年山东省普通一批投档线.xlsx')
colnames(dt2022) <- c('专业', '院校', '计划数', '位次')
dt2022 %<>%
  dplyr::filter(!grepl('定向|中外合作|预科', 专业))
dt2022$院校 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2022$专业 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2022_school <-  dt2022 %>%
  dplyr::filter(!is.infinite(位次),
                !is.na(位次)) %>%
  dplyr::group_by(`院校`) %>%
  dplyr::summarise(rank2022_all = max(`位次`, na.rm = TRUE)) %>%
  ungroup()
#################
##### 2023 ######
dt2023 <- read.xlsx('/Users/sousekilyu/Documents/R/GaoKao/data/2023年山东省普通一批投档线.xlsx')
colnames(dt2023) <- c('专业', '院校', '计划数', '位次')
dt2023 %<>%
  dplyr::filter(!grepl('定向|中外合作|预科', 专业))
dt2023$院校 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2023$专业 %<>% 
  str_replace_all('\\（.*?\\）', '') %>%
  str_replace_all('\\(.*?\\)', '')
dt2023_school <-  dt2023 %>%
  dplyr::filter(!is.infinite(位次),
                !is.na(位次)) %>%
  dplyr::group_by(`院校`) %>%
  dplyr::summarise(rank2023_all = max(`位次`, na.rm = TRUE)) %>%
  ungroup()


dt_delta <- data.frame()
for (i in 1:length(.noun)) {
  .noun_tmp <- .noun[i]
  .major_tmp <- .major[i]
  
  .dt2023_s <-  dt2023 %>%
    getLineByMajor(.noun_tmp, 2023) %>%
    left_join(dt2023_school)
  .dt2022_s <-  dt2022 %>%
    getLineByMajor(.noun_tmp, 2022) %>%
    left_join(dt2022_school)
  .dt2021_s <-  dt2021 %>%
    getLineByMajor(.noun_tmp, 2021) %>%
    left_join(dt2021_school)
  .dt2020_s <-  dt2020 %>%
    getLineByMajor(.noun_tmp, 2020) %>%
    left_join(dt2020_school)
  
  .dt_cp <- left_join(.dt2023_s, .dt2022_s) %>%
    left_join(.dt2021_s) %>%
    left_join(.dt2020_s) %>%
    mutate(delta_all = rank2022_all - rank2023_all,
           delta = rank2022 - rank2023,
           major = .major_tmp)
  dt_delta <- rbind(dt_delta, .dt_cp)
  
  #print(.major_tmp)
}

