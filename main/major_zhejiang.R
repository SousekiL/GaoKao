library(openxlsx)
library(data.table)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glue)
source('/Users/sousekilyu/Documents/文稿 - Souseki的MacBook Pro (2)/R/GaoKao/分专业位次/major.R')


#################
##### 2022 ######
dt2022 <- read.xlsx('/Users/sousekilyu/Documents/文稿 - Souseki的MacBook Pro (2)/R/GaoKao/data/2022年浙江省普通一批投档线.xlsx')
dt2022 %<>% select(专业名称, 学校名称, 计划数, 位次)
colnames(dt2022) <- c('专业', '院校', '计划数', '位次')
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
dt2023 <- read.xlsx('/Users/sousekilyu/Documents/文稿 - Souseki的MacBook Pro (2)/R/GaoKao/data/2023年浙江省普通一批投档线.xlsx')
dt2023 %<>% select(专业名称, 学校名称, 计划数, 位次)
colnames(dt2023) <- c('专业', '院校', '计划数', '位次')
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
  
  .dt_cp <- left_join(.dt2023_s, .dt2022_s) %>%
    filter(!is.na(rank2022), !is.na(rank2023)) %>%
    mutate(delta_all = rank2022_all - rank2023_all,
           delta = rank2022 - rank2023,
           major = .major_tmp)
  dt_delta <- rbind(dt_delta, .dt_cp)
  
  #print(.major_tmp)
}


###########
## 分析 ##
#### 各专业对比 ####
ggplot(dt_delta) + 
  geom_density(aes(x=delta, after_stat(density), 
                   group = major, fill = major), 
               alpha=.2) +
  xlim(-10000, 10000) +
  geom_vline(aes(xintercept=0),
             color="black", linetype="dashed", size=1)+
  #facet_grid(major ~ .) +
  facet_wrap(major ~ .) +
  theme(text = element_text(family='Kai'))

# 排序
.dt_delta_range <- dt_delta %>%
  dplyr::filter(!is.infinite(delta),
                !is.na(delta)) %>%
  group_by(major) %>%
  dplyr::filter(between(delta, mean(delta)-3*sd(delta), mean(delta)+3*sd(delta))) %>%
  dplyr::summarise(sum = sum(delta, na.rm = TRUE)) %>%
  arrange(desc(sum)) %>%
  ungroup()

neworder <- .dt_delta_range$major
dt_delta2 <- dt_delta %>%
  left_join(.dt_delta_range) %>%
  mutate(updown = if_else(sum >= 0, '上升', '下降')) %>%
  transform(major=factor(major,levels=neworder)) %>%
  arrange(major) %>%
  filter(between(delta, -20000, 20000))

ggplot(dt_delta2) + 
  geom_density(aes(x=delta, after_stat(density), 
                   group = major, fill = factor(updown)), 
               alpha=.2) +
  geom_vline(aes(xintercept=0),
             color="black", linetype="dashed", size=1)+
  scale_fill_manual(name = '' , 
                    values = c("上升" = "#05403F", "下降" = "#8C1F28")) +
  #facet_grid(major ~ .) +
  facet_wrap(major ~ ., ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 20),
        legend.position = "none")
ggsave('/Users/sousekilyu/Documents/文稿 - Souseki的MacBook Pro (2)/R/GaoKao/plot/浙江省专业位次变化.png',
       dpi = 300,
       width = 10,
       height = 35,
       limitsize = FALSE)
