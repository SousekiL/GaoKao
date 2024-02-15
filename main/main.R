
###########
## 分析 ##
###########
setwd('/Users/sousekilyu/Documents/R/GaoKao')
source('分专业位次/major_shangdong.R')
#### 捡漏王 ####
# 新传、外语、法学、计算机、土木工程
# 不分和5万档

dt2022_by <-  dt2022 %>%
  mutate(school = str_replace_all(.$院校, "[A-Z]*[0-9]*", ""),
         major = str_replace_all(.$专业, "[A-Z]*[0-9]*", "")) %>%
  mutate(major = str_replace_all(.$major, "[0-9]*[A-Z]*", "")) %>%
  dplyr::group_by(`school`, `major`) %>%
  dplyr::summarise(rank2022_by = max(`位次`, na.rm = TRUE)) %>%
  ungroup()
dt2023_by <-  dt2023 %>%
  mutate(school = str_replace_all(.$院校, "[A-Z]*[0-9]*", ""),
         major = str_replace_all(.$专业, "[A-Z]*[0-9]*", "")) %>%
  mutate(major = str_replace_all(.$major, "[0-9]*[A-Z]*", "")) %>%
  dplyr::group_by(`school`, `major`) %>%
  dplyr::summarise(rank2023_by = max(`位次`, na.rm = TRUE)) %>%
  ungroup()

dt_by <-  dt2023_by %>%
  left_join(dt2022_by) %>%
  filter(!is.na(rank2023_by), !is.na(rank2022_by)) %>%
  mutate(rank_delta = rank2022_by - rank2023_by)

# 分数上涨最快和下降最快的专业
top500_up <- dt_by %>%
  arrange(desc(rank_delta)) %>%
  `[`(1:500,) 
top500_up[1:20,]
top500_up_major <- setorder(data.table(table(top500_up$major)), col = -'N')
top500_up_major
write.xlsx(top500_up_major, 'data/top500_up_major.xlsx')


top500_down <- dt_by %>%
  arrange(rank_delta) %>%
  `[`(1:500,) 
top500_down[1:20,]
top500_down_major <- setorder(data.table(table(top500_down$major)), col = -'N')
top500_down_major
write.xlsx(top500_down_major, 'data/top500_down_major.xlsx')


# 分数上涨最快和下降最快的专业 top学校
top200_up_1w <- dt_by %>%
  filter(rank2022_by <= 10000) %>%
  arrange(desc(rank_delta)) %>%
  `[`(1:200,) 
top200_up_1w[1:20,]
top200_up_major <- setorder(data.table(table(top200_up_1w$major)), col = -'N')
top200_up_major
write.xlsx(top200_up_major, 'data/top200_up_major.xlsx')

top200_down_1w <- dt_by %>%
  filter(rank2022_by <= 10000) %>%
  arrange(rank_delta) %>%
  `[`(1:200,) 
top200_down_1w[1:20,]
top200_down_major <- setorder(data.table(table(top200_down_1w$major)), col = -'N')
top200_down_major
write.xlsx(top200_down_major, 'data/top200_down_major.xlsx')


#### 分年份直方图 ####
# 外国语院校
fr_school <- c('北京外国语大学', '上海外国语大学', '广东外语外贸大学', '北京第二外国语学院', 
               '西安外国语大学', '四川外国语大学', '大连外国语大学', '天津外国语大学', 
               '北京语言大学', '黑龙江大学')
dt_delta_fr <- dt_delta %>%
  mutate(school = str_replace_all(dt_delta$院校, "[A-Z]*[0-9]*", "")) %>%
  dplyr::filter(school %in% fr_school,
                major == '外国语言文学')

.dt_fr_all <- data.frame()
for (i in 1:length(fr_school)) {
  .dt_delta_by  <- filter(dt_delta_fr, school == fr_school[i])
  .dt_fr <- data.frame(
    school = fr_school[i],
    rbind(
      data.frame(
        year = 2020,
        type = 'school',
        rank = .dt_delta_by$rank2020_all
      ),
      data.frame(
        year = 2020,
        type = 'major',
        rank = .dt_delta_by$rank2020
      ),
      data.frame(
        year = 2021,
        type = 'school',
        rank = .dt_delta_by$rank2021_all
      ),
      data.frame(
        year = 2021,
        type = 'major',
        rank = .dt_delta_by$rank2021
      ),
      data.frame(
        year = 2022,
        type = 'school',
        rank = .dt_delta_by$rank2022_all
      ),
      data.frame(
        year = 2022,
        type = 'major',
        rank = .dt_delta_by$rank2022
      ),
      data.frame(
        year = 2023,
        type = 'school',
        rank = .dt_delta_by$rank2023_all
      ),
      data.frame(
        year = 2023,
        type = 'major',
        rank = .dt_delta_by$rank2023
      )
    )
  )
  .dt_fr_all <- rbind(.dt_fr_all, .dt_fr)
}

ggplot(.dt_fr_all) + 
  geom_line(aes(x=year, y=rank, 
                color = type), 
            linewidth = 1, alpha = .8) +
  scale_color_manual(name = '学校/专业最低位次' , 
                     values = c("major" = "#F16E67", "school" = "#6187B6"),
                     labels = c('专业(外国语言文学)', '学校')) +
  #facet_grid(major ~ .) +
  facet_wrap(school ~ ., scales = "free", ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = 'top'
        #legend.position = "none"
  )
ggsave('plot/山东省外国语言文学.png',
       dpi = 300,
       width = 10,
       height = 13,
       limitsize = FALSE)


# 计算机
fr_school <- c('浙江大学', '国防科技大学', '北京航空航天大学', '北京邮电大学', 
               '哈尔滨工业大学',  '南京大学', '华中科技大学', 
               '电子科技大学', '北京交通大学', '北京理工大学', '东北大学', '吉林大学',
               '同济大学', '中国科学技术大学', '武汉大学', '西北工业大学', '中南大学', '西安电子科技大学')
dt_delta_fr <- dt_delta %>%
  mutate(school = str_replace_all(dt_delta$院校, "[A-Z]*[0-9]*", "")) %>%
  dplyr::filter(school %in% fr_school,
                major == '计算机科学与技术')

.dt_fr_all <- data.frame()
for (i in 1:length(fr_school)) {
  .dt_delta_by  <- filter(dt_delta_fr, school == fr_school[i])
  if(dim(.dt_delta_by)[1] == 0) { next }
  .dt_fr <- data.frame(
    school = fr_school[i],
    rbind(
      data.frame(
        year = 2020,
        type = 'school',
        rank = .dt_delta_by$rank2020_all
      ),
      data.frame(
        year = 2020,
        type = 'major',
        rank = .dt_delta_by$rank2020
      ),
      data.frame(
        year = 2021,
        type = 'school',
        rank = .dt_delta_by$rank2021_all
      ),
      data.frame(
        year = 2021,
        type = 'major',
        rank = .dt_delta_by$rank2021
      ),
      data.frame(
        year = 2022,
        type = 'school',
        rank = .dt_delta_by$rank2022_all
      ),
      data.frame(
        year = 2022,
        type = 'major',
        rank = .dt_delta_by$rank2022
      ),
      data.frame(
        year = 2023,
        type = 'school',
        rank = .dt_delta_by$rank2023_all
      ),
      data.frame(
        year = 2023,
        type = 'major',
        rank = .dt_delta_by$rank2023
      )
    )
  )
  .dt_fr_all <- rbind(.dt_fr_all, .dt_fr)
}

ggplot(.dt_fr_all) + 
  geom_line(aes(x=year, y=rank, 
                color = type), 
            linewidth = 1, alpha = .8) +
  scale_color_manual(name = '学校/专业最低位次' , 
                     values = c("major" = "#F16E67", "school" = "#6187B6"),
                     labels = c('专业(计算机科学与技术)', '学校')) +
  #facet_grid(major ~ .) +
  facet_wrap(school ~ ., scales = "free", ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = 'top'
        #legend.position = "none"
  )
ggsave('plot/山东省计算机科学与技术.png',
       dpi = 300,
       width = 10,
       height = 15,
       limitsize = FALSE)

# 建筑学
fr_school <- c('清华大学', '东南大学', '天津大学', '同济大学', 
               '华南理工大学',  '哈尔滨工业大学', '浙江大学', 
               '重庆大学', '西安建筑科技大学', '北京建筑大学', 
               '南京大学', '武汉大学')
# '大连理工大学', '合肥工业大学', '湖南大学', '沈阳建筑大学'
dt_delta_fr <- dt_delta %>%
  mutate(school = str_replace_all(dt_delta$院校, "[A-Z]*[0-9]*", "")) %>%
  dplyr::filter(school %in% fr_school,
                major == '建筑学')

.dt_fr_all <- data.frame()
for (i in 1:length(fr_school)) {
  .dt_delta_by  <- filter(dt_delta_fr, school == fr_school[i])
  if(dim(.dt_delta_by)[1] == 0) { next }
  .dt_fr <- data.frame(
    school = fr_school[i],
    rbind(
      data.frame(
        year = 2020,
        type = 'school',
        rank = .dt_delta_by$rank2020_all
      ),
      data.frame(
        year = 2020,
        type = 'major',
        rank = .dt_delta_by$rank2020
      ),
      data.frame(
        year = 2021,
        type = 'school',
        rank = .dt_delta_by$rank2021_all
      ),
      data.frame(
        year = 2021,
        type = 'major',
        rank = .dt_delta_by$rank2021
      ),
      data.frame(
        year = 2022,
        type = 'school',
        rank = .dt_delta_by$rank2022_all
      ),
      data.frame(
        year = 2022,
        type = 'major',
        rank = .dt_delta_by$rank2022
      ),
      data.frame(
        year = 2023,
        type = 'school',
        rank = .dt_delta_by$rank2023_all
      ),
      data.frame(
        year = 2023,
        type = 'major',
        rank = .dt_delta_by$rank2023
      )
    )
  )
  .dt_fr_all <- rbind(.dt_fr_all, .dt_fr)
}



ggplot(.dt_fr_all) + 
  geom_line(aes(x=year, y=rank, 
                color = type), 
            linewidth = 1, alpha = .8) +
  scale_color_manual(name = '学校/专业最低位次' , 
                     values = c("major" = "#F16E67", "school" = "#6187B6"),
                     labels = c('专业(建筑学)', '学校')) +
  #facet_grid(major ~ .) +
  facet_wrap(school ~ ., scales = "free", ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = 'top'
        #legend.position = "none"
  )
ggsave('plot/山东省建筑学.png',
       dpi = 300,
       width = 10,
       height = 8,
       limitsize = FALSE)



# 土木工程
fr_school <- c('同济大学', '东南大学', '北京工业大学', '浙江大学', 
               '天津大学',  '哈尔滨工业大学', '大连理工大学', 
               '河海大学', '湖南大学', '中南大学', '西南交通大学')
dt_delta_fr <- dt_delta %>%
  mutate(school = str_replace_all(dt_delta$院校, "[A-Z]*[0-9]*", "")) %>%
  dplyr::filter(school %in% fr_school,
                major == '土木工程')

.dt_fr_all <- data.frame()
for (i in 1:length(fr_school)) {
  .dt_delta_by  <- filter(dt_delta_fr, school == fr_school[i])
  if(dim(.dt_delta_by)[1] == 0) { next }
  .dt_fr <- data.frame(
    school = fr_school[i],
    rbind(
      data.frame(
        year = 2020,
        type = 'school',
        rank = .dt_delta_by$rank2020_all
      ),
      data.frame(
        year = 2020,
        type = 'major',
        rank = .dt_delta_by$rank2020
      ),
      data.frame(
        year = 2021,
        type = 'school',
        rank = .dt_delta_by$rank2021_all
      ),
      data.frame(
        year = 2021,
        type = 'major',
        rank = .dt_delta_by$rank2021
      ),
      data.frame(
        year = 2022,
        type = 'school',
        rank = .dt_delta_by$rank2022_all
      ),
      data.frame(
        year = 2022,
        type = 'major',
        rank = .dt_delta_by$rank2022
      ),
      data.frame(
        year = 2023,
        type = 'school',
        rank = .dt_delta_by$rank2023_all
      ),
      data.frame(
        year = 2023,
        type = 'major',
        rank = .dt_delta_by$rank2023
      )
    )
  )
  .dt_fr_all <- rbind(.dt_fr_all, .dt_fr)
}



ggplot(.dt_fr_all) + 
  geom_line(aes(x=year, y=rank, 
                color = type), 
            linewidth = 1, alpha = .8) +
  scale_color_manual(name = '学校/专业最低位次' , 
                     values = c("major" = "#F16E67", "school" = "#6187B6"),
                     labels = c('专业(土木工程)', '学校')) +
  #facet_grid(major ~ .) +
  facet_wrap(school ~ ., scales = "free", ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = 'top'
        #legend.position = "none"
  )
ggsave('plot/山东省土木工程.png',
       dpi = 300,
       width = 10,
       height = 8,
       limitsize = FALSE)

#### 各专业对比 ####
# 排序
.dt_delta_range <- dt_delta %>%
  # 去除极值
  dplyr::filter(!is.infinite(delta),
                !is.na(delta)) %>%
  group_by(major) %>%
  dplyr::filter(between(delta, mean(delta)-3*sd(delta), mean(delta)+3*sd(delta))) %>%
  dplyr::summarise(sum = sum(delta, na.rm = TRUE)) %>%
  arrange(desc(sum)) %>%
  ungroup()

neworder <- .dt_delta_range$major
dt_delta2 <- dt_delta %>%
  dplyr::filter(!is.infinite(delta),
                !is.na(delta)) %>%
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
ggsave('plot/山东省专业位次变化.png',
       dpi = 300,
       width = 10,
       height = 35,
       limitsize = FALSE)

ggplot(dt_delta2[which(dt_delta2$major == '土木工程'),]) + 
  geom_density(aes(x=delta, after_stat(density), 
                   group = major, fill = factor(updown)), 
               alpha=.2) +
  geom_vline(aes(xintercept=0),
             color="black", linetype="dashed", size=1)+
  scale_fill_manual(name = '' , 
                    values = c("上升" = "#05403F", "下降" = "#8C1F28")) +
  xlim(-20000, 20000) +
  #facet_grid(major ~ .) +
  facet_wrap(major ~ ., ncol = 2) +
  theme(text = element_text(family='Kai'),
        strip.text = element_text(size = 30),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.position = "none")
ggsave('plot/山东省土木工程位次变化.png',
       dpi = 300,
       width = 10,
       height = 5,
       limitsize = FALSE)



