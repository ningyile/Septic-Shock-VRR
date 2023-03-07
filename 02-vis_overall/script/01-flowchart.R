rm(list = ls())
options(stringsAsFactors = F)

p_load(tidyverse)

if (T) {
  # 加载数据
  load('../01-mimic4/load/flowchart.Rdata')
  load('../01-mimic4/load/vis_2010.Rdata')
  
  
  # 流程图
  ## 脓毒症标准的患者数量
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    nrow
    
  ## 住ICU时间小于24h的患者
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    filter(., icu_los <= 24) %>% 
    nrow %>% 
    append(.,flowchart)
  
  ## 年龄小于18岁的患者
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    filter(., icu_los > 24) %>% 
    filter(., age < 18) %>% 
    nrow %>% 
    append(.,flowchart)
  
  ## 需要体重参数计算vis评分的患者体重为空
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    filter(., icu_los > 24) %>% 
    filter(., age >= 18) %>% 
    filter(., stay_id %in% wt_null_id$stay_id) %>%
    nrow %>% 
    append(.,flowchart)
  
  ## 入ICU时间、出ICU时间、离院时间为空的患者
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    filter(., icu_los > 24) %>% 
    filter(., age >= 18) %>% 
    filter(., !(stay_id %in% wt_null_id$stay_id)) %>%
    filter(., is.na(intime)|is.na(outtime)|is.na(dischtime)) %>%
    nrow %>% 
    append(.,flowchart)
  
  ## 死亡时间为na，但出ICU和出院时间相同（疑似非医嘱离院？）
  flowchart = population %>% 
    filter(., sepsis3 == 1) %>% 
    filter(., icu_los > 24) %>% 
    filter(., age >= 18) %>% 
    filter(., !(stay_id %in% wt_null_id$stay_id)) %>%
    filter(., !(is.na(intime)|is.na(outtime)|is.na(dischtime))) %>%
    filter(., is.na(deathtime)&outtime==dischtime) %>%
    nrow %>% 
    append(.,flowchart)
  
  ## 入住期间未使用血管活性药物患者
  flowchart = merge_data %>% 
    filter(.,vis_vaso_tag == 'N') %>%
    nrow %>% 
    append(.,flowchart)
  
  ## 初始非零vis评分时间大于入院后24h的患者
  flowchart = merge_data %>% 
    filter(.,vis_vaso_tag == 'Y') %>% # 剔除住ICU期间未使用vaso的患者
    filter(.,first_nonzero_vis_time_tag == 'N') %>% 
    nrow %>% 
    append(.,flowchart)
  
  ## vaso使用不足24h的患者
  flowchart = merge_data %>% 
    filter(.,vis_vaso_tag == 'Y') %>% # 剔除9985个住ICU期间未使用vaso的患者
    filter(.,first_nonzero_vis_time_tag == 'Y') %>% 
    filter(is.na(vis_reduction_rate)) %>%
    nrow %>% 
    append(.,flowchart)
  
  ## 纳入患者总人数
  flowchart = nrow(df) %>% 
    append(.,flowchart)
  
  ## 重命名对象
  df_mimic = df
  
  flowchart_mimic = data.frame(step=(length(flowchart):1),num=flowchart)  %>% 
    arrange(step)

  group_mimic = data.frame(table(df$group)) %>% 
    `colnames<-`(c('group', 'num'))

  ## 保存结果
  save(df_mimic, flowchart_mimic, group_mimic, vis_2010, file = './load/mimic.Rdata')
  write.csv(flowchart_mimic, file = './csv/flowchart.csv')
  }





