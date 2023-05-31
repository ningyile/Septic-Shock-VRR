rm(list = ls())
options(stringsAsFactors = F)

p_load(caTools, tidyverse)

# 数据准备
load("./load/mimic.Rdata")

dat = df_mimic %>%
  dplyr::select(stay_id, icu_outcome, hos_outcome) %>%
  mutate(icu_outcome = as.factor(icu_outcome), hos_outcome = as.factor(hos_outcome))

# 设置训练数据和测试数据
set.seed(100)
sample_split <- sample.split(dat$icu_outcome, SplitRatio = 0.8)
train_set_idx <- subset(dat$stay_id, sample_split == TRUE)
test_set_idx <- subset(dat$stay_id, sample_split == FALSE)

# 设置取齐长度
vis_len = c(1:48)

walk(vis_len, function(vis_len){
  
  # min_max标准化自定义函数
  norm_minmax <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  # 提取纳入研究患者的前vis_len小时vis数据
  vis_lstm <- vis_2010 %>%
    select(-c(2, 3, 5)) %>%
    dplyr::filter(stay_id %in% dat$stay_id) %>%
    dplyr::filter(dur_h <= vis_len) %>% 
    # # min_max标准化
    # mutate(across(c(3:9), ~norm_minmax(.)))
    # zscore标准化
    mutate(across(c(3:9), ~scale(.)))
  
  sanity_idx=vis_lstm %>% count(stay_id) %>% dplyr::filter(.,n==vis_len) %>% pull(stay_id)
  
  assign(
    "vis_lstm_matrix",
    vis_lstm %>%
      dplyr::filter(stay_id %in% sanity_idx)
  )
  
  # 完整度检测(载入LSTM模型前需要长度一致)
  sanity <- mean(vis_lstm_matrix$stay_id %in% sanity_idx)
  
  # 如果完整就写入数据
  if (sanity==1) {
    
    # 完成变量x的训练与验证数据集
    assign(
      paste0("x_train_", vis_len, "h"),
      vis_lstm_matrix %>% filter(stay_id %in% train_set_idx)
    )
    assign(
      paste0("x_test_", vis_len, "h"),
      vis_lstm_matrix %>% filter(stay_id %in% test_set_idx)
    )
    
    # 完成标签y的训练与验证数据集
    assign(paste0("y_train_", vis_len, "h"),
           dat %>%
             filter(stay_id %in% train_set_idx) %>%
             filter(stay_id %in% sanity_idx)
    ) 
    
    assign(paste0("y_test_", vis_len, "h"),
           dat %>%
             filter(stay_id %in% test_set_idx) %>%
             filter(stay_id %in% sanity_idx)) 
    
    # 创建写入数据函数
    write_data <- function(label) {
      write.csv(get(paste0(label, "_", vis_len, "h")),
                file = paste0("./csv/vis_lstm_matrix/icu_outcome/", label, "_", vis_len, "h.csv"))
    }
    
    write_data("x_train")
    write_data("x_test")
    write_data("y_train")
    write_data("y_test")
  }
})

# 各个小时vis完整性患者数目
sanity_proportion = data.frame(
  vis_len = sapply(vis_len, function(vis_len) {
    paste0("vis_", vis_len, "h")
  }),
  proportion = sapply(vis_len, function(vis_len) {
    vis_2010 %>%
      dplyr::filter(stay_id %in% dat$stay_id) %>%
      dplyr::filter(dur_h <= vis_len) %>%
      count(stay_id) %>%
      dplyr::filter(n == vis_len) %>%
      pull(stay_id) %>%
      length()
  })
)

write.csv(sanity_proportion,"./csv/vis_lstm_matrix/icu_outcome/sanity_proportion.csv")



