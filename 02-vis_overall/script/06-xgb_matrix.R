rm(list = ls())
options(stringsAsFactors = F)

p_load(tidyverse,pROC)

# 数据准备
load("./load/mimic.Rdata")

# min_max标准化自定义函数
norm_minmax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dat = df_mimic %>%
  dplyr::select(vis_24h, vis_48h,
                vis_reduction_rate,
                # vis_last, vis_reduction_rate_last,
                max_vis_vasopressin_24h, max_vis_dopamine_24h, max_vis_dobutamine_24h,
                max_vis_epinephrine_24h, max_vis_milrinone_24h, max_vis_norepinephrine_24h,
                max_vis_vasopressin_48h, max_vis_dopamine_48h, max_vis_dobutamine_48h,
                max_vis_epinephrine_48h, max_vis_milrinone_48h, max_vis_norepinephrine_48h,
                icu_outcome) %>%
  # mutate(icu_outcome = replace(icu_outcome, icu_outcome == 0, -1)) %>% 
  mutate(icu_outcome = as.factor(icu_outcome)) 
  # 标准化
  # mutate(across(c(1:15), ~scale(.)))
  # mutate(across(c(1:15), ~norm_minmax(.)))



# 查看na情况
sapply(dat, function(x){sum(is.na(x))})
str(dat)

# 设置训练数据和测试数据
set.seed(100)
sample_split <- sample.split(dat$icu_outcome, SplitRatio = 0.8)
train_set <- subset(dat, sample_split == TRUE)
test_set <- subset(dat, sample_split == FALSE)

y_train <- as.integer(train_set$icu_outcome) - 1
y_test <- as.integer(test_set$icu_outcome) - 1
x_train <- train_set %>% select(-icu_outcome)
x_test <- test_set %>% select(-icu_outcome)

save(y_train, y_test, x_train, x_test, file = './load/xgb_matrix.Rdata')



