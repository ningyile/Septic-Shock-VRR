rm(list = ls())
options(stringsAsFactors = F)

p_load(pROC,tidyverse)

# 加载数据
pre_xgb=read.csv("./csv/pre_xgb.csv") %>% 
  dplyr::select(labels,prob_1) %>% 
  `colnames<-`(c("labels","prob"))
pre_lstm=read.csv("./csv/vis_lstm_prediction/icu_outcome/prediction_48h.csv")


if (F) {
  # xgb模型ROC曲线
  roc(
    pre_xgb$labels,
    pre_xgb$prob,
    plot = TRUE,
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8",
    lwd = 4,
    print.auc = TRUE
  )
  
  # LSTM模型ROC曲线
  roc(
    pre_lstm$labels,
    pre_lstm$probs_of_pre,
    plot = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#4daf4a",
    lwd = 4,
    print.auc = TRUE,
    add = TRUE,
    print.auc.y = 40
  )
  
  # 添加图例
  legend(
    "bottomright",
    legend = c("XGB model", "LSTM model"),
    col = c("#377eb8", "#4daf4a"),
    lwd = 4
  )
}

# 使用ggroc转化为ggplot2对象以便于存储
# 创建roc对象
roc_xgb <- roc(pre_xgb$labels, pre_xgb$prob)
roc_lstm <- roc(pre_lstm$labels, pre_lstm$probs_of_pre)

# ggroc基本函数
roc <- ggroc(list(xgb=roc_xgb, lstm=roc_lstm), 
             alpha = 1, 
             linetype = 1, 
             size = 2)
roc

# 添加ggplot2参数
p = roc + 
  xlab("False Positive Percentage") + 
  ylab("True Postive Percentage") + 
  ggtitle("ROC curves") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype=5) +
  # 修改颜色
  scale_colour_manual(
    values = c("#2C728EFF", "#75D054FF"),
    name = "Model",
    breaks = c("lstm", "xgb"),
    labels = c(
      paste("LSTM model at 48h with dynamic VIS features,", "AUROC:", round(roc_lstm[["auc"]], 2)),
      paste("XGBoost model with static VIS features,", "AUROC:", round(roc_xgb[["auc"]], 2))
    )
  ) +
  theme_bw() +
  # theme(panel.grid = element_blank()) +   # 删去网格线
  theme(panel.border = element_rect(colour = "black", size=1)) +   # 删去外层边框
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5)) + # 主标题居中 
  theme(axis.text = element_text(size=12,face = "bold")) +
  theme(axis.title = element_text(size=14,face = "bold")) + 
  # 修改图例
  theme(
    legend.position = c(1, 0),#plot内右下角
    legend.justification = c(1,0),#固定图例右下角
    legend.background = element_blank(),#图例背景色
    legend.key = element_blank(),#图标背景色
    legend.box.background = element_rect(color = "black",linetype = 1, size = 0.5), #图例外框和背景色默认填充白色
    legend.margin = margin(6, 6, 6, 6)#边框大小调整
  )

roc_curve=p
roc_curve

save(roc_curve,file = './load/dlml/roc_curve.Rdata')








