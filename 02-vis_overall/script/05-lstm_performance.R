rm(list = ls())
options(stringsAsFactors = F)

p_load(reportROC,ggpubr)

# 模型参数数据
vis_len=c(1:48)
mod_perf <- map_df(vis_len, function(vis_len) {
  pre = read.csv(paste0("./csv/vis_lstm_prediction/icu_outcome/prediction_", vis_len, "h.csv"))
  reportROC(pre$labels, pre$probs_of_pre, plot = F)
}) %>% 
  mutate_all(., function(x) as.numeric(as.character(x))) %>% 
  `rownames<-`(paste0(vis_len, "h")) %>% 
  mutate(dur_h=vis_len) 

# AUROC折线图
p1 = ggplot(mod_perf, aes(x = dur_h , y = AUC)) +
  ggtitle("AUROC after admission") +
  # 带状图函数：ymin设置CI下界，ymax设置CI上界；
  geom_ribbon(
    aes(ymin = AUC.low, ymax = AUC.up),
    linetype = 10,
    fill = "steelblue",
    # colour = "steelblue",
    alpha = 0.3
  ) +
  # 折线图函数
  geom_line(colour = "steelblue") +
  xlab("Time after admission(h)") +
  ylab("AUROC") +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +   # 删去网格线
  theme(panel.border = element_blank()) +   # 删去外层边框
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5)) + # 主标题居中 
  theme(axis.line = element_line(size = 0.5, colour = "black")) +  # 加上坐标轴（无刻度、无标签）
  theme(axis.text = element_text(size=12,face = "bold")) +
  theme(axis.title = element_text(size=14,face = "bold"))

p1

# LSTM模型model performance
raw = mod_perf %>%
  t %>%
  as.data.frame %>%
  select(1, 12, 24, 36, 48) %>%
  dplyr::slice(grep("AUC|PLR|NLR|PPV|NPV", rownames(.))) %>%
  dplyr::slice(-2) %>%
  t %>%
  round(.,2) %>% 
  as.data.frame

mp = data.frame(
  AUROC = paste0(raw$AUC, "(", raw$AUC.low, "-", raw$AUC.up, ")"),
  PLR = paste0(raw$PLR, "(", raw$PLR.low, "-", raw$PLR.up, ")"),
  NLR = paste0(raw$NLR, "(", raw$NLR.low, "-", raw$NLR.up, ")"),
  PPV = paste0(raw$PPV, "(", raw$PPV.low, "-", raw$PPV.up, ")"),
  NPV = paste0(raw$NPV, "(", raw$NPV.low, "-", raw$NPV.up, ")")
) %>%
  t %>%
  as.data.frame %>% 
  `colnames<-`(c("1h","12h","24","36h","48h")) %>% 
  mutate(metrics=rownames(.))

mpm <- mp %>% gather(key = "dur_h", value = "value", -metrics)
mpm$metrics<-factor(mpm$metrics,levels = c("PLR","NLR","PPV","NPV","AUROC"))
mpm$dur_h<-factor(mpm$dur_h,levels = c("1h","12h","24","36h","48h"))
# 折线图下加表格
tbl <- ggplot(mpm, aes(x = dur_h, y = metrics, label = format(value))) +
  geom_text(size = 5) + theme_bw() +
  theme(panel.grid = element_blank()) +   # 删去网格线
  theme(panel.border = element_blank()) +  # 删去外层边框
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text=element_text(size=12,face = "bold")) +
  theme(axis.ticks = element_blank())

tbl


p=ggarrange(p1, tbl, heights = c(4, 1),
            ncol = 1, nrow = 2, align = "v")

lstm_model=p
lstm_model

save(lstm_model,file = './load/dlml/lstm_model.Rdata')

