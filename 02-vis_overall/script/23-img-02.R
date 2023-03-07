# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

options(device = cairo_pdf)

p_load(survival, survminer, tidyverse, gtsummary, flextable, tools, ggpubr)

# 获取图像数据路径
path=list.files(c('./load/dlml/','./load/img'),full.names = T)

# 加载数据
path %>% purrr::map(., load,.GlobalEnv)


# LSTM即时预测、ROC、SHAP图整合
full_plot = ggarrange(lstm_model, roc_curve, 
                      widths = c(1,1),
                      ncol = 2, nrow = 1)
ggsave(full_plot,filename = './img/pdf/img_res1.pdf',height = 9, width = 18, encoding="MacRoman")


# 匹配后队列ICU死亡率KM曲线
full_plot = ggarrange(p8, p12,
                      p16, p20,
                      p24, p28,
                      widths = c(2,2,
                                 2,2,
                                 2,2),
                      ncol = 2, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res2.pdf',height = 18, width = 12, encoding="MacRoman")


# 匹配后队列ICU死亡率森林图
full_plot = ggarrange(p9, p13,
                      p17, p21,
                      p25, p29,
                      widths = c(3,3,
                                 3,3,
                                 3,3),
                      ncol = 2, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res3.pdf',height = 18, width = 18, encoding="MacRoman")


# 匹配后队列院内死亡率KM曲线
full_plot = ggarrange(p10, p14,
                      p18, p22,
                      p26, p30,
                      widths = c(2,2,
                                 2,2,
                                 2,2),
                      ncol = 2, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res4.pdf',height = 18, width = 12, encoding="MacRoman")


# 匹配后队列院内死亡率森林图
full_plot = ggarrange(p11, p15,
                      p19, p23,
                      p27, p31,
                      widths = c(3,3,
                                 3,3,
                                 3,3),
                      ncol = 2, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res5.pdf',height = 18, width = 18, encoding="MacRoman")




# 原始队列ICU死亡率KM曲线和累计事件表整合
full_plot = ggarrange(p3, p4, 
                      widths = c(1,1.5),
                      ncol = 2, nrow = 1)
ggsave(full_plot,filename = './img/pdf/img_sup2.pdf',height = 8, width = 16, encoding="MacRoman")



# 原始队列院内死亡率KM曲线和累计事件表整合
full_plot = ggarrange(p6, p7, 
                      widths = c(1,1.5),
                      ncol = 2, nrow = 1, align = "v")

ggsave(full_plot,filename = './img/pdf/img_sup3.pdf',height = 8, width = 16, encoding="MacRoman")


