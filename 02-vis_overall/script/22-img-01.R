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


# 生存曲线和累计事件表整合为一个图
full_plot = ggarrange(p3, p4, 
                      widths = c(1,1.5),
                      ncol = 2, nrow = 1)
ggsave(full_plot,filename = './img/pdf/img_res2.pdf',height = 9, width = 18, encoding="MacRoman")

full_plot = ggarrange(p8, p9,
                      p12, p13,
                      p16, p17,
                      p20, p21,
                      p24, p25,
                      p28, p29,
                      widths = c(2,3,
                                 2,3,
                                 2,3,
                                 2,3),
                      ncol = 4, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res3.pdf',height = 20, width = 30, encoding="MacRoman")



full_plot = ggarrange(p6, p7, 
                      widths = c(1,1.5),
                      ncol = 2, nrow = 1, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res4.pdf',height = 9, width = 18, encoding="MacRoman")


full_plot = ggarrange(p10, p11,
                      p14, p15,
                      p18, p19,
                      p22, p23,
                      p26, p27,
                      p30, p31,
                      widths = c(2,3,
                                 2,3,
                                 2,3,
                                 2,3),
                      ncol = 4, nrow = 3, align = "v")

ggsave(full_plot,filename = './img/pdf/img_res5.pdf',height = 20, width = 30, encoding="MacRoman")





