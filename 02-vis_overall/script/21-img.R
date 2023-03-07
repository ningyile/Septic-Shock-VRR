# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

options(device = cairo_pdf)

p_load(survival, survminer, tidyverse, gtsummary, flextable, tools, pheatmap)

# 获取图像数据路径
path=list.files('./load/img',full.names = T)

# 加载数据
path %>% purrr::map(., load,.GlobalEnv)

# 获取图像数据名字
plot_data <- list.files('./load/img') %>% 
  tools::file_path_sans_ext() %>% 
  str_sort(., numeric = T)

# 合并为list
plot_list = mget(plot_data)

# 输出为单个pdf
if (T) {
  pdf("./img/pdf/img.pdf",width = 8, height = 8, encoding="MacRoman")
  invisible(lapply(plot_list, print))
  dev.off()
}






# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

options(device = cairo_pdf)

#深度学习与机器学习图像合并
load('./load/dlml/lstm_model.Rdata')
load('./load/dlml/roc_curve.Rdata')


# 获取图像数据名字
plot_data <- c("roc_curve", "lstm_model")

# 合并为list
plot_list = mget(plot_data)

# 输出为单个pdf
if (T) {
  pdf("./img/pdf/dlml.pdf",width = 9, height = 9, encoding="MacRoman")
  invisible(lapply(plot_list, print))
  dev.off()
}




