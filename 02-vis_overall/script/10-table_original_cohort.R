# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

p_load(table1,flextable)
# custommized funs developed by ningyile
p_load(big_strong)

# 数据准备
load("./load/final.Rdata")

# 查看变量数据类型
str(df)

# 协变量名
covariate_names <- df %>%
  names %>%
  purrr::discard(grepl("Group", .)) %>%
  purrr::keep(1:length(.) %in% (10:45)) %>% 
  paste0('`',.,'`')

covariate_names
length(covariate_names)

# 通用formula部分
var_fml <- covariate_names %>% 
  paste(collapse = " + ")
var_fml

# 制表fml
fml=var_fml %>% paste('~',.,'| Group') %>% as.formula()
fml

# 制表table1
tbl=make_table1(fml,df)
tbl

tbl1 <- tbl

save(tbl1, file ="./load/tbl/tbl1.Rdata" )




