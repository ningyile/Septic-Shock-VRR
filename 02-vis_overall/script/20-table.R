# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

p_load(flextable, officer, tidyverse)

# 获取表格数据路径
path=list.files('./load/tbl',full.names = T)

# 加载数据
path %>% purrr::map(., load,.GlobalEnv)

# 设置文档格式
sect_properties <- prop_section(
  page_size = page_size(
    orient = "portrait",
    width = 29.7/2.54,
    height = 42/2.54),
  type = "nextColumn",
  page_margins = page_mar(
    bottom = 0.2,
    top = 0.2,
    right = 0.2,
    left = 0.2,
    header = 0.5,
    footer = 0.5,
    gutter = 0.5
  ))

# 保存为文档
save_as_docx(`Basic demographic of original cohort` = tbl1, 
             
             `Univariate Cox regression for ICU mortality` = tbl2, 
             `Pairwise comparisons between groups for ICU mortality survival curves` = tbl3, 
             `Multivariate Cox regression analysis for ICU mortality` = tbl4, 
             
             `Univariate Cox regression for in hospital mortality` = tbl5, 
             `Pairwise comparisons between groups for in hospital mortality survival curves` = tbl6, 
             `Multivariate Cox regression analysis for in hospital mortality` = tbl7, 
             
             # cohort1
             `Basic demographic of original cohort1 after multiple imputation` = tbl8, 
             `Basic demographic of matched cohort1 after multiple imputation` = tbl9, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort1` = tbl10, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort1` = tbl11, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort1` = tbl12, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort1` = tbl13, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort1` = tbl14, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort1` = tbl15, 
             `The number of SMD greater than 0.1 before and after matching for cohort1` = tbl_smd1, 
             
             # cohort2
             `Basic demographic of original cohort2 after multiple imputation` = tbl16, 
             `Basic demographic of matched cohort2 after multiple imputation` = tbl17, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort2` = tbl18, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort2` = tbl19, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort2` = tbl20, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort2` = tbl21, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort2` = tbl22, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort2` = tbl23,
             `The number of SMD greater than 0.1 before and after matching for cohort2` = tbl_smd2, 
             
             # cohort3
             `Basic demographic of original cohort3 after multiple imputation` = tbl24, 
             `Basic demographic of matched cohort3 after multiple imputation` = tbl25, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort3` = tbl26, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort3` = tbl27, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort3` = tbl28, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort3` = tbl29, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort3` = tbl30, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort3` = tbl31,
             `The number of SMD greater than 0.1 before and after matching for cohort3` = tbl_smd3, 
             
             # cohort4
             `Basic demographic of original cohort4 after multiple imputation` = tbl32, 
             `Basic demographic of matched cohort4 after multiple imputation` = tbl33, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort4` = tbl34, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort4` = tbl35, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort4` = tbl36, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort4` = tbl37, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort4` = tbl38, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort4` = tbl39,
             `The number of SMD greater than 0.1 before and after matching for cohort4` = tbl_smd4, 
             
             # cohort5
             `Basic demographic of original cohort5 after multiple imputation` = tbl40, 
             `Basic demographic of matched cohort5 after multiple imputation` = tbl41, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort5` = tbl42, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort5` = tbl43, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort5` = tbl44, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort5` = tbl45, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort5` = tbl46, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort5` = tbl47,
             `The number of SMD greater than 0.1 before and after matching for cohort5` = tbl_smd5, 
             
             # cohort6
             `Basic demographic of original cohort6 after multiple imputation` = tbl48, 
             `Basic demographic of matched cohort6 after multiple imputation` = tbl49, 
             `Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort6` = tbl50, 
             `Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort6` = tbl51, 
             `Multivariate Cox regression analysis for ICU mortality of matched cohort6` = tbl52, 
             `Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort6` = tbl53, 
             `Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort6` = tbl54, 
             `Multivariate Cox regression analysis for in hospital mortality of matched cohort6` = tbl55,
             `The number of SMD greater than 0.1 before and after matching for cohort6` = tbl_smd6, 
             
             path = "./img/tbl.docx", 
             pr_section = sect_properties)



