# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

p_load(table1,tableone,Matching,survey,reshape2,ggplot2,
       tidyverse,mice,gtsummary,flextable,survminer)
# custommized funs developed by ningyile
p_load(big_strong)

set.seed(100)

gp_select <- c("50% ≤ VRR", "0 ≤ VRR < 50%", "-50% ≤ VRR < 0", "VRR < -50%")[c(1,3)]
col_select <- c("#75D054FF", "#2C728EFF", "#FB5D0F", "#FB75D3")[c(1,3)]

f='./load/matched_cohort2.Rdata'

if (!file.exists(f)) {
  # 数据准备
  load("./load/pairwise_psm_cohort.Rdata")
  
  str(psm_cohort2)
  
  df = psm_cohort2 
  
  # 重置因子
  df$Group=factor(df$Group)
  
  df$Group
  
  # 数据列名称顺序
  col_order <- df %>%
    names

  # 协变量
  vars <- df %>%
    names %>%
    purrr::discard(grepl("Group", .)) %>%
    purrr::keep(1:length(.) %in% (10:45))
  
  vars
  length(vars)
  
  # 含NA的协变量
  na_vars=apply(df, 2, function(x) {mean(is.na(x))})[vars] %>% 
    subset(.!=0 ) %>% 
    names()
  na_vars
  
  # 含NA的df
  na_df=df %>% dplyr::select(all_of(na_vars))
  # md.pattern(na_df)
  
  # 多重插补
  mi=mice(na_df, m=5, maxit = 50, method = 'pmm', seed = 500)
  summary(mi)
  
  # 原始数据和估算数据的分布
  if (F) {
    densityplot(mi)
    stripplot(mi, pch = 20, cex = 1.2)
  }
  
  # 获取完整插补数据框（5个模型中第3个）
  mi_df=complete(mi,3)
  
  # 合并插补数据
  df=df %>% 
    dplyr::select(-all_of(na_vars)) %>% 
    bind_cols(mi_df) %>% 
    relocate(any_of(col_order))
  
  # 查看即将匹配的数据框是否含NA
  mean(apply(df[vars], 2, function(x) {mean(is.na(x))}))
  
  ## Construct a table
  tabUnmatched <- CreateTableOne(vars = vars, strata = "Group", data = df, test = FALSE)
  
  ## Show table with SMD
  print(tabUnmatched, smd = TRUE)
  
  ## Count covariates with important imbalance
  addmargins(table(ExtractSmd(tabUnmatched) > 0.1))
  
  
  # Propensity score estimation
  ## Fit model
  fml = vars %>% 
    paste0('`',.,'`') %>% 
    paste(collapse = " + ") %>% 
    paste('Group ~ ',.) %>% 
    as.formula()
  fml
  
  psModel <- glm(formula = fml,
                 family  = binomial(link = "logit"),
                 data    = df)
  
  
  ## Predicted probability of being assigned to esm
  df$p_exp <- predict(psModel, type = "response")
  
  ## Predicted probability of being assigned to ctrl
  df$p_ctr <- 1 - df$p_exp
  
  ## 指派为exp组和ctrl组的预测概率
  df$p_assign[df$Group_int == 0] = df$p_exp[df$Group_int == 0]
  df$p_assign[df$Group_int == 1] = df$p_ctr[df$Group_int == 1]
  
  ## Smaller of pRhc vs pNoRhc for matching weight
  df$p_min <- pmin(df$p_exp, df$p_ctr)
  
  # Propensity score matching
  listMatch <- Match(Tr       = (df$Group_int == 0),      # Need to be in 0,1
                     ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                     X        = log(df$p_exp / df$p_ctr),
                     ## 1:1 matching
                     M        = 1,
                     ## caliper = 0.2 * SD(logit(PS))
                     caliper  = 0.2,
                     replace  = FALSE,
                     ties     = TRUE,
                     version  = "fast")
  
  
  ## Extract matched data
  matched_df=df[unlist(listMatch[c("index.treated","index.control")]), ]
  id=matched_df$stay_id
  
  ## Construct a table
  tabMatched <- CreateTableOne(vars = vars, strata = "Group", data = matched_df, test = FALSE)
  
  ## Show table with SMD
  print(tabMatched, smd = TRUE)
  
  ## Count covariates with important imbalance
  addmargins(table(ExtractSmd(tabMatched) > 0.1))
  
  ## 匹配前后SMD是否大于0.1情况
  tbl_before_matcing = addmargins(table(ExtractSmd(tabUnmatched) > 0.1)) %>% as.data.frame()
  tbl_after_matcing = addmargins(table(ExtractSmd(tabMatched) > 0.1)) %>% as.data.frame()
  
  tbl_summary_matching = merge(tbl_before_matcing, tbl_after_matcing, by = 'Var1', all = T) %>% 
    mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
    `colnames<-`(c('Items','Before matcing','After matcing')) %>% 
    mutate(Items = case_when(Items == 'FALSE' ~ 'SMD ≤ 0.1',
                             Items == 'TRUE' ~ 'SMD > 0.1',
                             Items == 'Sum' ~ 'Total number of covariables')) %>%
    flextable() %>% 
    set_caption(., "The number of SMD greater than 0.1 before and after matching for cohort2") 
  tbl_summary_matching
  tbl_smd2 = tbl_summary_matching
  save(tbl_smd2, file ="./load/tbl/tbl_smd2.Rdata" )
  
  save(df, matched_df, file = f)
}else{load(f)}

df$Group

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

# 多重插补后table1
tbl = make_table1(fml,df)
tbl
tbl16 <- tbl
save(tbl16, file ="./load/tbl/tbl16.Rdata")

# 匹配后table1
tbl = make_table1(fml,matched_df)
tbl
tbl17 <- tbl
save(tbl17, file ="./load/tbl/tbl17.Rdata")



# Matching weight
df$ps_weight = df$p_min / df$p_assign
# ipw
ipw_svydesign <- svydesign(ids = ~ stay_id, weights = ~ ps_weight, data = df)





# ICU死亡率

# PSM调查加权广义线性模型(ICU死亡率)
fml <- var_fml %>% paste('icu_outcome ~ Group + ', .) %>% as.formula()
fml

res.svyglm <- svyglm(fml,
                     family = binomial(link = "logit"),
                     design = ipw_svydesign,
                     data = df)
res.svyglm
summary(res.svyglm)
tbl_regression(res.svyglm,exponentiate = TRUE)

tbl18 <- tbl_regression(res.svyglm,exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Survey-weighted generalised linear model of with all covariates using IPW for ICU mortality of cohort2") 
tbl18
save(tbl18, file ="./load/tbl/tbl18.Rdata" )



# PSM调查加权COX回归模型(ICU死亡率)
fml <- var_fml %>% paste('Surv((icu_los), icu_outcome) ~ Group + ', .) %>% as.formula()
fml
res.svycox<-svycoxph(fml,
                     design = ipw_svydesign,
                     data = df)
res.svycox
summary(res.svycox)
tbl_regression(res.svycox,exponentiate = TRUE)

tbl19 <- tbl_regression(res.svycox,exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Survey-weighted Cox model with all covariates using IPW for ICU mortality of cohort2") 
tbl19
save(tbl19, file ="./load/tbl/tbl19.Rdata" )



# VRR分组生存曲线(ICU死亡率)
fit <- survfit(Surv((icu_los), icu_outcome) ~ Group, data = matched_df)

## km曲线
p = ggsurvplot(fit,
               title = "Kaplan-Meier survival curve for ICU mortality of matched cohort2",
               xlab = "Time(Days)", 
               ylab = "Surivival probability",
               xlim = c(0,100),
               break.x.by = 10,
               pval = T,  #添加P值
               conf.int = T,
               legend.title = "Group",
               legend.labs = gp_select,
               # risk.table = T, # 添加风险表
               # risk.table.y.text = T,
               cumevents = T, # 添加累计事件表
               cumevents.y.text = T,
               # ncensor.plot = TRUE, # 绘制t时刻删失对象的数量
               # ncensor.plot.title = "Censor",
               # surv.median.line = "hv", # 指定中位数生存率
               ggtheme = theme_bw(), # 主题设置
               linetype = 1, # 更改曲线类型
               # font.x = c(14, "plain", "black"),
               # font.y = c(14, "plain", "black"),
               # font.tickslab = c(14, "plain", "black"),
               # font.legend = 14,
               # font.subtitle = 14,
               # cumevents.font.tickslab = 14
               palette = col_select #指定色板
)

p

## 生存曲线和累计事件表整合为一个图
full_plot = ggarrange(p$plot, p$cumevents, heights = c(2, 0.8),
                      ncol = 1, nrow = 2, align = "v")
full_plot
p12=full_plot
save(p12,file = './load/img/p12.Rdata')

# 多元COX回归(ICU死亡率)
## 批量单变量，用sapply创建一个循环，把每个因素的公式写好
univ_formulas <- sapply(covariate_names,
                        function(x) as.formula(paste('Surv(icu_los, icu_outcome) ~', x)))

## 循环遍历
univ_models <- lapply(univ_formulas, function(x){coxph(x, data = df)})
## Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
## 结果输出一个dataframe：
res <- t(as.data.frame(univ_results)) %>% 
  `rownames<-`(covariate_names)

as.data.frame(res) %>% subset(., p.value <= 0.05)

fml <- as.data.frame(res) %>% subset(., p.value <= 0.05) %>% rownames() %>%
  c("Group", .) %>%
  paste(collapse = " + ") %>% paste('Surv(icu_los, icu_outcome) ~', .) %>% as.formula()
fml

## 多变量Cox回归
res.cox <- coxph(fml, data = matched_df)
summary(res.cox)
tbl_regression(res.cox,exponentiate = TRUE)

tbl20 <- tbl_regression(res.cox, exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Multivariate Cox regression analysis for ICU mortality of matched cohort2") 
tbl20
save(tbl20, file ="./load/tbl/tbl20.Rdata" )

## 森林图
p = ggforest(
  res.cox,
  data = matched_df,
  main = "Forest plot of multivariate Cox regression analysis for ICU mortality of matched cohort2

  Hazard Ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.8,
  refLabel = "reference",
  noDigits = 3
)
p
p13=p
save(p13,file = './load/img/p13.Rdata')






# 院内死亡率

# PSM调查加权广义线性模型(院内死亡率)
fml <- var_fml %>% paste('hos_outcome ~ Group + ', .) %>% as.formula()
fml

res.svyglm <- svyglm(fml,
                     family = binomial(link = "logit"),
                     design = ipw_svydesign,
                     data = df)
res.svyglm
summary(res.svyglm)
tbl_regression(res.svyglm,exponentiate = TRUE)

tbl21 <- tbl_regression(res.svyglm,exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Survey-weighted generalised linear model of with all covariates using IPW for in hospital mortality of cohort2") 
tbl21
save(tbl21, file ="./load/tbl/tbl21.Rdata" )



# PSM调查加权COX回归模型(院内死亡率)
fml <- var_fml %>% paste('Surv((hos_los), hos_outcome) ~ Group + ', .) %>% as.formula()
fml
res.svycox<-svycoxph(fml,
                     design = ipw_svydesign,
                     data = df)
res.svycox
summary(res.svycox)
tbl_regression(res.svycox,exponentiate = TRUE)

tbl22 <- tbl_regression(res.svycox,exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Survey-weighted Cox model with all covariates using IPW for in hospital mortality of cohort2") 
tbl22
save(tbl22, file ="./load/tbl/tbl22.Rdata" )



# VRR分组生存曲线(院内死亡率)
fit <- survfit(Surv((hos_los), hos_outcome) ~ Group, data = matched_df)

## km曲线
p = ggsurvplot(fit,
               title = "Kaplan-Meier survival curve for in hospital mortality of matched cohort2",
               xlab = "Time(Days)", 
               ylab = "Surivival probability",
               xlim = c(0,100),
               break.x.by = 10,
               pval = T,  #添加P值
               conf.int = T,
               legend.title = "Group",
               legend.labs = gp_select,
               # risk.table = T, # 添加风险表
               # risk.table.y.text = T,
               cumevents = T, # 添加累计事件表
               cumevents.y.text = T,
               # ncensor.plot = TRUE, # 绘制t时刻删失对象的数量
               # ncensor.plot.title = "Censor",
               # surv.median.line = "hv", # 指定中位数生存率
               ggtheme = theme_bw(), # 主题设置
               linetype = 1, # 更改曲线类型
               # font.x = c(14, "plain", "black"),
               # font.y = c(14, "plain", "black"),
               # font.tickslab = c(14, "plain", "black"),
               # font.legend = 14,
               # font.subtitle = 14,
               # cumevents.font.tickslab = 14
               palette = col_select #指定色板
)

p

## 生存曲线和累计事件表整合为一个图
full_plot = ggarrange(p$plot, p$cumevents, heights = c(2, 0.8),
                      ncol = 1, nrow = 2, align = "v")
full_plot
p14=full_plot
save(p14,file = './load/img/p14.Rdata')

# 多元COX回归(院内死亡率)
## 批量单变量，用sapply创建一个循环，把每个因素的公式写好
univ_formulas <- sapply(covariate_names,
                        function(x) as.formula(paste('Surv(hos_los, hos_outcome) ~', x)))

## 循环遍历
univ_models <- lapply(univ_formulas, function(x){coxph(x, data = df)})
## Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
## 结果输出一个dataframe：
res <- t(as.data.frame(univ_results)) %>% 
  `rownames<-`(covariate_names)

as.data.frame(res) %>% subset(., p.value <= 0.05)

fml <- as.data.frame(res) %>% subset(., p.value <= 0.05) %>% rownames() %>%
  c("Group", .) %>%
  paste(collapse = " + ") %>% paste('Surv(hos_los, hos_outcome) ~', .) %>% as.formula()
fml

## 多变量Cox回归
res.cox <- coxph(fml, data = matched_df)
summary(res.cox)
tbl_regression(res.cox,exponentiate = TRUE)

tbl23 <- tbl_regression(res.cox, exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Multivariate Cox regression analysis for in hospital mortality of matched cohort2") 
tbl23
save(tbl23, file ="./load/tbl/tbl23.Rdata" )

## 森林图
p = ggforest(
  res.cox,
  data = matched_df,
  main = "Forest plot of multivariate Cox regression analysis for in hospital mortality of matched cohort2

  Hazard Ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.8,
  refLabel = "reference",
  noDigits = 3
)
p
p15=p
save(p15,file = './load/img/p15.Rdata')







