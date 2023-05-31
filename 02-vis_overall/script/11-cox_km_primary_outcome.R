# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

p_load(survival, survminer, tidyverse, gtsummary, flextable)

gp_select = c("50% ≤ VRR", "0 ≤ VRR < 50%", "-50% ≤ VRR < 0", "VRR < -50%")
col_select = c("#75D054FF", "#2C728EFF", "#FB5D0F", "#FB75D3")

# 数据准备
load("./load/final.Rdata")

# VRR分组单变量
res.cox <- coxph(Surv(icu_los, icu_outcome) ~ Group, data = df)
res.cox
summary(res.cox)

tbl_regression(res.cox,exponentiate = TRUE) 

tbl2 <- tbl_regression(res.cox, exponentiate = TRUE) %>%
  as_flex_table() %>%
  set_caption(., "Univariate Cox regression for ICU mortality") 

tbl2

save(tbl2, file ="./load/tbl/tbl2.Rdata" )


#森林图
p = ggforest(
  res.cox,
  data = df,
  main = "Forest plot of univariate Cox regression analysis for ICU mortality
  
  Hazard Ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.8,
  refLabel = "reference",
  noDigits = 3
)
p
p2=p
save(p2,file = './load/img/p2.Rdata')

# VRR分组生存曲线
fit <- survfit(Surv((icu_los), icu_outcome) ~ Group, data = df)

# km曲线
p = ggsurvplot(fit,
               title = "Kaplan-Meier survival curve for ICU mortality of original cohort", 
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

# 生存曲线和累计事件表整合为一个图
full_plot = ggarrange(p$plot, p$cumevents, heights = c(2, 0.8),
                      ncol = 1, nrow = 2, align = "v")
full_plot
p3=full_plot
save(p3,file = './load/img/p3.Rdata')


# 生存曲线两两比对
res_pairwise <- pairwise_survdiff(Surv(icu_los, icu_outcome) ~ Group, data = df)
res_pairwise

tbl3 = res_pairwise[["p.value"]] %>% 
  as.data.frame %>% 
  mutate_all(function(x){ifelse(x>0.05,round(x,2),formatC(x, format="e", digits = 2))}) %>% 
  mutate_all(function(x){ifelse(is.na(x), '-', x)}) %>%
  mutate(Group=c('0 ≤ VRR < 50%','-50% ≤ VRR < 0','VRR < -50%'), .before = 1) %>% 
  flextable() %>% 
  set_caption(., "Pairwise comparisons between groups for ICU mortality survival curves") 

tbl3
save(tbl3, file ="./load/tbl/tbl3.Rdata" )


# 批量单变量
covariate_names <- df %>%
  names %>%
  purrr::discard(grepl("Group", .)) %>%
  purrr::keep(1:length(.) %in% (10:45)) %>% 
  paste0('`',.,'`')

covariate_names
length(covariate_names)

# 用sapply创建一个循环，把每个因素的公式写好
univ_formulas <- sapply(covariate_names,
                        function(x) as.formula(paste('Surv(icu_los, icu_outcome) ~', x)))

# 循环遍历
univ_models <- lapply(univ_formulas, function(x){coxph(x, data = df)})
# Extract data 
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
# 结果输出一个dataframe：
res <- t(as.data.frame(univ_results)) %>% 
  `rownames<-`(covariate_names)
  
as.data.frame(res) %>% subset(., p.value <= 0.05)


fml <- as.data.frame(res) %>% subset(., p.value <= 0.05) %>% rownames() %>%
  c("Group", .) %>%
  paste(collapse = " + ") %>% paste('Surv(icu_los, icu_outcome) ~', .) %>% as.formula()
fml


# 多变量Cox回归
res.cox <- coxph(fml, data = df)
summary(res.cox)
tbl_regression(res.cox,exponentiate = TRUE)

tbl4 <- tbl_regression(res.cox, exponentiate = TRUE) %>%
  as_flex_table() %>% 
  set_caption(., "Multivariate Cox regression analysis for ICU mortality") 
tbl4
save(tbl4, file ="./load/tbl/tbl4.Rdata" )


#森林图
p = ggforest(
  res.cox,
  data = df,
  main = "Forest plot of multivariate Cox regression analysis for ICU mortality

  Hazard Ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.8,
  refLabel = "reference",
  noDigits = 3
)
p
p4=p
save(p4,file = './load/img/p4.Rdata')



