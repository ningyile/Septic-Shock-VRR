# 一键清空
rm(list = ls())
options(stringsAsFactors = F)

# 数据准备
load("./load/final.Rdata")

if (T) {
  # 两两比较数据比对
  psm_cohort1 = df %>% 
    filter(.,Group == '50% ≤ VRR' | Group == '0 ≤ VRR < 50%') %>% 
    mutate(Group_int = case_when(Group == '50% ≤ VRR' ~ 0, 
                                 Group == '0 ≤ VRR < 50%' ~ 1)) 
  
  psm_cohort2 = df %>% 
    filter(.,Group == '50% ≤ VRR' | Group == '-50% ≤ VRR < 0') %>% 
    mutate(Group_int = case_when(Group == '50% ≤ VRR' ~ 0, 
                                 Group == '-50% ≤ VRR < 0' ~ 1)) 
  
  psm_cohort3 = df %>% 
    filter(.,Group == '50% ≤ VRR' | Group == 'VRR < -50%') %>% 
    mutate(Group_int = case_when(Group == '50% ≤ VRR' ~ 0, 
                                 Group == 'VRR < -50%' ~ 1)) 
  
  psm_cohort4 = df %>% 
    filter(.,Group == '0 ≤ VRR < 50%' | Group == '-50% ≤ VRR < 0')%>% 
    mutate(Group_int = case_when(Group == '0 ≤ VRR < 50%' ~ 0, 
                                 Group == '-50% ≤ VRR < 0' ~ 1)) 
  
  psm_cohort5 = df %>% 
    filter(.,Group == '0 ≤ VRR < 50%' | Group == 'VRR < -50%') %>% 
    mutate(Group_int = case_when(Group == '0 ≤ VRR < 50%' ~ 0, 
                                 Group == 'VRR < -50%' ~ 1)) 
  
  
  psm_cohort6 = df %>% 
    filter(.,Group == '-50% ≤ VRR < 0' | Group == 'VRR < -50%') %>% 
    mutate(Group_int = case_when(Group == '-50% ≤ VRR < 0' ~ 0, 
                                 Group == 'VRR < -50%' ~ 1)) 
  
  save(psm_cohort1, 
       psm_cohort2, 
       psm_cohort3, 
       psm_cohort4, 
       psm_cohort5, 
       psm_cohort6, 
       file = "./load/pairwise_psm_cohort.Rdata")
}


