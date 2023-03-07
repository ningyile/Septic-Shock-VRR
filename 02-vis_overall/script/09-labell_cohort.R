rm(list = ls())
options(stringsAsFactors = F)

# 数据准备
load("./load/mimic.Rdata")

df <- df_mimic %>%
  mutate(icu_los=(icu_los/24),
         hos_los=(hos_los/24)) %>% 
  # 挑选出变量
  dplyr::select(
    ## 基本信息
    stay_id,
    intime,
    outtime,
    deathtime,
    dischtime,
    icu_outcome,
    icu_los,
    hos_outcome,
    hos_los,
    ## 协变量
    age,
    gender,
    weight,
    sapsii,
    sofa,
    first_nonzero_vis_2010,
    vent,
    sedative,
    icd_afib,
    icd_cad,
    icd_chf,
    icd_copd,
    icd_liver,
    icd_malignancy,
    icd_renal,
    icd_stroke,
    vs_heart_rate_first,
    vs_map_first,
    vs_temp_first,
    lab_wbc_first,
    lab_hemoglobin_first,
    lab_platelet_first,
    lab_sodium_first,
    lab_potassium_first,
    lab_chloride_first,
    lab_bun_first,
    lab_creatinine_first,
    lab_ph_first,
    lab_po2_first,
    lab_pco2_first,
    lab_lactate_first,
    lab_bicarbonate_first,
    vs_cvp_tag,
    lab_bnp_tag,
    lab_troponin_tag,
    lab_creatinine_kinase_tag,
    group
  ) %>% 
  # 更改因子
  within(., {
    ## 协变量
    group    = factor(group, levels=c(1, 2, 3, 4), labels = c("50% ≤ VRR", "0 ≤ VRR < 50%", "-50% ≤ VRR < 0", "VRR < -50%"))
    gender   = factor(gender, levels=c('F', 'M'), labels=c("Female", "Male"))
    ## 治疗
    vent     = factor(vent, levels=c(0, 1), labels=c("NO", "YES"))
    sedative = factor(sedative, levels=c(0, 1), labels=c("NO", "YES"))
    ## 并发症
    icd_afib       = factor(icd_afib, levels=c(0, 1), labels=c("NO", "YES"))
    icd_cad        = factor(icd_cad, levels=c(0, 1), labels=c("NO", "YES"))
    icd_chf        = factor(icd_chf, levels=c(0, 1), labels=c("NO", "YES"))
    icd_copd       = factor(icd_copd, levels=c(0, 1), labels=c("NO", "YES"))
    icd_liver      = factor(icd_liver, levels=c(0, 1), labels=c("NO", "YES"))
    icd_malignancy = factor(icd_malignancy, levels=c(0, 1), labels=c("NO", "YES"))
    icd_renal      = factor(icd_renal, levels=c(0, 1), labels=c("NO", "YES"))
    icd_stroke     = factor(icd_stroke, levels=c(0, 1), labels=c("NO", "YES"))
    ## 生命体征是否检测
    vs_cvp_tag                 = factor(vs_cvp_tag, levels=c(0, 1), labels=c("NO", "YES"))
    ## 实验室检查是否检测
    lab_bnp_tag                = factor(lab_bnp_tag, levels=c(0, 1), labels=c("NO", "YES"))
    lab_troponin_tag           = factor(lab_troponin_tag, levels=c(0, 1), labels=c("NO", "YES"))
    lab_creatinine_kinase_tag  = factor(lab_creatinine_kinase_tag, levels=c(0, 1), labels=c("NO", "YES"))
  }) %>% 
  # 更改标签
  rename(., 
         Group = group,
         Age = age,
         Gender = gender,
         Weight = weight,
         `SAPS II score` = sapsii,
         `SOFA score` = sofa,
         `First nonzero VIS score` = first_nonzero_vis_2010,
         ## 治疗
         `Mechanical ventilation` = vent,
         Sedative = sedative,
         ## 并发症
         AFIB = icd_afib,
         CAD = icd_cad,
         CHF = icd_chf,
         COPD = icd_copd,
         Liver = icd_liver,
         Malignancy = icd_malignancy,
         Renal = icd_renal,
         Stroke = icd_stroke,
         ## 生命体征
         `Heart rate` = vs_heart_rate_first,
         MAP= vs_map_first,
         Temperature = vs_temp_first,
         ## 生命体征是否检测
         `CVP (tested)` = vs_cvp_tag,
         ## 实验室检查
         WBC = lab_wbc_first,
         Hemoglobin = lab_hemoglobin_first,
         Platelet = lab_platelet_first,
         Sodium = lab_sodium_first,
         Potassium = lab_potassium_first,
         Chloride = lab_chloride_first,
         Bun = lab_bun_first,
         Creatinine = lab_creatinine_first,
         PH = lab_ph_first,
         PO2 = lab_po2_first,
         PCO2 = lab_pco2_first,
         Lactate = lab_lactate_first,
         Bicarbonate = lab_bicarbonate_first,
         ## 实验室检查是否检测
         `BNP (tested)` = lab_bnp_tag,
         `Troponin (tested)` = lab_troponin_tag,
         `Creatinine kinase (tested)` = lab_creatinine_kinase_tag
  )


save(df,file = "./load/final.Rdata")




