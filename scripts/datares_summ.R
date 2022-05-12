library(readr)
library(tidyverse)
source("funcs.R")

full_df <- read_csv("cleaned_df.csv")
lexicon <- read_csv("lexicon.csv")
datares_df <- read_csv("survey_1.csv")
datares_df <- datares_df[complete.cases(datares_df),]

colnames(datares_df)[3:52] <- colnames(full_df)[1:50]

datares_df <- datares_df %>% 
  select(-Timestamp)

datares_df <- datares_df %>% 
  slice(2:nrow(datares_df))

datares_df <- cbind(datares_df[,1], summarise_person(datares_df[,-1]))



datares_summ <- datares_df %>% 
  rowwise %>%
  mutate(EXT_p = (extraversion/50) * 100, 
          EST_p = (neuroticism/50) * 100,
          AGR_p = (agreeableness/50) * 100,
          CSN_p = (conscientiousness/50) * 100,
          OPN_p = (openness/50) * 100) %>% 
  select(Name, EXT_p,EST_p,AGR_p,CSN_p,OPN_p)


datares_summ <- datares_summ %>% 
  mutate(EXT_lvl = case_when(
    EXT_p >= 0 & EXT_p < 10 ~ "0-10",
    EXT_p >= 10 & EXT_p < 20 ~ "10-20",
    EXT_p >= 20 & EXT_p < 30 ~ "20-30",
    EXT_p >= 30 & EXT_p < 40 ~ "30-40",
    EXT_p >= 40 & EXT_p < 50 ~ "40-50",
    EXT_p >= 50 & EXT_p < 60 ~ "50-60",
    EXT_p >= 60 & EXT_p < 70 ~ "60-70",
    EXT_p >= 70 & EXT_p < 80 ~ "70-80",
    EXT_p >= 80 & EXT_p < 90 ~ "80-90",
    EXT_p >= 90 & EXT_p < 100 ~ "90-100"),
    EXT_lvl = factor(EXT_lvl, levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")),
  
    EST_lvl = case_when(
      EST_p >= 0 & EST_p < 10 ~ "0-10",
      EST_p >= 10 & EST_p < 20 ~ "10-20",
      EST_p >= 20 & EST_p < 30 ~ "20-30",
      EST_p >= 30 & EST_p < 40 ~ "30-40",
      EST_p >= 40 & EST_p < 50 ~ "40-50",
      EST_p >= 50 & EST_p < 60 ~ "50-60",
      EST_p >= 60 & EST_p < 70 ~ "60-70",
      EST_p >= 70 & EST_p < 80 ~ "70-80",
      EST_p >= 80 & EST_p < 90 ~ "80-90",
      EST_p >= 90 & EST_p < 100 ~ "90-100"),
    EST_lvl = factor(EST_lvl, levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")),
    
    AGR_lvl = case_when(
      AGR_p >= 0 & AGR_p < 10 ~ "0-10",
      AGR_p >= 10 & AGR_p < 20 ~ "10-20",
      AGR_p >= 20 & AGR_p < 30 ~ "20-30",
      AGR_p >= 30 & AGR_p < 40 ~ "30-40",
      AGR_p >= 40 & AGR_p < 50 ~ "40-50",
      AGR_p >= 50 & AGR_p < 60 ~ "50-60",
      AGR_p >= 60 & AGR_p < 70 ~ "60-70",
      AGR_p >= 70 & AGR_p < 80 ~ "70-80",
      AGR_p >= 80 & AGR_p < 90 ~ "80-90",
      AGR_p >= 90 & AGR_p < 100 ~ "90-100"),
    AGR_lvl = factor(AGR_lvl, levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")),
    
    CSN_lvl = case_when(
      CSN_p >= 0 & CSN_p < 10 ~ "0-10",
      CSN_p >= 10 & CSN_p < 20 ~ "10-20",
      CSN_p >= 20 & CSN_p < 30 ~ "20-30",
      CSN_p >= 30 & CSN_p < 40 ~ "30-40",
      CSN_p >= 40 & CSN_p < 50 ~ "40-50",
      CSN_p >= 50 & CSN_p < 60 ~ "50-60",
      CSN_p >= 60 & CSN_p < 70 ~ "60-70",
      CSN_p >= 70 & CSN_p < 80 ~ "70-80",
      CSN_p >= 80 & CSN_p < 90 ~ "80-90",
      CSN_p >= 90 & CSN_p < 100 ~ "90-100"),
    CSN_lvl = factor(CSN_lvl, levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100")),
    
    OPN_lvl = case_when(
      OPN_p >= 0 & OPN_p < 10 ~ "0-10",
      OPN_p >= 10 & OPN_p < 20 ~ "10-20",
      OPN_p >= 20 & OPN_p < 30 ~ "20-30",
      OPN_p >= 30 & OPN_p < 40 ~ "30-40",
      OPN_p >= 40 & OPN_p < 50 ~ "40-50",
      OPN_p >= 50 & OPN_p < 60 ~ "50-60",
      OPN_p >= 60 & OPN_p < 70 ~ "60-70",
      OPN_p >= 70 & OPN_p < 80 ~ "70-80",
      OPN_p >= 80 & OPN_p < 90 ~ "80-90",
      OPN_p >= 90 & OPN_p < 100 ~ "90-100"),
    OPN_lvl = factor(OPN_lvl, levels=c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100"))
    )

datares_summ <- datares_summ %>% 
  ungroup
#write_csv(datares_summ, "datares_full.csv")


