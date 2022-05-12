rm(list=ls())
source("preamble.R")
dr_df <- read_csv("datares_full.csv")
agr_m <- read_rds("agr_m.rds")
est_m <- read_rds("est_m.rds")
ext_m <- read_rds("ext_m.rds")
opn_m <- read_rds("opn_m.rds")
csn_m <- read_rds("csn_m.rds")

replace_s <- c("0-10","10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")
agr_m <- as.matrix(agr_m)
rownames(agr_m) <- replace_s
colnames(agr_m) <- replace_s

est_m <- as.matrix(est_m)
rownames(est_m) <- replace_s
colnames(est_m) <- replace_s

opn_m <- as.matrix(opn_m)
rownames(opn_m) <- replace_s
colnames(opn_m) <- replace_s

ext_m <- as.matrix(ext_m)
rownames(ext_m) <- replace_s
colnames(ext_m) <- replace_s

csn_m <- as.matrix(csn_m)
rownames(csn_m) <- replace_s
colnames(csn_m) <- replace_s

dr_df$AGR_lvl <- as.factor(dr_df$AGR_lvl)
dr_df$EXT_lvl <- as.factor(dr_df$EXT_lvl)
dr_df$EST_lvl <- as.factor(dr_df$EST_lvl)
dr_df$CSN_lvl <- as.factor(dr_df$CSN_lvl)
dr_df$OPN_lvl <- as.factor(dr_df$OPN_lvl)

p1_i <- levels(dr_df$AGR_lvl)[as.numeric(dr_df[1,][7])] == rownames(agr_m)
p2_i <- levels(dr_df$AGR_lvl)[as.numeric(dr_df[2,7])] == colnames(agr_m)
agr_m[p1_i, p2_i]

person_1 <- dr_df[1,7]


matches_list <- list()

for (j in 1:nrow(dr_df)) {
  matches_df <- data.frame(Name = character(30),
                           match_ext = character(30),
                           match_est = character(30),
                           match_arg = character(30),
                           match_csn = character(30),
                           match_opn = character(30))
  
  for (i in 1:nrow(dr_df)) {
    if (i == j) {
      next
    }
    p1_i <- levels(dr_df$EXT_lvl)[as.numeric(dr_df[j,7])] == rownames(ext_m)
    p1_iest <- levels(dr_df$EST_lvl)[as.numeric(dr_df[j,8])] == rownames(est_m)
    p1_iagr <- levels(dr_df$AGR_lvl)[as.numeric(dr_df[j,9])] == rownames(agr_m)
   p1_icsn <- levels(dr_df$CSN_lvl)[as.numeric(dr_df[j,10])] == rownames(csn_m)
   p1_iopn <- levels(dr_df$OPN_lvl)[as.numeric(dr_df[j,11])] == rownames(opn_m)

   p2_i <- levels(dr_df$EXT_lvl)[as.numeric(dr_df[i,7])] == colnames(ext_m)
   p2_iest <- levels(dr_df$EST_lvl)[as.numeric(dr_df[i,8])] == colnames(est_m)
   p2_iagr <- levels(dr_df$AGR_lvl)[as.numeric(dr_df[i,9])] == colnames(agr_m)
   p2_icsn <- levels(dr_df$CSN_lvl)[as.numeric(dr_df[i,10])] == colnames(csn_m)
    p2_iopn <- levels(dr_df$OPN_lvl)[as.numeric(dr_df[i,11])] == colnames(opn_m)
  
   matches_df[i,1] <-  dr_df[i,1]
   matches_df[i,2] <- ext_m[p1_i, p2_i]
   matches_df[i,3] <- est_m[p1_iest, p2_iest]
   matches_df[i,4] <-  agr_m[p1_iagr, p2_iagr]
   matches_df[i,5] <- csn_m[p1_icsn, p2_icsn]
   matches_df[i,6] <- opn_m[p1_iopn, p2_iopn]
  }
  
  matches_list[[j]] <- matches_df
}

names(matches_list) <- dr_df$Name
#write_rds(matches_list, "matches_list.rds")

#matches_list <- read_rds("matches_list.rds")
matches_list <- map(matches_list,function(x){x %>% 
    filter(Name != "")})


encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
matches_list[[1]]

encode_list <- function(x) {
  x$agr_encode <- encode_ordinal(x[["match_arg"]], order = c("Low", "Fair", "Good", "High"))
  x$opn_encode <- encode_ordinal(x[["match_opn"]], order = c("Low", "Fair", "Good", "High"))
  x$csn_encode <- encode_ordinal(x[["match_csn"]], order = c("Low", "Fair", "Good", "High"))
  x$ext_encode <- encode_ordinal(x[["match_ext"]], order = c("Low", "Fair", "Good", "High"))
  x$est_encode <- encode_ordinal(x[["match_est"]], order = c("Low", "Fair", "Good", "High"))
  x
}

matches_list_e <- map(matches_list,encode_list)
matches_list_e

get_compatibility <- function(x) {
  x %>% 
   rowwise %>% 
    mutate(Compatibility = sum(agr_encode, opn_encode, csn_encode, ext_encode, est_encode)) %>% 
    select(Name, Compatibility) %>% 
    arrange(desc(Compatibility))
}

compatible_list <- map(matches_list_e, get_compatibility)
matches_list_e[[1]]

for (i in 1:length(compatible_list)) {
  current_name <- names(compatible_list)[i]
  compatible_list[[i]] <- compatible_list[[i]] %>% 
    filter(Name != current_name)
}

#write_rds(compatible_list, "compatible.rds")

concise_list <- map(compatible_list, function(x) {
  x %>% 
    ungroup %>% 
    slice(1:5)
})

#write_rds(concise_list, "concise_compatible.rds")
