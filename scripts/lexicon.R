lexicon <- read_csv("matches.csv")
s <- lexicon$...1

str_match(s, "\\d+\\-\\d+\\-\\d+\\-\\d+")
s_num <- strsplit(s,split="-")
s_num <- map(s_num, as.numeric)
s_num[1]

create_intervals <- function(x) {
  list(c(x[1],x[2]), c(x[3],x[4]))
}

s_num <- map(s_num, create_intervals)
test <- s_num[[1]] %>% 
  unlist

s_num <- map(s_num, unlist)

lexicon$min_r1 <- NA
lexicon$max_r1 <- NA
lexicon$min_r2 <- NA
lexicon$max_r2 <- NA

for (i in 1:length(s_num)) {
  lexicon$min_r1[i] <- s_num[[i]][1]
}
for (i in 1:length(s_num)) {
  lexicon$max_r1[i] <- s_num[[i]][2]
}
for (i in 1:length(s_num)) {
  lexicon$min_r2[i] <- s_num[[i]][3]
}
for (i in 1:length(s_num)) {
  lexicon$max_r2[i] <- s_num[[i]][4]
}

lexicon <- lexicon %>% 
  select(-range_1)

colnames(lexicon)[1] <- "range"

test <- lexicon %>% 
  select(Extrovert, min_r1:max_r2)
splits <- str_match(lexicon$range, "(\\d+\\-\\d+)(?:\\-)(\\d+\\-\\d+)")
fuller <- as.tibble(cbind(splits[,2:3], lexicon))

colnames(fuller)[1:2] <- c("r1", "r2")
testing <- fuller %>% 
  select(r1,r2,Extrovert)
neuro <- fuller %>% 
  select(r1,r2,`Neuroticism / Emotional Stability`)
csn_i <- fuller %>% 
  select(r1,r2,Conscientousness)
agr_i <- fuller %>% 
  select(r1,r2, Agreeableness)
opn_i  <- fuller %>% 
  select(r1,r2, `Openness / Intellect`)


ext_names<- sort(unique(as.character(unlist(testing[1:2]))))
ext_m <- matrix(0, 10, 10, dimnames = list(ext_names, ext_names))
ext_m[as.matrix(testing[c(1,2)])] <- testing$Extrovert
ext_m[as.matrix(testing[c(2,1)])] <- testing$Extrovert

est_names<- sort(unique(as.character(unlist(neuro[1:2]))))
est_m <- matrix(0, 10, 10, dimnames = list(est_names, est_names))
est_m[as.matrix(neuro[c(1,2)])] <- neuro$`Neuroticism / Emotional Stability`
est_m[as.matrix(neuro[c(2,1)])] <- neuro$`Neuroticism / Emotional Stability`

csn_names<- sort(unique(as.character(unlist(neuro[1:2]))))
csn_m <- matrix(0, 10, 10, dimnames = list(csn_names, csn_names))
csn_m[as.matrix(csn_i[c(1,2)])] <- csn_i$Conscientousness
csn_m[as.matrix(csn_i[c(2,1)])] <- csn_i$Conscientousness

agr_names<- sort(unique(as.character(unlist(neuro[1:2]))))
agr_m <- matrix(0, 10, 10, dimnames = list(agr_names, agr_names))
agr_m[as.matrix(agr_i[c(1,2)])] <- agr_i$Agreeableness
agr_m[as.matrix(agr_i[c(2,1)])] <- agr_i$Agreeableness

opn_names<- sort(unique(as.character(unlist(neuro[1:2]))))
opn_m <- matrix(0, 10, 10, dimnames = list(opn_names, opn_names))
opn_m[as.matrix(opn_i[c(1,2)])] <- opn_i$`Openness / Intellect`
opn_m[as.matrix(opn_i[c(2,1)])] <- opn_i$`Openness / Intellect`

# write_rds(ext_m, "ext_m.rds")
# write_rds(est_m, "est_m.rds")
# write_rds(opn_m, "opn_m.rds")
# write_rds(agr_m, "agr_m.rds")
# write_rds(csn_m, "csn_m.rds")
# write_csv(lexicon, "lexicon.csv")

