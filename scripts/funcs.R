summarise_person <- function(df) {
df <- df %>% 
  summarise(across(.fns = as.numeric))

sum(df$EXT1[!is.na(df$EXT1)])
head(df)

df$EXT2 = 6 - df$EXT2
df$EXT4 = 6 - df$EXT4
df$EXT6 = 6 - df$EXT6
df$EXT8 = 6 - df$EXT8
df$EXT10 = 6 - df$EXT10
df$EST2 = 6 - df$EST2
df$EST4 = 6 - df$EST4
df$AGR1 = 6 - df$AGR1
df$AGR3 = 6 - df$AGR3
df$AGR5 = 6 - df$AGR5
df$AGR7 = 6 - df$AGR7
df$CSN2 = 6 - df$CSN2
df$CSN4 = 6 - df$CSN4
df$CSN6 = 6 - df$CSN6
df$CSN8 = 6 - df$CSN8
df$OPN2 = 6 - df$OPN2
df$OPN4 = 6 - df$OPN4
df$OPN6 = 6 - df$OPN6

#________________Create Overall Score DF_________________________
df["extraversion"]= (df$EXT1 + df$EXT2 + df$EXT3 + df$EXT4 + df$EXT5 + df$EXT6 + df$EXT7 + df$EXT8 + df$EXT9 + df$EXT10)
df["neuroticism"] = (df$EST1 + df$EST2 + df$EST3 + df$EST4 + df$EST5 + df$EST6 + df$EST7 + df$EST8 + df$EST9 + df$EST10)
df["agreeableness"] = (df$AGR1 + df$AGR2 + df$AGR3 + df$AGR4 + df$AGR5 + df$AGR6 + df$AGR7 + df$AGR8 + df$AGR9 + df$AGR10)
df["conscientiousness"] = (df$CSN1 + df$CSN2 + df$CSN3 + df$CSN4 + df$CSN5 + df$CSN6 + df$CSN7 + df$CSN8 + df$CSN9 + df$CSN10)
df["openness"] = (df$OPN1 + df$OPN2 + df$OPN3 + df$OPN4 + df$OPN5 + df$OPN6 + df$OPN7 + df$OPN8 + df$OPN9 + df$OPN10)

df
}