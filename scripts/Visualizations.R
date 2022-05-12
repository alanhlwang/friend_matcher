library(readr)
library(tidyverse)
library(corrplot)
library(caret)
library(car)
library(ggcorrplot)
library(ggthemes)
library(gganimate)

df <- read_csv("cleaned_df.csv")
ovr_df <- read.csv("overall_df.csv")
ovr_df <- ovr_df[,-1]
ovr_df
            
#_______________Create DF, Polar Chart Visualization_____________________
class_df <- ovr_df %>%
  mutate(Class = names(.)[max.col(.)]) %>% 
  group_by(Class) %>% 
  summarise(n=n()) %>% 
  slice(-6)

class_df2 <- ovr_df %>%
  mutate(Class = names(.)[max.col(.)]) %>% 
  group_by(Class)


#________________Visualization_______________________
pb = ggplot(class_df,
            aes(x = Class, y = n))
pb = pb + geom_bar(stat = "identity")
pb = pb + aes(fill = Class)
pb = pb + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pb = pb + scale_fill_manual(values = c("#F0FFFF", "#ADD8E6", "#89CFF0", "#6495ED", "#0096FF"), 
                            name = "Type")

pb.polar = pb + coord_polar(clip = "off") +
  xlab("") + ylab("") +
  theme_fivethirtyeight() +
  ggtitle("Frequency of Max Personality Type") +
  theme(plot.title = element_text(hjust=0.5, size = 15), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "vertical",
        legend.key.size = unit(0.35, "cm")) 
pb.polar


#_________Visualization_________________
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(gganimate)
library(transformr)
df3 <- class_df2 %>% 
  filter(Class != "neuroticism")

ggplot(data=class_df2, aes(x=extraversion, group=Class, fill=Class)) +
  geom_density(adjust=1.5,  alpha=.4, color=NA) + 
  theme_minimal() +
  ggtitle("Distribution of Extraversion Scores\nby Personality Type") +
  theme(plot.title = element_text(hjust=0.5, size = 15))


df$EST

new <- df[complete.cases(df),]
df_new <- as.tibble(new) %>% 
  select(EXT1:EXT10)
df_new2 <- as.tibble(new) %>% 
  select(AGR1:AGR10)
df_new3 <- as.tibble(new) %>% 
  select(CSN1:CSN10)
df_new4 <- as.tibble(new) %>% 
  select(OPN1:OPN10)
df_new5 <- as.tibble(new) %>% 
  select(EST1:EST10)

new_viz <- df_new %>% mutate(sum_1 = rowSums(.[1:10]))
new_viz2 <- df_new %>% mutate(sum_1 = rowSums(.[11:20]))
new_viz3 <- df_new %>% mutate(sum_1 = rowSums(.[21:30]))
new_viz4 <- df_new %>% mutate(sum_1 = rowSums(.[31:40]))
new_viz5 <- df_new %>% mutate(sum_1 = rowSums(.[41:50]))

#_________Visualization_________________
ggplot(new_viz, aes(x=sum_1)) +
  geom_histogram(col = "black",fill = "blue") +
  theme_fivethirtyeight() +
  transition_states(new_viz$EXT1,1,2)
