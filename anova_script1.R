library(dplyr)
library(ggplot2)
library(afex)
library(emmeans)
library(visdat)

#between measures ANOVA

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")

head(my_data)

my_data_tidied <- my_data %>%
  mutate(Condition = factor(Condition))
head(my_data_tidied)

my_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(Ability), sd = sd(Ability))

set.seed(1234)
my_data_tidied %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13))

model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied)
summary(model)

emmeans(model, pairwise ~ Condition)
emmeans(model, pairwise ~ Condition, adjust = "bonferroni")

#repeated measures ANOVA

rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
head(rm_data)

rm_data_tidied <- rm_data %>%
  mutate(Condition = factor(Condition))
head(rm_data_tidied)

rm_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd (RT))

rm_data_tidied %>%
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "RT (s)")

rm_model <- aov_4(RT ~ Condition + (1 + Condition | Participant), data = rm_data_tidied)
summary(rm_model)

anova(rm_model)

emmeans(rm_model, pairwise ~ Condition, adjust = "Bonferroni")

#factorial ANOVA

factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/factorial_data.csv")
head(factorial_data)

factorial_data_tidied <- factorial_data %>%
  mutate(Sentence = factor(Sentence), Context = factor(Context))
head(factorial_data_tidied)

factorial_data_tidied %>%
  group_by(Context, Sentence) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

vis_miss(factorial_data_tidied)

factorial_data_tidied %>%
  group_by(Context, Sentence) %>%
  summarise(mean_rt = mean(RT, na.rm = TRUE), sd_rt = sd(RT, na.rm = TRUE))

factorial_data_tidied %>%
  filter(!is.na(RT)) %>%
  ggplot(aes(x = Context:Sentence, y = RT, colour = Context:Sentence)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Context X Sentence", y = "RT (ms)")

model_subjects <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Subject), data = factorial_data_tidied, na.rm = TRUE)
anova(model_subjects)

model_items <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Item), data = factorial_data_tidied, na.rm = TRUE)
anova(model_items)

emmeans(model_subjects, pairwise ~ Context * Sentence, adjust = "none")

