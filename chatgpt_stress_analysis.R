#loading libraries
library("dplyr")
library("tidyverse")
library("ggplot2")

#Importing dataset on RStudio using the directory 

#cleaning the data

##removing unnecessary rows 
datax = data[-c(1,2),]

##keeping necessary columns
datax = datax[, c("Finished","Q1","Q2","Q4","Q5","Q6","Q3-Randomized_DO")]

##removing rows with missing data
datax <- datax %>% filter(!is.na(Q5))

##renaming Q3-Randomized_DO
datax <- datax %>%
  rename(trust = "Q1")

datax <- datax %>%
  rename(pre = "Q2")

datax <- datax %>%
  rename(post = "Q4")

datax <- datax %>%
  rename(sat = "Q5")

datax <- datax %>%
  rename(group = "Q3-Randomized_DO")


##extracting the numbers from mixed text¥
datax <- datax %>% mutate_at(vars(trust, pre, post, sat), parse_number)


#How many are in the treatment group and the control group?
datax %>%
  count(group)

#new column for the post intervention stress level result  
datax$stress_diff <- datax$Q2 - datax$Q4

#creating  charts

##bar charts
ggplot(datax, aes(x = trust)) +
    geom_bar(fill = "pink", color = "black") +
    labs(title="Trust in ChatGPT's Intellectual Capability (0 to 10)", x="") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(0, 10)) +  
  theme_minimal()
  
ggplot(datax, aes(x = pre)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title="Stress Level PRE-Intervention (0 to 10)", x="") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(0, 10)) +  
  theme_minimal()
  
ggplot(datax, aes(x = post)) +
    geom_bar(fill = "blue", color = "black") +
    labs(title="Stress Level POST-Intervention (0 to 10)", x="") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(0, 10)) +  
  theme_minimal()

ggplot(datax, aes(x = sat)) +
    geom_bar(fill = "pink", color = "black") +
    labs(title="Satisfaction with Response (0 to 10)", x="") +
   scale_x_continuous(breaks = c(0, 1, 2, 4, 6, 8, 10), limits = c(0, 10)) +  
  theme_minimal()

##box plot for stress reduction in both groups
ggplot(datax, aes(x = group, y = stress_diff)) + 
  geom_boxplot() +
  labs(title = "Distribution of Stress Deduction", x = "", y = "") +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3) +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), 
               color = "red", vjust = -1) + 
  theme_minimal()
  
#tests
##paired t-test for the treatment group
treatment_data <- datax %>%
  filter(group == "Treatment")

t_test_treatment <- t.test(treatment_data$pre, treatment_data$post, paired = TRUE)

print(t_test_treatment)

##paired t-test for the control group
control_data <- datax %>%
  filter(group == "Control")

t_test_control <- t.test(control_data$pre, control_data$post, paired = TRUE)

print(t_test_control)

##two-sample t-test
treatment_diff <- treatment_data$pre - treatment_data$post
control_diff <- control_data$pre - control_data$post

two_sam_t_test <- t.test(treatment_diff, control_diff)
print(two_sam_t_test)

##simple regression analyses for the effect of trust for stress reduction

slm <- lm(stress_diff~trust, data = datax)
summary(slm)

slm2 <- lm(stress_diff~trust, data = treatment_data)
summary(slm2)









