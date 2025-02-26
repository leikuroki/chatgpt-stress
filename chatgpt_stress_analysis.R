#loading libraries
library("dplyr")
library("tidyverse")
library("ggplot2")
library("psych")
library("effsize")

#importing dataset
data <- read.csv("/Users/leilanikuroki/Library/CloudStorage/OneDrive-個人用/Documents/AAA Wolverhampton Psy of Mental Health/Dissertation/Paper/data.csv")

#cleaning and manipulating the data

##removing unnecessary rows 
datax = data[-c(1,2),]

##keeping necessary columns
datax = datax[, c("Finished","Q1","Q2","Q4","Q5","Q6","Q3.Randomized_DO", "UserLanguage")]

##removing rows with missing data, i.e. responses that did not get to the last question
datax <- datax %>% mutate(Q5 = na_if(Q5, ""))  # Convert empty strings to NA
datax <- datax %>% mutate(Q5 = na_if(Q5, "NA"))  # Convert text "NA" to actual NA
datax <- datax %>% filter(!is.na(Q5))

##renaming columns
datax <- datax %>%
  rename(trust = "Q1")

datax <- datax %>%
  rename(pre = "Q2")

datax <- datax %>%
  rename(post = "Q4")

datax <- datax %>%
  rename(sat = "Q5")

datax <- datax %>%
  rename(group = "Q3.Randomized_DO")

##extracting the numbers from mixed text
datax <- datax %>% mutate_at(vars(trust, pre, post, sat), parse_number)

##new column for the post intervention stress level result  
datax$stress_diff <- datax$pre - datax$post

#export the dataset (the github version of the dataset)
write.csv(datax, file = "chatgpt_stress_data.csv")

#exploring the data
##how many are in the treatment group and the control group?
datax %>%
  count(group)

##how many were done in Eng and Jap?
datax %>%
  count(UserLanguage)

#summary of stats
numeric_data <- datax %>% 
  select(where(is.numeric))

summary_table <- describe(numeric_data)

print(summary_table[, c("vars", "mean", "sd", "median", "se")])

##satisfaction in treatment group
treatment_data <- datax %>%
  filter(group == "Treatment")

summary_treatment_data <- describe(treatment_data)

print(summary_treatment_data["sat", c("vars", "mean", "sd", "median", "se")])

##confidence intervals

###pre
pre_t <- t.test(numeric_data$pre)
con_int_pre <- pre_t$conf.int
print(con_int_pre)

###post
post_t <- t.test(numeric_data$post)
con_int_post <- post_t$conf.int
print(con_int_post)

###trust
trust_t <- t.test(numeric_data$trust)
con_int_trust <- trust_t$conf.int
print(con_int_trust)

###satisfaction
sat_t <- t.test(numeric_data$sat)
con_int_sat <- sat_t$conf.int
print(con_int_sat)

###satisfaction (only the treatment group)
sat_t_treatment <-t.test(treatment_data$sat)

con_int_sat_treatment <- sat_t_treatment$conf.int

print(con_int_sat_treatment)

#creating  charts

##bar charts
ggplot(datax, aes(x = trust)) +
    geom_bar(fill = "pink", color = "black") +
    labs(title="", x="Trust in ChatGPT's Intellectual Capability (0 to 10)") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +  
  theme_minimal()

ggplot(datax, aes(x = pre)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title="", x="Stress Level PRE-Intervention (0 to 10)") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(-1,10.5)) +  
  theme_minimal()
  
ggplot(datax, aes(x = post)) +
    geom_bar(fill = "blue", color = "black") +
    labs(title="", x="Stress Level POST-Intervention (0 to 10)") +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10), limits = c(-1,10.5)) +  
  theme_minimal()

ggplot(datax, aes(x = sat)) +
    geom_bar(fill = "pink", color = "black") +
    labs(title="", x="Satisfaction with Response (0 to 10)") +
   scale_x_continuous(breaks = c(0, 1, 2, 4, 6, 8, 10)) +  
  theme_minimal()

##box plot for stress reduction in both groups
ggplot(datax, aes(x = group, y = stress_diff)) + 
  geom_boxplot() +
  labs(title = "", x = "", y = "Stress Reduction") +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 3) +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), 
               color = "red", vjust = -1) + 
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8), limits = c(-2,8)) +  
  theme_minimal()

#tests
##paired t-test for the treatment group
treatment_data <- datax %>%
  filter(group == "Treatment")

t_test_treatment <- t.test(treatment_data$pre, treatment_data$post, paired = TRUE)

print(t_test_treatment)

sd(treatment_data$stress_diff)

cohen.d(treatment_data$pre, treatment_data$post, paired = TRUE)

##paired t-test for the control group
control_data <- datax %>%
  filter(group == "Control")

t_test_control <- t.test(control_data$pre, control_data$post, paired = TRUE)

print(t_test_control)

sd(control_data$stress_diff)

cohen.d(control_data$pre, control_data$post, paired = TRUE)

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

#£scatterplot for trust and stress reduction
ggplot(treatment_data, aes(x = trust, y = stress_diff)) + 
  geom_point(color = "blue", size = 3) +  # Scatter plot points
  labs(title = "Basic Scatterplot", x = "X-axis", y = "Y-axis") +
  theme_minimal()

#Q3 for both groups
quantile(treatment_diff, probs = 0.75)
quantile(control_diff, probs = 0.75)




