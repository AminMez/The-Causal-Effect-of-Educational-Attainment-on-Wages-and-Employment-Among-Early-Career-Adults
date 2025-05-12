install.packages("haven")
install.packages("AER")
install.packages("latex2exp")
library(latex2exp)
library(haven)
library(dplyr)
library(tidyverse)
library(AER)

# Read the .dta file
Census_data <- read_dta("cen_ind_2021_pumf_v2.dta")

Census_select<-Census_data%>%
  select(c(CFInc,hdgree,agegrp,Gender,PR5,pr,Citizen,lfact,Wages,Mob5,LOC_ST_RES))%>%
  mutate(CFInc = as.factor(CFInc))%>%
  mutate(Citizen = as.factor(Citizen))%>%
  mutate(agegrp = as.factor(agegrp))%>%
  mutate(pr=as.factor(pr))%>%
  mutate(gender=as.factor(Gender))


Census_filtered<-Census_select%>%
  filter(agegrp %in% c(9,10,11))%>%
  filter(!hdgree %in% c(88,99))%>%
  filter(CFInc != 88)%>%
  filter(lfact %in%  c(1,2,3,4,5,6,7,8,9,10))%>%
  filter(!Wages %in%  c(99999999,88888888))


Census_coded<-Census_filtered%>%
  mutate(Different_study_location = ifelse(LOC_ST_RES %in% c(2,3),1,0))%>%
  mutate(Moved_in_5_years = ifelse(Mob5 %in% c(5,6),1,0))%>%
  mutate(Moved_for_studying = Different_study_location*Moved_in_5_years)%>%
  mutate(employment_status = ifelse(lfact %in% c(1,2),1,0))


summary_df <- Census_coded %>%
  select(hdgree, Wages, Moved_for_studying, CFInc, Citizen, agegrp, Gender, pr) %>%
  summary()


summary_df
#First version: Already well done, treating hdgree as an ordinal variable

#first stage regression


first_stage <- lm(hdgree ~ Moved_for_studying + CFInc + Citizen + agegrp + Gender + pr,
                  data = Census_coded)
summary(first_stage)

first_stage_no_control <-lm(hdgree ~ Moved_for_studying,
                            data = Census_coded)
summary(first_stage_no_control)




#semi-parametric binnin


Census_coded$edu_hat <- fitted(first_stage)

Census_coded<-Census_coded%>%
  mutate(edu_hat_bins= as.factor(round(edu_hat,0)))


table(Census_coded$hdgree)

second_stage_wages <- lm(log(Wages) ~ edu_hat_bins + CFInc + Citizen + agegrp + Gender + pr, data = Census_coded)
summary(second_stage_wages)

second_stage_wages_without_control <- lm(log(Wages)~ edu_hat_bins,
                                         data=Census_coded)
summary(second_stage_wages_without_control)


#plot the result on wages
wage_values_education<-second_stage_wages$coefficients[2:11]
y<-as.numeric(100*(exp(wage_values_education)-1))
se <- summary(second_stage_wages)$coefficients[2:11, "Std. Error"]


wage_df <- data.frame(
  edu_group = factor(c("Apprenticeship certificate","Program less than 1 year",
                       "1-2 year program", "Program of more than 2 year", "Certificate/Diploma", 
                       "Bachelor", "Above Bachelor", "Degree in Medical Field", "Master","Doctor"),
                     levels = c("Apprenticeship certificate","Program less than 1 year","1-2 year program", "Program of more than 2 year", "Certificate/Diploma", 
                                "Bachelor", "Above Bachelor", "Degree in Medical Field", "Master","Doctor")),
  estimate = y 
)



wage_df$group_index <- as.numeric(wage_df$edu_group)
wage_df$lower <- 100 * (exp(wage_values_education - 1.96 * se) - 1)
wage_df$upper <- 100 * (exp(wage_values_education + 1.96 * se) - 1)

library(ggplot2)
library(latex2exp)

ggplot(wage_df, aes(x = group_index, y = estimate)) +
  geom_point(color="red",size = 3,shape=20) +
  geom_errorbar(aes(ymin = lower, ymax = upper),color = "grey40", linetype = "dashed", width = 0.1)+
  geom_smooth(aes(color = "f(D_hat)"), se = FALSE, size = 1.2) +
  geom_text(aes(label = paste0(round(estimate, 1), "%")), vjust = -0.5, size = 4.5) +
  scale_x_continuous(
    breaks = wage_df$group_index,
    labels = wage_df$edu_group
  ) +
  scale_color_manual(
    values = c("f(D_hat)" = "blue"),
    labels = c("f(D_hat)" = TeX("$f(\\hat{D})$"))
  ) +
  labs(
    title = "Estimated Percentage Wage Increase by Predicted Education Group",
    x = "Predicted Education Group",
    y = "Wage Increase (%) Compared to Reference Group",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 0.5),
    plot.title = element_text(hjust = 0.7, face = "bold"),
    legend.title = element_blank(),
    legend.position = "right"
  )+
  scale_y_continuous(breaks=seq(from=0,to=325,by=25))



#second stage

second_stage_employment <- glm(employment_status ~ edu_hat_bins + CFInc + Citizen + agegrp + Gender + pr,
                               data = Census_coded,
                               family=binomial)
summary(second_stage_employment)

second_stage_employment_without_control<- glm(employment_status ~ edu_hat_bins,
                                              data = Census_coded,
                                              family=binomial)
summary(second_stage_employment_without_control)



employment_log_odds <- second_stage_employment$coefficients[2:11]


employment_se <- summary(second_stage_employment)$coefficients[2:11, "Std. Error"]

logit_to_prob <- function(x) { exp(x) / (1 + exp(x)) }

prob_estimate <- 100 * logit_to_prob(employment_log_odds)
prob_lower <- 100 * logit_to_prob(employment_log_odds - 1.96 * employment_se)
prob_upper <- 100 * logit_to_prob(employment_log_odds + 1.96 * employment_se)


employment_df <- data.frame(
  edu_group = factor(c("Apprenticeship certificate","Program less than 1 year",
                       "1-2 year program", "Program of more than 2 year", "Certificate/Diploma", 
                       "Bachelor", "Above Bachelor", "Degree in Medical Field", "Master","Doctor"),
                     levels = c("Apprenticeship certificate","Program less than 1 year",
                                "1-2 year program", "Program of more than 2 year", "Certificate/Diploma", 
                                "Bachelor", "Above Bachelor", "Degree in Medical Field", "Master","Doctor")),
  estimate = prob_estimate,
  lower = prob_lower,
  upper = prob_upper
)






employment_df$group_index <- as.numeric(employment_df$edu_group)


ggplot(employment_df, aes(x = group_index, y = estimate)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                color = "grey40", linetype = "dashed", width = 0.1) +
  geom_point(color = "red", size = 2.5) +
  geom_smooth(method = "loess", span = 0.5,aes(color = "g(D̂)"), se = FALSE, size = 1.2) +
  geom_text(aes(label = paste0(round(estimate, 1), "%")), 
            vjust = -0.5, size = 4.5) +
  scale_color_manual(
    values = c("g(D̂)" = "green3"),
    labels = c("g(D̂)" = latex2exp::TeX("$g(\\hat{D})$"))
  ) +
  scale_x_continuous(
    breaks = employment_df$group_index,
    labels = employment_df$edu_group
  )+
  labs(
    title = "Estimated Employment Probability by Predicted Education Group",
    x = "Predicted Education Group",
    y = "Odds of Being Employed (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )+
  scale_y_continuous(breaks=seq(from=30,to=100,by=10))+
  geom_hline(yintercept = 55.2, color = "red2", linetype = "dashed")


log(0.2/0.8)


#miscellaneous

hist()

hist(log(Census_coded$Wages), 
     breaks = 60, 
     col = "lightgray", 
     freq = FALSE, 
     main = "Histogram of log(Wages) with Density Curve", 
     xlab = "log(Wages)")

lines(density(log(Census_coded$Wages), na.rm = TRUE), 
      col = "blue", 
      lwd = 2)

hist(Census_coded$hdgree, 
     breaks = 15, 
     col = "lightgray", 
     freq = FALSE, 
     main = "Histogram of hdgree with Density Curve", 
     xlab = "hdgree")


Census_coded$hdgree

#summary table

library(knitr)
library(kableExtra)

numeric_vars <- Census_coded %>%
  select(hdgree, Wages)

numeric_summary <- numeric_vars %>%
  summarise(across(everything(), list(
    Mean = ~mean(.x, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    N = ~sum(!is.na(.x))
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_")

numeric_summary


categorical_vars<-Census_coded%>%
  select(Gender,CFInc,pr,Citizen,employment_status,Moved_for_studying,edu_hat_bins)
get_freq_table <- function(varname) {
  categorical_vars %>%
    count(!!sym(varname)) %>%
    mutate(
      Variable = varname,
      Category = as.character(!!sym(varname)),
      Percent = round(n / sum(n) * 100, 2)
    ) %>%
    rename(Count = n) %>%
    select(Variable, Category, Count, Percent)
}


cat_tables <- lapply(names(categorical_vars), get_freq_table) %>%
  bind_rows()


kable(cat_tables, format = "html", escape = FALSE,
      col.names = c("Variable", "Category", "Count", "Percent"),
      caption = "Frequencies of Categorical Variables") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "left") %>%
  collapse_rows(columns = 1, valign = "top")


kable(cat_tables, format = "html", caption = "Frequencies of Categorical Variables") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))



Census_coded$Gender

