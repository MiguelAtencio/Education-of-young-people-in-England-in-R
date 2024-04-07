tuesdata <- tidytuesdayR::tt_load('2024-01-23')

library(tidyverse)

english_education <- tuesdata$english_education

view(english_education)

english_education %>% select(town11nm, activity_at_age_19_employment_with_earnings_above_0, 
                             activity_at_age_19_employment_with_earnings_above_10_000) %>%
  arrange(desc(activity_at_age_19_employment_with_earnings_above_0))


earnings_above_10k <-english_education %>% select(town11nm, size_flag, job_density_flag, 
                             activity_at_age_19_employment_with_earnings_above_10_000) %>%
  arrange(desc(activity_at_age_19_employment_with_earnings_above_10_000))


earnings_above_10_000_per_size_flag <- english_education %>% select(size_flag, 
  activity_at_age_19_employment_with_earnings_above_10_000) %>% 
  drop_na(size_flag, activity_at_age_19_employment_with_earnings_above_10_000) %>%
  group_by(size_flag) %>%
  summarise(mean = mean(activity_at_age_19_employment_with_earnings_above_10_000),
            sd = ifelse(is.na(sd(activity_at_age_19_employment_with_earnings_above_10_000)), 
                        0,sd(activity_at_age_19_employment_with_earnings_above_10_000)),
            max = max(activity_at_age_19_employment_with_earnings_above_10_000),
            min = min(activity_at_age_19_employment_with_earnings_above_10_000)) %>%
  arrange(desc(mean))


earnings_above_10_000_per_size_flag %>% ggplot(aes(size_flag, mean)) + 
  geom_col(fill = 'white', colour='black') + 
  labs(x = "Size Category", y = "mean proportion",
       title = "Proportion of 19 year olds who have an income above 10,000 per size of town") + 
  theme(plot.title = element_text(hjust = 0.5))


earnings_above_10k_per_job_density <- earnings_above_10k %>% drop_na(job_density_flag, 
  activity_at_age_19_employment_with_earnings_above_10_000) %>%group_by(size_flag, job_density_flag) %>%
  summarise(mean = mean(activity_at_age_19_employment_with_earnings_above_10_000),
            sd = ifelse(is.na(sd(activity_at_age_19_employment_with_earnings_above_10_000)), 
                        0,sd(activity_at_age_19_employment_with_earnings_above_10_000))) %>%
  arrange(desc(mean))

earnings_above_10k_per_job_density %>% ggplot(aes(job_density_flag, mean)) +
  geom_col(fill = 'white', colour='black') + facet_wrap(~size_flag) + 
  labs(x = "Job Density", y = "mean proportion", 
       title = "Proportion of 19 year olds who have an income above 10,000 per job density area")

#goodness of fit test can be done

by_demographics <- english_education %>% select(size_flag, income_flag, level4qual_residents35_64_2011) %>% drop_na(size_flag, income_flag, level4qual_residents35_64_2011)%>% 
  group_by(size_flag, income_flag) %>% filter(level4qual_residents35_64_2011 == "High")

attaintment_by_demographics <- english_education %>% drop_na(size_flag, ttwa_classification, job_density_flag, income_flag, key_stage_4_attainment_school_year_2012_to_2013) %>% 
  select(size_flag, ttwa_classification, job_density_flag, income_flag, key_stage_4_attainment_school_year_2012_to_2013) %>% 
  group_by(size_flag, ttwa_classification, job_density_flag, income_flag) %>% arrange(desc(key_stage_4_attainment_school_year_2012_to_2013))


# Model education_score = log(population_2011) + key_stage_2_attainment_school_year_2007_to_2008 + 
#                         key_stage_4_attainment_school_year_2012_to_2013 + 
#                         activity_at_age_19_full_time_higher_education + activity_at_age_19_employment_with_earnings_above_10_000

#Assumptions of Model

english_education %>% ggplot(aes(population_2011, education_score)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE) + scale_x_continuous(trans="log")


pop_2011 <- lm(education_score ~ log(population_2011), data = english_education)

ggplot(pop_2011, aes(x = .fitted, y = .resid)) + geom_point() + 
  labs(x="Fitted", y="Residuals", title="Fitted vs Residuals") + geom_smooth(method = "lm") #Heteroskedasticity is a problem

ggplot(pop_2011, aes(x = .resid)) + geom_histogram() #Normally distributed

stage_2_attain <- lm(education_score ~ key_stage_2_attainment_school_year_2007_to_2008, data = english_education)

ggplot(stage_2_attain, aes(x = .fitted, y = .resid)) + geom_point() + 
  labs(x="Fitted", y="Residuals", title="Fitted vs Residuals") + geom_smooth(method = "lm") #Heteroskedasticity does not appear to be a bad problem. However, there are some outliers.

ggplot(stage_2_attain, aes(x = .resid)) + geom_histogram() #Normally distributed


cor(english_education %>% select(population_2011, key_stage_2_attainment_school_year_2007_to_2008, 
      key_stage_4_attainment_school_year_2012_to_2013,
      activity_at_age_19_full_time_higher_education, activity_at_age_19_employment_with_earnings_above_10_000),
    use = "pairwise.complete.obs") #There are a couple of variables that are correlated that can cause problems


model <- lm(education_score ~ log(population_2011) + key_stage_2_attainment_school_year_2007_to_2008 + 
                           key_stage_4_attainment_school_year_2012_to_2013 + 
     activity_at_age_19_full_time_higher_education + activity_at_age_19_employment_with_earnings_above_10_000,
   data = english_education) %>% summary()
  



