traffic_clean <- readRDS("traffic-cleaned.rds")

# install.packages("ggplot2")
# install.packages("remotes")
# library(remotes)
# install_github("rkabacoff/qacr")

library(qacr)
#overall summary
df_summary(traffic_clean)

#univariate categorical variable
tab(traffic_clean, subject_race, plot =TRUE)

#univariate quantitative
dstats(traffic_clean, subject_age)

#bivariate categorical vs categorical
crosstab(traffic_clean, subject_race, intervention_duration,
         type = "rowpercent")

#bivariate categorical vs quantitative
dstats(traffic_clean, subject_age, intervention_date)

#bivariate quantitative
corplot(mtcars)