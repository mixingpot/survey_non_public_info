
# Notes -------------------------------------------------------------------
# (c) 2019, MixingPot.ai

# Libraries + Options -----------------------------------------------------
require(pacman)
p_load(tidyverse, sjPlot, stargazer, survey)

# Raw Data ----------------------------------------------------------------
survey <- read_csv("survey1_clean.csv")
saveRDS(survey, "survey.RDS")


# Feature Engineering -----------------------------------------------------
survey.summary <- data.frame(
  Lived_in_Atlanta    = ifelse(!is.na(survey$Lived_in_Atlanta) & survey$Lived_in_Atlanta     == "Atlanta", 1, 0),
  Lived_in_Baltimore  = ifelse(!is.na(survey$Lived_in_Baltimore) & survey$Lived_in_Baltimore == "Baltimore", 1, 0),
  Lived_in_SF         = ifelse(!is.na(survey$Lived_in_SF) & survey$Lived_in_SF               == "SF", 1, 0),
  Lived_in_Ivy        = ifelse(!is.na(survey$Lived_in_Ivy) & grepl("Any", survey$Lived_in_Ivy), 1, 0),
  Q2_indirect         = ifelse(survey$q2 == "Yes - but only in media (TV, books, magazines, etc.) or when learning or discussing history", 1, 0),
  Q2_direct           = ifelse(grepl("Yes (", survey$q2, fixed=T), 1, 0),
  Q3_both             = ifelse((!is.na(survey$q3)) & survey$q3 == "Yes - both", 1, 0),
  Q4_biz              = ifelse((!is.na(survey$q4)) & survey$q4 == "Business", 1, 0),
  Q4_gov              = ifelse((!is.na(survey$q4)) & survey$q4 == "Government", 1, 0),
  Q4_edu              = ifelse((!is.na(survey$q4)) & survey$q4 == "Higher Education", 1, 0),
  Q4_NA               = ifelse((!is.na(survey$q4)) & survey$q4 == "N/A or I don't know", 1, 0),
  Current_Region_East_North_Central = ifelse(!is.na(survey$Region) & survey$Region == "East North Central",1,0),
  Current_Region_East_South_Central = ifelse(!is.na(survey$Region) & survey$Region == "East South Central",1,0),
  Current_Region_Middel_Atlantic = ifelse(!is.na(survey$Region) & survey$Region    == "Middle Atlantic",1,0),
  Current_Region_Mountain = ifelse(!is.na(survey$Region) & survey$Region           == "Mountain",1,0),
  Current_Region_New_England = ifelse(!is.na(survey$Region) & survey$Region        == "New England",1,0),
  Current_Region_Pacific = ifelse(!is.na(survey$Region) & survey$Region            == "Pacific",1,0),
  Current_Region_South_Atlantic = ifelse(!is.na(survey$Region) & survey$Region     == "South Atlantic",1,0),
  Current_Region_West_North_Central = ifelse(!is.na(survey$Region) & survey$Region == "West North Central",1,0),
  Current_Region_West_South = ifelse(!is.na(survey$Region) & survey$Region         == "West South Central",1,0),
  Gender_Female = ifelse(!survey$Gender=="Male",1 , 0),
  Gender_Male = ifelse(survey$Gender=="Male",1 , 0)
)

sum(is.na(survey.summary)) # should be 0

stargazer(survey.summary, 'html')

saveRDS(survey.summary, "survey.summary.RDS") # write out (for .Rmd)


# EDA ---------------------------------------------------------------------

# Overview
glimpse(survey)

# Features
colnames(survey)

# Age
prop.table(table(survey$Age))

# Current Location
prop.table(table(survey$Region))

# Gender
prop.table(table(survey$Gender))

# Income
prop.table(table(survey$`Household Income`))

# Location (region)
prop.table(table(survey$Region))

# Summary Stats
stargazer(survey.summary, type = 'latex')

# Q1
cat("4.7% had lived in Atlanta:")
sum(!is.na(survey$Lived_in_Atlanta))/nrow(survey)

cat("3.2% had lived in Baltimore:")
sum(!is.na(survey$Lived_in_Baltimore))/nrow(survey)

cat("6.3% had lived in San Francisco:")
sum(!is.na(survey$Lived_in_SF))/nrow(survey)

cat("11.1% had lived in a city home to an Ivy League university:")
sum(!is.na(survey$Lived_in_Ivy))/nrow(survey)

# Q2
table(survey$q2)
prop.table(table(survey$q2))

# Q3
table(survey$q3)
prop.table(table(survey$q3))

# Q4
table(survey$q4)
prop.table(table(survey$q4))

# Sample Weighting --------------------------------------------------------
# Unweighted Survey (raw)
data.svy.unweighted <- svydesign(ids=~1, data=survey.summary)

# U.S., 18+ Population Age (marginal probabilities)
gender.dist <- data.frame(Gender = c("Male", "Female"),
                          Freq = nrow(data) * c(0.492, 0.508))

# Raking of replicate weights
data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~Gender),
                      population.margins = list(gender.dist))

data.svy.rake.trim  <- trimWeights(data.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)

svymean(survey.summary, data.svy.rake.trim)

saveRDS(survey.summary, "survey.summary.RDS") # For .Rmd latex table
