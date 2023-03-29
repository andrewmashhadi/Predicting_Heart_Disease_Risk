
# Set Environment Variables For Input/Output Paths

## define input path
PATH_MAIN_DATA   <- Sys.getenv("STATS_412_PROJECT_DATA")

## define output path
PATH_IMAGE_DIR  <- Sys.getenv("STATS_412_PROJECT_IMAGE_DIR")


# Load the necessary libraries and read the dataset

 
library(tidyverse)

rawdf <- read.csv(paste0(PATH_MAIN_DATA,"/framingham.csv"))

df <- rawdf %>%
  drop_na()

# proportion missing
1 - nrow(df) / nrow(rawdf)
 


# Summarize the features in the Framingham data

 
summary(df)
 


# Exploratory Data Analysis

## Response

 
df$TenYearCHD[df$TenYearCHD == 0] <- "Not At Risk"
df$TenYearCHD[df$TenYearCHD == 1] <- "At Risk"
df$TenYearCHD <- factor(df$TenYearCHD)

table(df$TenYearCHD)

df %>%
  group_by(TenYearCHD) %>%
  summarise(Proportion = n() / nrow(df)) %>%
  ggplot(aes(x = TenYearCHD, y = Proportion, fill = TenYearCHD)) +
  geom_col() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Sample Proportion of Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_chd.png"))
 


## Education Level

 
df$education[df$education == 1] <- "No High School"
df$education[df$education == 2] <- "High School"
df$education[df$education == 3] <- "Bachelors"
df$education[df$education == 4] <- "Graduate School"

education_levels <- c("No High School", "High School", "Bachelors", "Graduate School")
df$education <- factor(df$education, levels = education_levels)

table(df$education)

ggplot(df, aes(x = education, fill = education)) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Education Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_education.png"))
 


## Current Smoker

 
df$currentSmoker[df$currentSmoker == 0] <- "Non-Smoker"
df$currentSmoker[df$currentSmoker == 1] <- "Smoker"
df$currentSmoker <- factor(df$currentSmoker)

table(df$currentSmoker)
ggplot(df, aes(x = currentSmoker, fill = currentSmoker)) +
  geom_bar() +
  ylim(0, 2000) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Current Smoker Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_smoker_status.png"))
 


## Gender Frequency

 
df$male[df$male == 1] <- "Male"
df$male[df$male == 0] <- "Female"

table(df$male)
ggplot(df, aes(x = factor(male), fill = factor(male))) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Gender Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_sex.png"))
 


## Cigs Per Day

 
df$cigsPerDay <- cut(df$cigsPerDay, c(-1, 5, 10, 15, 20, 90))
levels(df$cigsPerDay) <- c("None", "Light", "Occasional", "Many", "Addiction")

table(df$cigsPerDay)
ggplot(df, aes(x = cigsPerDay, fill = cigsPerDay)) + 
  geom_bar() +
  theme_minimal() +  
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Cigarettes Per Day")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_cigs_per_day.png"))
 


## BP Meds

 
df$BPMeds[df$BPMeds == 0] <- "No"
df$BPMeds[df$BPMeds == 1] <- "Yes"
df$BPMeds <- factor(df$BPMeds)

table(df$BPMeds)

ggplot(df, aes(x = BPMeds, fill = BPMeds)) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Blood Pressure Medication Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_bp_meds.png"))
 


## Prevalent Strokes

 
df$prevalentStroke[df$prevalentStroke == 0] <- "No"
df$prevalentStroke[df$prevalentStroke == 1] <- "Yes"
df$prevalentStroke <- factor(df$prevalentStroke)

table(df$prevalentStroke)

ggplot(df, aes(x = prevalentStroke, fill = prevalentStroke)) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("History of Strokes Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_strokes.png"))
 


## Hypertension

 
df$prevalentHyp[df$prevalentHyp == 0] <- "No"
df$prevalentHyp[df$prevalentHyp == 1] <- "Yes"
df$prevalentHyp <- factor(df$prevalentHyp)

table(df$prevalentHyp)

ggplot(df, aes(x = prevalentHyp, fill = prevalentHyp)) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Hypertension Status Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_hypertension.png"))
 


## DiaBP

 
df$diaBP <- as.numeric(df$diaBP)
ggplot(df, aes(x = diaBP, fill = "#F8766D")) +
  geom_histogram(bins = 15) + 
  theme_minimal() +
  theme(legend.position = "None") + 
  xlab("Diastolic Blood Pressure") +
  ggtitle("Histogram of Diastolic Blood Pressure")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_dbp.png"))

df$diaBP <- cut(df$diaBP, c(0, 80, 89, max(df$diaBP)))
levels(df$diaBP) <- c('Average', 'Above Average', 'High')
table(df$diaBP)

ggplot(df, aes(x = diaBP, fill = diaBP)) + 
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Diastolic Blood Pressure Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_dbp.png"))
 


## Glucose

 
ggplot(df, aes(x = glucose, fill = "#F8766D")) + 
  geom_histogram(bins = 20) +
  theme_minimal() +
  theme(legend.position = "None") +
  ggtitle("Histogram of Glucose")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_glucose.png"))

df$glucose <- cut(df$glucose, c(0, 98, max(df$glucose)))
levels(df$glucose) <- c('Normal', 'Diabetic')

ggplot(df, aes(x = glucose, fill = glucose)) + 
  geom_bar() +
  ylim(0, 3500) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Glucose Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_glucose.png"))
 


## Age

 
ggplot(df, aes(x = age, fill = "#F8766D")) + 
  geom_histogram(bins = 8) +
  theme_minimal() + 
  theme(legend.position = "None") +
  ggtitle("Histogram of Age")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_age.png"))

df$age <- cut(df$age, c(30, 40, 50, 60, 70))
levels(df$age) <- c("30's", "40's", "50's", "Over 60")

table(df$age)

df %>%
  ggplot(aes(x = age, fill = age)) +
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Age Group Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_age.png"))
 


## SysBP

 
df$sysBP <- as.numeric(df$sysBP)
ggplot(df, aes(x = sysBP, fill = "#F8766D")) +
  geom_histogram(bins = 15) + 
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Systolic Blood Pressure") +
  ggtitle("Histogram of Systolic Blood Pressure")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_sbp.png"))

df$sysBP <- cut(df$sysBP, c(0, 120, 139, max(df$sysBP)))
levels(df$sysBP) <- c('Average', 'Above Average', 'High')
table(df$sysBP)

ggplot(df, aes(x = sysBP, fill = sysBP)) + 
  geom_bar() +
  theme_minimal() +  
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Systolic Blood Pressure Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_sbp.png"))
 


## Heart Rate

 
ggplot(df, aes(x = heartRate, fill = "#F8766D")) + 
  geom_histogram(bins = 10) + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") + 
  ggtitle("Histogram of Heart Rate") +
  xlab("Heart Rate")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_heart_rate.png"))
 


## BMI

 
ggplot(df, aes(x = BMI, fill = "#F8766D")) + 
  geom_histogram(bins = 15) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  ggtitle("Histogram of BMI")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_bmi.png"))

df$BMI <- cut(df$BMI, c(0, 18.5, 25, 30, max(df$BMI)))
levels(df$BMI) <- c('Under Weight', 'Healthy Weight', 'Over Weight', 'Obese')

ggplot(df, aes(x = BMI, fill = BMI)) + 
  geom_bar() +
  theme_minimal() +  
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("BMI Frequencies")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_bmi.png"))
 


## Diabetes Status

 
df$diabetes[df$diabetes == 0] <- "No Diabetes"
df$diabetes[df$diabetes == 1] <- "Diabetes"
df$diabetes <- factor(df$diabetes)

ggplot(df, aes(x = diabetes, fill = diabetes)) + 
  geom_bar() +
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("") + 
  ggtitle("Diabetes Frequency")

ggsave(paste0(PATH_IMAGE_DIR,"/bar_diabetes.png"))
 


## Cholesterol
 
ggplot(df, aes(x = totChol, fill = "#F8766D")) + 
  geom_histogram(bins = 20) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.position = "None") + 
  xlab("Total Cholesterol") + 
  ggtitle("Histogram of Total Cholesterol")

ggsave(paste0(PATH_IMAGE_DIR,"/hist_cholesterol.png"))
 


# Two Way Relationships

## Numeric Variables Boxplots

 
df2 <- read.csv(paste0(PATH_MAIN_DATA, "/framingham.csv")) %>%
  drop_na()

df2$TenYearCHD[df2$TenYearCHD == 0] <- "Not At Risk"
df2$TenYearCHD[df2$TenYearCHD == 1] <- "At Risk"
df2$TenYearCHD <- factor(df2$TenYearCHD)

ggplot(df2, aes(x = TenYearCHD, y = diaBP, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Diastolic Blood Pressure") +
  ggtitle("Diastolic Blood Pressure vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_dbp.png"))

ggplot(df2, aes(x = TenYearCHD, y = totChol, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Cholesteral") +
  ggtitle("Cholesteral Levels vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_cholesterol.png"))

ggplot(df2, aes(x = TenYearCHD, y = age, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Age") +
  ggtitle("Age vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_age.png"))

ggplot(df2, aes(x = TenYearCHD, y = glucose, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Glucose") +
  ggtitle("Glucose vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_glucose.png"))

ggplot(df2, aes(x = TenYearCHD, y = sysBP, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Systolic Blood Pressure") +
  ggtitle("Systolic Blood Pressure vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_sbp.png"))

ggplot(df2, aes(x = TenYearCHD, y = cigsPerDay, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Cigarettes Per Day") +
  ggtitle("Cigarettes Per Day vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_cigs_per_day.png"))

ggplot(df2, aes(x = TenYearCHD, y = BMI, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("BMI") +
  ggtitle("BMI vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_bmi.png"))

ggplot(df2, aes(x = TenYearCHD, y = heartRate, fill = TenYearCHD)) +
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Heart Rate") +
  ggtitle("Heart Rate vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_heart_rate.png"))
 


## Conditional Distributions

 
ggplot(df2, aes(x = cigsPerDay, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Cigarettes Per Day") +
  ggtitle("Cigarettes Per Day Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_cigs_per_day.png"))

ggplot(df2, aes(x = glucose, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Glucose") +
  ggtitle("Glucose Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_glucose.png"))

ggplot(df2, aes(x = totChol, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Cholesterol") +
  ggtitle("Cholesterol Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_cholesterol.png"))

ggplot(df2, aes(x = age, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Age") +
  ggtitle("Age Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_age.png"))

ggplot(df2, aes(x = sysBP, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Systolic Blood Pressure") +
  ggtitle("Systolic Blood Pressure Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_sbp.png"))

ggplot(df2, aes(x = BMI, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("BMI") +
  ggtitle("BMI Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_bmi.png"))

ggplot(df2, aes(x = diaBP, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Diastolic Blood Pressure") +
  ggtitle("Diastolic Blood Pressure Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_dbp.png"))

ggplot(df2, aes(x = heartRate, fill = "#F8766D")) +
  geom_density() +
  facet_wrap(~ TenYearCHD, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("Heart Rate") +
  ggtitle("Heart Rate Conditional Distribution")

ggsave(paste0(PATH_IMAGE_DIR,"/dens_heart_rate.png"))

df2 %>%
  group_by(TenYearCHD) %>%
  summarise(age_SD = sd(age),
            cigsPerDay_SD = sd(cigsPerDay),
            totChol_SD = sd(totChol),
            sysBP_SD = sd(sysBP),
            diaBP_SD = sd(diaBP),
            BMI_SD = sd(BMI),
            heartRate_SD = sd(heartRate),
            glucose_SD = sd(glucose))
 


## Interactions between Strictly Numeric Variables

 
wilcox.test(sysBP*cigsPerDay ~ TenYearCHD, df2)

ggplot(df2, aes(x = TenYearCHD, y = cigsPerDay * sysBP, fill = TenYearCHD)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Systolic BP x Cigarettes Per Day") +
  ggtitle("(Systolic BP x Cigarettes Per Day) vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_int_sbp_cigs_per_day.png"))

wilcox.test(age * cigsPerDay ~ TenYearCHD, df2)

ggplot(df2, aes(x = TenYearCHD, y = cigsPerDay * age, fill = TenYearCHD)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Age x Cigarettes Per Day") +
  ggtitle("(Age x Cigarettes Per Day) vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_int_age_cigs_per_day.png"))

wilcox.test(sysBP * glucose ~ TenYearCHD, df2)

ggplot(df2, aes(x = TenYearCHD, y = glucose * sysBP, fill = TenYearCHD)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("") +
  ylab("Systolic BP x Glucose Level") +
  ggtitle("(Systolic BP x Glucose Level) vs Heart Disease Risk")

ggsave(paste0(PATH_IMAGE_DIR,"/box_int_sbp_glucose.png"))
 


## Diastolic Blood Pressure Level

  
table(df$TenYearCHD, df$diaBP)

df %>%
  group_by(TenYearCHD, diaBP) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = diaBP, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease Risk By Diastolic Blood Pressure Level")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_dbp.png"))
 


## Diabetes Status

  
table(df$TenYearCHD, df$diabetes)

df %>%
  group_by(TenYearCHD, diabetes) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = diabetes, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) +
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease Risk By Diabetes Status")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_diabetes.png"))
 


## Age

  
table(df$TenYearCHD, df$age)

df %>%
  group_by(TenYearCHD, age) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = age, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease Risk By Age")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_age.png"))
 


## Education Level

  
table(df$TenYearCHD, df$education)

df %>%
  group_by(TenYearCHD, education) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = education, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Education")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_education.png"))
 


## Gender

  
table(df$TenYearCHD, df$male)

df %>%
  group_by(TenYearCHD, male) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = male, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Gender")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_sex.png"))
 


## Smoker Status

  
table(df$TenYearCHD, df$currentSmoker)

df %>%
  group_by(TenYearCHD, currentSmoker) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = currentSmoker, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Smoker Status")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_smoker_status.png"))
 


## Cigs Per Day

table(df$TenYearCHD, df$cigsPerDay)

df %>%
  group_by(TenYearCHD, cigsPerDay) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = cigsPerDay, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Smoker Type")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_cigs_per_day.png"))
 


## Glucose Level

table(df$TenYearCHD, df$glucose)

df %>%
  group_by(TenYearCHD, glucose) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = glucose, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Glucose Levels")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_glucose.png"))
 


## BP Meds

  
table(df$TenYearCHD, df$BPMeds)

df %>%
  group_by(TenYearCHD, BPMeds) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = BPMeds, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Blood Pressure Medication Status")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_bp_meds.png"))
 


## Systolic Blood Pressure Level

  
table(df$TenYearCHD, df$sysBP)

df %>%
  group_by(TenYearCHD, sysBP) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = sysBP, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease Risk By Systolic Blood Pressure Level")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_sbp.png"))
 


## BMI

  
table(df$TenYearCHD, df$BMI)

df %>%
  group_by(TenYearCHD, BMI) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = BMI, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Body Mass Index")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_bmi.png"))
 


## Hypertension
 
table(df$TenYearCHD, df$prevalentHyp)

df %>%
  group_by(TenYearCHD, prevalentHyp) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = prevalentHyp, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Hypertension")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_hypertension.png"))
 


## Prevalent Stroke
 
table(df$TenYearCHD, df$prevalentStroke)

df %>%
  group_by(TenYearCHD, prevalentStroke) %>%
  summarise(proportion = n() / nrow(df)) %>%
  ggplot(aes(x = prevalentStroke, y = proportion, fill = TenYearCHD)) +
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14)) + 
  theme(legend.title = element_blank()) +
  xlab("") + 
  ylab("") +
  ggtitle("Proportion of Heart Disease By Stroke")

ggsave(paste0(PATH_IMAGE_DIR,"/two_way_stroke.png"))
 


## Categorical Variables Chisq tests 

 
chisq.test(df$TenYearCHD, df$glucose)
chisq.test(df$TenYearCHD, df$currentSmoker)
chisq.test(df$TenYearCHD, df$education)
chisq.test(df$TenYearCHD, df$cigsPerDay)
chisq.test(df$TenYearCHD, df$prevalentHyp)
chisq.test(df$TenYearCHD, df$diaBP)
chisq.test(df$TenYearCHD, df$male)
chisq.test(df$TenYearCHD, df$BMI)
chisq.test(df$TenYearCHD, df$sysBP)
chisq.test(df$TenYearCHD, df$BPMeds)
chisq.test(df$TenYearCHD, df$prevalentStroke)
chisq.test(df$TenYearCHD, df$diabetes)
chisq.test(df$TenYearCHD, df$age)
 


# VIF

 
glmodel1 <- glm(TenYearCHD ~ ., data = df2, family = binomial)

data.frame(DAAG::vif(glmodel1)) %>%
  rownames_to_column(var = "var") %>%
  ggplot(aes(x = DAAG..vif.glmodel1., y = fct_reorder(var, DAAG..vif.glmodel1.), 
             fill = fct_reorder(var, DAAG..vif.glmodel1.))) +
  geom_col() +
  scale_y_discrete(labels = c("Prevalent Stroke", "Education", "Total Cholesterol", "Heart Rate", "Blood Pressure Medication", "Body Mass Index", "Gender", "Age", "Diabetes", "Glucose", "Prevalent Hypertension", "Smoker Status", "Cigarettes Per Day", "Diastolic Blood Pressure", "Systolic Blood Pressure")) +
  theme_minimal() +
  theme(legend.position = "None") +
  xlab("VIF") +
  ylab("Variables") +
  ggtitle("VIF Values")

ggsave(paste0(PATH_IMAGE_DIR,"/vif.png"))
 


# Correlation Plot

 
corrplot::corrplot(
  cor(df2 %>% select(diaBP, BMI, cigsPerDay, age, totChol, sysBP, glucose,heartRate) %>%
        rename(`Diastolic Blood Pressure` = diaBP,
               `Body Mass Index` = BMI,
               `Cigarettes Per Day` = cigsPerDay,
               `Age` = age,
               `Total Cholesterol` = totChol,
               `Systolic Blood Pressure` = sysBP,
               `Glucose` = glucose,
               `Heart Rate` = heartRate)),
  type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

ggsave(paste0(PATH_IMAGE_DIR,"/cor.png"))
 







