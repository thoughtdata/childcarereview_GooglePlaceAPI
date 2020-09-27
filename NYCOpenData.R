library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(plyr)
library(raster)

data <- read.csv("NYC_CC_2020.csv", header=TRUE) %>%
  filter(Status == "Active" | Status == "Permitted") %>%
  select(Borough,	ZipCode,Permit.Expiration,Date.Permitted,
Status,Age.Range,Maximum.Capacity,	Day.Care.ID,	Program.Type,	Facility.Type,
Child.Care.Type,Building.Identification.Number,Violation.Rate.Percent, Average.Violation.Rate.Percent,	Total.Educational.Workers,	Average.Total.Educational.Workers,
Public.Health.Hazard.Violation.Rate,	Average.Public.Health.Hazard.Violation.Rate,	Critical.Violation.Rate,	Average.Critical.Violation.Rate	)
data<-data %>% distinct(Legal.Name, Day.Care.ID, .keep_all = TRUE)

#data$Legal.Name[!duplicated(data$Legal.Name)]
#data %>% distinct(data$Day.Care.ID, .keep_all = TRUE)
#data[!duplicated(data$Day.Care.ID), ]
#unique(data)
# Remove rows with NA
data <- na.omit(data)
# Remove empty rows
data <- data %>%  filter(Date.Permitted != "")
data$Date.Permitted <- as.Date(data$Date.Permitted, format = "%m/%d/%Y")
data$Inspection.Date <- as.Date(data$Inspection.Date, format = "%m/%d/%Y")
#data <- data %>%
#  mutate(age_center = difftime(Inspection.Date, Date.Permitted, units = "days"))
# Clean Column names
data <- clean_names(data)
# Data
data <- data %>%  filter(maximum_capacity >0)
#CENTER_UNIT<-data %>% distinct(Building.Identification.Number, .keep_all = TRUE)
qplot(maximum_capacity, data = data, main = "Distribution of Maximum Capacity")
qplot(total_educational_workers, data = data, main = "Distribution of Educational Workers")
#Look at the dataset
glimpse(data)
data$zip_code <- factor(data$zip_code)
sort(data$zip_code)

data %>% group_by(zip_code) %>% tally( name="number.of.center")
summarise(data, mean_maximum_capacity =mean(maximum_capacity))
summarise(data, mean_violation_rate =mean(violation_rate_percent))
summarise(data, mean_workers =mean(total_educational_workers))
summarise(data, mean_health_hazard_violation =mean(public_health_hazard_violation_rate))


#Now remove plyr and try again and you get the grouped summary.
detach(package:plyr)
zipcodeunite<-
  data %>%  group_by(zip_code, borough) %>%
  summarise(total.count=n(),
            sum_capacity = sum(maximum_capacity), 
            mean_maximum_capacity =mean(maximum_capacity),
            mean_workers =mean(total_educational_workers),
            mean_violation_rate =mean(violation_rate_percent),
            mean_health_hazard_violation =mean(public_health_hazard_violation_rate)
            )

zippoverty <- read.csv("~/ACS_16_5YR_B17001_EDDDD.csv", header=TRUE) 
zipcodeunite
zippoverty$zip_code<-as.factor(zippoverty$zip_code)
test<-full_join(zippoverty,zipcodeunite, by = "zip_code" )

test$STratio<-test$mean_maximum_capacity/test$mean_workers

#write.csv(zipcodeunite,"~/zipcodeunite.csv",row.names = TRUE)


test <- na.omit(test)
test <- clean_names(test)
write.csv(test,"~/zipcodeunite.csv",row.names = TRUE)


test$belowpoverty5years=test$below_poverty_level_female_under_5_years+test$below_poverty_level_male_under_5_years+test$below_poverty_leve_male_5_years+test$below_poverty_level_female_5_years
test %>%  group_by(borough) %>%
  summarise(below_poverty = sum(below_poverty_level) )
test %>%  group_by(borough) %>%
  summarise(belowpoverty5years = sum(belowpoverty5years) )


f1 <- total_count ~ estimate_total+ below_poverty_level + sum_capacity + mean_workers 
f2<-  mean_violation_rate ~estimate_total+ below_poverty_level + sum_capacity + mean_workers
f3<-  mean_health_hazard_violation ~estimate_total+ below_poverty_level + sum_capacity + mean_workers
f3<-  STratio ~ estimate_total+belowpoverty5years+ borough

m1 <- lm(f1, data=test)
m2 <- lm(f2, data=test)
m3 <- lm(f3, data=test)

summary(m1)
summary(m2)
summary(m3)



anova(m1)
predVal <- predict(m1)
residVal <- residuals(m1)
ggplot(mapping = aes(x = predVal, y = residVal)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
qqnorm(residVal, pch=19)
qqline(residVal, col="red")



anova(m2)
predVal_2 <- predict(m2)
residVal_2 <- residuals(m2)
ggplot(mapping = aes(x = predVal_2, y = residVal_2)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
qqnorm(residVal_2, pch=19)
qqline(residVal_2, col="red")

anova(m3)
predVal_3 <- predict(m3)
residVal_3 <- residuals(m3)
ggplot(mapping = aes(x = predVal_3, y = residVal_3)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")
qqnorm(residVal_3, pch=19)
qqline(residVal_3, col="red")


# Diagnostic
diag <- ls.diag(m3)
unusual_points <- test %>%
  mutate(h_i = diag$hat,
         stnd_res = diag$std.res,
         stud_res = diag$stud.res,
         cooks = diag$cooks)
# H_i
unusual_points %>%
  filter(h_i > 12/43538  | h_i > 18/43538) %>%
  head(5)
# Standardized residual
unusual_points %>%
  filter(abs(stnd_res) > 2 | abs(stnd_res) > 3) %>%
  head(5)
# Studentized residual
unusual_points %>%
  filter(abs(stud_res) > 2 | abs(stud_res) > 3) %>%
  head(5)
sum (test$sum_capacity)

