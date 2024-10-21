Data <- read.csv("/Users/moontasirmahatbjalal/Desktop/DS Lab Task/ds_salaries.csv", header = TRUE, sep=",")
Data
str(Data)

num_instances <- nrow(Data)
num_attributes <- ncol(Data)
cat("Number of instances (rows):", num_instances, "\n") 
cat("Number of Attribute (Columns):", num_attributes, "\n") 
str(Data)


Data$company_size <- as.factor(Data$company_size)
actualData <- Data
str(actualData)



s_data = table(actualData$experience_level, actualData$employment_type)
s_data
chisq_Test1 <- chisq.test(s_data)
chisq_Test1

s_data = table(actualData$experience_level, actualData$job_title)
s_data
chisq_Test2 <- chisq.test(s_data)
chisq_Test2

s_data = table(actualData$experience_level, actualData$salary_currency)
s_data
chisq_Test3 <- chisq.test(s_data)
chisq_Test3

s_data = table(actualData$experience_level, actualData$employee_residence)
s_data
chisq_Test4 <- chisq.test(s_data)
chisq_Test4

s_data = table(actualData$experience_level, actualData$company_location)
s_data
chisq_Test5 <- chisq.test(s_data)
chisq_Test5

s_data = table(actualData$job_title, actualData$employee_residence)
s_data
chisq_Test6 <- chisq.test(s_data)
chisq_Test6

s_data = table(actualData$job_title, actualData$company_location)
s_data
chisq_Test7 <- chisq.test(s_data)
chisq_Test7




pearson_Test1<- cor(actualData$work_year, actualData$salary, method = "pearson")
cat("Person correlation coefficient is:", pearson_Test1)

library(ggplot2)
ggplot(actualData, aes(x = work_year, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of work_year vs salary",
       x = "work_year",
       y = "salary") +
  geom_text(x = 1, y = 8, label = paste("Correlation coefficient:", round(pearson_Test1, 2)))



pearson_Test2<- cor(actualData$salary, actualData$salary_in_usd, method = "pearson")
cat("Person correlation coefficient is:", pearson_Test2)

ggplot(actualData, aes(x = salary, y = salary_in_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of salary vs salary_in_usd",
       x = "salary",
       y = "salary_in_usd") +
  geom_text(x = 1, y = 8, label = paste("Correlation coefficient:", round(pearson_Test2, 2)))



pearson_Test3<- cor(actualData$salary, actualData$remote_ratio, method = "pearson")
cat("Person correlation coefficient is:", pearson_Test3)

ggplot(actualData, aes(x = salary, y = remote_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of salary vs remote_ratio",
       x = "salary",
       y = "remote_ratio") +
  geom_text(x = 1, y = 8, label = paste("Correlation coefficient:", round(pearson_Test3, 2)))



pearson_Test4<- cor(actualData$work_year, actualData$salary_in_usd, method = "pearson")
cat("Person correlation coefficient is:", pearson_Test3)

ggplot(actualData, aes(x = work_year, y = salary_in_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of salary vs salary_in_usd",
       x = "work_year",
       y = "salary_in_usd") +
  geom_text(x = 1, y = 8, label = paste("Correlation coefficient:", round(pearson_Test4, 2)))




anova1<- aov(actualData$work_year~actualData$experience_level)
summary(anova1)

anova2<- aov(actualData$work_year~actualData$employment_type)
summary(anova2)

anova3<- aov(actualData$work_year~actualData$job_title)
summary(anova3)

anova4<- aov(actualData$work_year~actualData$salary_currency)
summary(anova4)

anova5<- aov(actualData$work_year~actualData$employee_residence)
summary(anova5)

anova6<- aov(actualData$work_year~actualData$company_location)
summary(anova6)

anova7<- aov(actualData$work_year~actualData$company_size)
summary(anova7)

