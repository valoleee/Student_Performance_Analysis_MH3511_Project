# Appendix 1: Code for installing packages, loading library and cleaning of dataset
# install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")

# loading libraries
library(dplyr)
library(ggplot2)
library(psych)

# cleaning of dataset
ds <- read.csv("student_performance_dataset.csv")
ds <- na.omit(ds)
ds <- ds %>% distinct(Student_ID, .keep_all = T)

# Appendix 2: Code for plotting bar plot of Gender
ggplot(ds, aes(x = Gender, fill = Gender)) +
  geom_bar(col = "black", width = 0.4) +
  scale_fill_manual(values = c("Male" = "#4393C3", "Female" = "#D6604D")) +
  labs(title = "Barplot of Gender", x = "Gender", y = "Count") +
  ylim(0, 300) +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 3: Code for histogram and boxplot of Study_Hours_per_Week
ggplot(ds, aes(x = Study_Hours_per_Week)) +
  geom_histogram(fill = "#D6604D", col = "black", bins = 18) + 
  scale_fill_manual(values = c("Male" = "#4393C3", "Female" = "#D6604D")) +
  labs(title = "Histogram of Study Hours Per Week", 
       x = "Study Hours Per Week", 
       y = "Count") +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

boxplot(ds$Study_Hours_per_Week,
        main = "Boxplot of Study Hours Per Week",
        col = "#D6604D")

# Appendix 4: Code for histogram and boxplot of Attendance_Rate
ggplot(ds, aes(x = Attendance_Rate)) +
  geom_histogram(fill = "#4393C3", col = "black", bins = 18) + 
  labs(title = "Histogram of Attendance Rate", 
       x = "Attendance Rate", 
       y = "Count") +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

boxplot(ds$Attendance_Rate,
        main = "Boxplot of Attendance Rate",
        col = "#4393C3")

# Appendix 5: Code for histogram and boxplot of Past_Exam_Scores
ggplot(ds, aes(x = Past_Exam_Scores)) +
  geom_histogram(fill = "#FFC125", col = "black", bins = 15) + 
  labs(title = "Histogram of Past Exam Scores", 
       x = "Past Exam Scores", 
       y = "Count") +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

boxplot(ds$Past_Exam_Scores,
        main = "Boxplot of Past Exam Scores",
        col = "#FFC125",
        ylab = "Past Exam Scores")

# Appendix 6: Code for barplot of Parental_Education_Level
ds$Parental_Education_Short <- recode(ds$Parental_Education_Level,
                                      "High School" = "HS",
                                      "Bachelors" = "Bach",
                                      "Masters" = "Mast",
                                      "PhD" = "PhD")

ggplot(ds, aes(x = Parental_Education_Short, fill = Parental_Education_Short)) +
  geom_bar(col = "black", width = 0.4) + 
  scale_fill_manual(values = c("HS" = "#4393C3", 
                               "Bach" = "#D6604D",
                               "Mast" = "#FFC125",
                               "PhD" = "#8FBC8F")) +
  labs(title = "Barplot of Parental Education Level", 
       x = "Education Level", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 7: Code for barplot of Internet_Access_at_Home
ggplot(ds, aes(x = Internet_Access_at_Home, fill = Internet_Access_at_Home)) +
  geom_bar(col = "black", width = 0.4) + 
  scale_fill_manual(values = c("Yes" = "#4393C3", 
                               "No" = "#D6604D")) +
  labs(title = "Barplot of Internet Access at Home", 
       x = "Internet Access at Home", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 8: Code for barplot of Extracurricular_Activities
ggplot(ds, aes(x = Extracurricular_Activities, fill = Extracurricular_Activities)) +
  geom_bar(col = "black", width = 0.4) + 
  scale_fill_manual(values = c("Yes" = "#4393C3", "No" = "#D6604D")) +
  labs(title = "Barplot of Extracurricular Activities", 
       x = "Participation Extracurricular Activities", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))


# Appendix 9: Code for histogram and boxplot of Final_Exam_Score 
ggplot(ds, aes(x = Final_Exam_Score)) +
  geom_histogram(fill = "#8FBC8F", col = "black", bins = 15) + 
  labs(title = "Histogram of Final Exam Score", 
       x = "Final Exam Score", 
       y = "Count") +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

boxplot(ds$Final_Exam_Score,
        main = "Boxplot of Final Exam Score",
        col = "#8FBC8F")

# Appendix 10: Code for barplot of Pass_Fail 
ggplot(ds, aes(x = Pass_Fail, fill = Pass_Fail)) +
  geom_bar(col = "black", width = 0.4) + 
  scale_fill_manual(values = c("Pass" = "#4393C3", "Fail" = "#D6604D")) +
  labs(title = "Barplot of Students' Status", 
       x = "Status", 
       y = "Count") +
  theme_minimal() +
  ylim(0, 400) +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 11: Code for Shapiro-Wilk Normality Test
ds$log_Study_Hours_per_Week <- log(ds$Study_Hours_per_Week) 
ds$log_Attendance_Rate <- log(ds$Attendance_Rate)
ds$log_Past_Exam_Scores <- log(ds$Past_Exam_Scores)
ds$log_Final_Exam_Score <- log(ds$Final_Exam_Score)

shapiro.test(ds$log_Attendance_Rate)
shapiro.test(ds$log_Study_Hours_per_Week)
shapiro.test(ds$log_Past_Exam_Scores)
shapiro.test(ds$log_Final_Exam_Score)

# Appendix 12: Code for Chi-Squared Test for Gender and Pass_Fail
gender_data <- table(ds$Gender, ds$Pass_Fail)
chisq.test(gender_data)

# Appendix 13: Code for Chi-Squared Test for Pass_Fail and Study_Hours_per_week
ds$Study_Hour_Category <- cut(ds$Study_Hours_per_Week,
                              breaks = c(9, 19, 29, 39),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)

table_hours <- table(ds$Study_Hour_Category, ds$Pass_Fail)

chisq.test(table_hours)

# Appendix 14: Code for ANOVA Test of Study_Hours_per_Week and Parental_Education_Level
ds$Parental_Education_Short <- recode(ds$Parental_Education_Level,
                                      "High School" = "HS",
                                      "Bachelors" = "Bach",
                                      "Masters" = "Mast",
                                      "PhD" = "PhD")

ggplot(ds, aes(x = Parental_Education_Short, 
               y = Study_Hours_per_Week, 
               fill = Parental_Education_Short)) +
  geom_boxplot(col = "black", width = 0.4) + 
  scale_fill_manual(values = c("HS" = "#4393C3", 
                               "Bach" = "#D6604D",
                               "Mast" = "#FFC125",
                               "PhD" = "#8FBC8F")) +
  labs(title = "Study Hours by Parental Education", 
       x = "Parental Education Level", 
       y = "Study Hours per Week") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

anova_model <- aov(Study_Hours_per_Week ~ Parental_Education_Level, data = ds)

summary(anova_model)

# Appendix 15: Code for Wilcoxon Signed-Rank Test Past_Exam_Scores and Final_Exam_Score
wilcox.test(ds$Past_Exam_Scores, ds$Final_Exam_Score, paired = T)

# create new dataset scores with different structure to get density plot
scores <- data.frame(Exam = c(rep("Past_Exam_Scores", nrow(ds)), 
                              rep("Final_Exam_Score", nrow(ds))),
                     Score = c(ds$Past_Exam_Scores, ds$Final_Exam_Score))

ggplot(scores, aes(x = Score, fill = Exam)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Past vs Final Exam Scores",
       x = "Score", 
       y = "Density") +
  scale_fill_manual(values = c("#4393C3", "#8FBC8F")) +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 16: Code for Kruskal Wallis Test and Violin plot of Final_Exam_Score by Parental_Education_Level
kruskal.test (Final_Exam_Score ~ Parental_Education_Level, data = ds)

ds$Parental_Education_Short <- recode(ds$Parental_Education_Level,
                                      "High School" = "HS",
                                      "Bachelors" = "Bach",
                                      "Masters" = "Mast",
                                      "PhD" = "PhD")

ggplot(ds, aes(x = Parental_Education_Short, 
               y = Final_Exam_Score, 
               fill = Parental_Education_Short)) +
  geom_violin(trim = FALSE) +  
  scale_fill_manual(values = c("HS" = "#4393C3", 
                               "Bach" = "#D6604D",
                               "Mast" = "#FFC125",
                               "PhD" = "#8FBC8F")) +
  labs(title = "Distribution of Final Exam Scores by Parental Education",
       x = "Parental Education Level",
       y = "Final Exam Score") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 17: Code for Wilcoxon Rank Sum Test of Past_Exam_Score, Final_Exam_Score and Internet_Access_at_Home
# create new column Score_Change (final - past)
ds$Score_Change <- ds$Final_Exam_Score - ds$Past_Exam_Scores
ds$Score_Change
wilcox.test(ds$Score_Change ~ ds$Internet_Access_at_Home)

# Appendix 18: Code for Wilcoxon Rank Sum Test of Final_Exam_Score and Extracurricular_Activities
wilcox.test(Final_Exam_Score ~ Extracurricular_Activities, data = ds)

# boxplot of Final Exam Score vs Extracurricular Activities
ggplot(ds, aes(x = Extracurricular_Activities, y = Final_Exam_Score)) +
  geom_boxplot(color = "black", fill = c("#D6604D", "#4393C3")) + 
  labs(title = "Final Exam Score vs Extracurricular Activities", 
       x = "Extracurricular Activities", 
       y = "Final Exam Score") +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5),  
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold", size = 12))

# Appendix 19: Code for Friedman Rank Sum Test of Attendance_Rate, Past_Exam_Scores and Final_Exam_Score
# select relevant variables and create new dataset ds1 with different structure 
ds1 <- data.frame(Student_ID = rep(ds$Student_ID, times = 3),
                  Measurement = c(rep("Past_Exam_Scores", nrow(ds)), 
                                  rep("Attendance_Rate", nrow(ds)), 
                                  rep("Final_Exam_Score", nrow(ds))),
                  Value = c(ds$Past_Exam_Scores, ds$Attendance_Rate, ds$Final_Exam_Score))
friedman.test(Value ~ Measurement | Student_ID, data = ds1)
pairwise.wilcox.test(ds1$Value, ds1$Measurement, p.adjust.method = "none")

# Appendix 20: Code for Correlations between Final_Exam_Score and other continuous variables, Study_Hours_per_Week, Attendance_Rate and Past_Exam_Scores
# create new dataset ds2 with continuous variables only
ds2 <- ds[, c(3, 4, 5, 9)]
pairs.panels(ds2, 
             method = "spearman",     # variables are not normal
             pch = 16, 
             density = TRUE, 
             ellipses = FALSE,
             hist.col = "darkseagreen",
             cex.cor = 1.5,
             cex.labels = 1.5)

# Appendix 21: Code for Regression of Final_Exam_Score, and Study_Hours_per_Week, Attendance_Rate and Past_Exam_Scores
ds2 <- ds[, c(3, 4, 5, 9)]

model1 <- lm(Final_Exam_Score ~ Study_Hours_per_Week + Attendance_Rate + Past_Exam_Scores, 
            data = ds2)

summary(model1)
