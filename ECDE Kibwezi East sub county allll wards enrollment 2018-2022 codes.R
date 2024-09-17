# Load data
Y = read.csv(file.choose())
Y
head(Y)
tail(Y)
names(Y)
str(Y)
summary(Y)

# Check for any missing data
summary(Y)

# Load necessary libraries
library(ggplot2)

# Updated data for Kibwezi East wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Ivingoni/Nzambani", "Mtito Andei", "Thange", "Masongaleni"),
  Public_2022 = c(1209, 1707, 1375, 1519),
  Private_2022 = c(377, 313, 364, 18),
  Public_2021 = c(1437, 1836, 1558, 1690),
  Private_2021 = c(381, 367, 354, 15),
  Public_2020 = c(1161, 1480, 1179, 1401),
  Private_2020 = c(208, 194, 195, 14),
  Public_2019 = c(1511, 1647, 1455, 1689),
  Private_2019 = c(329, 466, 290, 22),
  Public_2018 = c(1547, 1681, 1453, 2052),
  Private_2018 = c(371, 342, 348, 19)
)

# Summing up the public and private enrollments across all years
Y$Total_Public <- rowSums(Y[, grep("Public", names(Y))])
Y$Total_Private <- rowSums(Y[, grep("Private", names(Y))])
Y$Total_Enrollment <- Y$Total_Public + Y$Total_Private

print("Total public and private enrollment for each ward (2018–2022):")
print(Y)

# Overall Total Public and Private Enrollment (2018–2022)
total_public <- sum(Y$Total_Public)
total_private <- sum(Y$Total_Private)
total_enrollment <- sum(Y$Total_Enrollment)

print(paste("Total public enrollment overall (2018–2022):", total_public))
print(paste("Total private enrollment overall (2018–2022):", total_private))
print(paste("Total enrollment overall (2018–2022):", total_enrollment))

# Proportions of Public vs Private Enrollment (2018–2022)
proportion_public <- total_public / total_enrollment
proportion_private <- total_private / total_enrollment

print(paste("Proportion of public enrollment:", proportion_public))
print(paste("Proportion of private enrollment:", proportion_private))

# Bar Plot for Public vs Private Enrollment (2018–2022)
ggplot(Y, aes(x = Ward)) +
  geom_bar(aes(y = Total_Public, fill = "Public"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Private, fill = "Private"), stat = "identity", position = "dodge") +
  labs(title = "Public vs Private Enrollment by Ward (2018–2022)", y = "Number of Enrollments") +
  scale_fill_manual(name = "Enrollment Type", values = c("Public" = "blue", "Private" = "red"))

# Pie Chart for Total Public vs Total Private (2018–2022)
total_enrollments <- c(total_public, total_private)
enrollment_labels <- c("Public", "Private")

pie(total_enrollments, labels = enrollment_labels, col = c("blue", "red"), main = "Total Public vs Private Enrollment (2018–2022)")

# Pie charts for each ward's enrollment across all years (2018–2022)
# Ivingoni/Nzambani Ward
enrollment_ivingoni <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_ivingoni <- c("Public", "Private")

pie(enrollment_ivingoni, labels = labels_ivingoni, 
    main = "Ivingoni/Nzambani Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Mtito Andei Ward
enrollment_mtito <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_mtito <- c("Public", "Private")

pie(enrollment_mtito, labels = labels_mtito, 
    main = "Mtito Andei Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Thange Ward
enrollment_thange <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_thange <- c("Public", "Private")

pie(enrollment_thange, labels = labels_thange, 
    main = "Thange Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Masongaleni Ward
enrollment_masongaleni <- c(Y$Total_Public[4], Y$Total_Private[4])
labels_masongaleni <- c("Public", "Private")

pie(enrollment_masongaleni, labels = labels_masongaleni, 
    main = "Masongaleni Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Correlation Analysis
# Correlation between public and private (2018–2022)
cor_public_private <- cor(Y$Total_Public, Y$Total_Private, use = "complete.obs")
print(paste("Correlation between public and private enrollment (2018–2022):", cor_public_private))

# Scatter Plot: Public vs Private (2018–2022)
ggplot(Y, aes(x = Total_Public, y = Total_Private)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Public vs Private Enrollment (2018–2022)", x = "Public Enrollment", y = "Private Enrollment")

# Scatter Plot: Public vs Total Enrollment (2018–2022)
ggplot(Y, aes(x = Total_Public, y = Total_Enrollment)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot: Public vs Total Enrollment (2018–2022)", x = "Public Enrollment", y = "Total Enrollment")

# Scatter Plot: Private vs Total Enrollment (2018–2022)
ggplot(Y, aes(x = Total_Private, y = Total_Enrollment)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatter Plot: Private vs Total Enrollment (2018–2022)", x = "Private Enrollment", y = "Total Enrollment")
