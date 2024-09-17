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

# Updated data for Kaiti wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Ukia", "Kilungu", "Ilima", "Kee"),
  Public_2022 = c(1659, 1257, 1323, 1828),
  Private_2022 = c(368, 132, 28, 0),
  Public_2021 = c(1928, 1486, 1614, 1022),
  Private_2021 = c(359, 189, 40, 0),
  Public_2020 = c(1664, 1230, 1370, 860),
  Private_2020 = c(382, 187, 30, 0),
  Public_2019 = c(1716, 1486, 1405, 900),
  Private_2019 = c(481, 318, 25, 0),
  Public_2018 = c(1858, 1362, 1435, 914),
  Private_2018 = c(468, 300, 0, 0)
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
# Ukia Ward
enrollment_ukia <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_ukia <- c("Public", "Private")

pie(enrollment_ukia, labels = labels_ukia, 
    main = "Ukia Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Kilungu Ward
enrollment_kilungu <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_kilungu <- c("Public", "Private")

pie(enrollment_kilungu, labels = labels_kilungu, 
    main = "Kilungu Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Ilima Ward
enrollment_ilima <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_ilima <- c("Public", "Private")

pie(enrollment_ilima, labels = labels_ilima, 
    main = "Ilima Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Kee Ward
enrollment_kee <- c(Y$Total_Public[4], Y$Total_Private[4])
labels_kee <- c("Public", "Private")

pie(enrollment_kee, labels = labels_kee, 
    main = "Kee Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

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

