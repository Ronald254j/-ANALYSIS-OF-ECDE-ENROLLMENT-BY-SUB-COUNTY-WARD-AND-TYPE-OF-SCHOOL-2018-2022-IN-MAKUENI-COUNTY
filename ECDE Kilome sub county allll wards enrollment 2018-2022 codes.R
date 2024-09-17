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

# Updated data for Kilome wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Mukaa/kitaingo", "Kiima kiu/kalanzoni", "Kasikeu"),
  Public_2022 = c(1237, 1836, 1758),
  Private_2022 = c(58, 605, 612),
  Public_2021 = c(1222, 1750, 1729),
  Private_2021 = c(56, 531, 315),
  Public_2020 = c(1035, 1588, 1505),
  Private_2020 = c(40, 295, 317),
  Public_2019 = c(1208, 1791, 1666),
  Private_2019 = c(67, 292, 394),
  Public_2018 = c(1555, 2191, 2090),
  Private_2018 = c(58, 251, 342)
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
# Mukaa/kitaingo Ward
enrollment_mukaa <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_mukaa <- c("Public", "Private")

pie(enrollment_mukaa, labels = labels_mukaa, 
    main = "Mukaa/kitaingo Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Kiima kiu/kalanzoni Ward
enrollment_kalanzoni <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_kalanzoni <- c("Public", "Private")

pie(enrollment_kalanzoni, labels = labels_kalanzoni, 
    main = "Kiima kiu/kalanzoni Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Kasikeu Ward
enrollment_kasikeu <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_kasikeu <- c("Public", "Private")

pie(enrollment_kasikeu, labels = labels_kasikeu, 
    main = "Kasikeu Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

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
