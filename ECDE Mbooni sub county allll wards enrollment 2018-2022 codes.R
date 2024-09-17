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

# Updated data for Mbooni wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Mbooni", "Tulimani", "Kithungo/kitundu", "Kalawa", "Kako/waia", "Kisau/kiteta"),
  Public_2022 = c(1605, 1612, 1399, 1224, 1016, 1677),
  Private_2022 = c(75, 111, 0, 97, 87, 292),
  Public_2021 = c(1588, 1713, 1498, 1416, 1217, 1897),
  Private_2021 = c(68, 106, 0, 135, 83, 180),
  Public_2020 = c(1314, 1255, 1216, 1144, 934, 1595),
  Private_2020 = c(50, 168, 0, 139, 89, 239),
  Public_2019 = c(2042, 1603, 1571, 1414, 1132, 1862),
  Private_2019 = c(192, 146, 0, 166, 141, 354),
  Public_2018 = c(2460, 1591, 1808, 1346, 1217, 1942),
  Private_2018 = c(205, 146, 0, 167, 140, 354)
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
# Mbooni Ward
enrollment_mbooni <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_mbooni <- c("Public", "Private")

pie(enrollment_mbooni, labels = labels_mbooni, 
    main = "Mbooni Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Tulimani Ward
enrollment_tulimani <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_tulimani <- c("Public", "Private")

pie(enrollment_tulimani, labels = labels_tulimani, 
    main = "Tulimani Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Kithungo/Kitundu Ward
enrollment_kithungo <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_kithungo <- c("Public", "Private")

pie(enrollment_kithungo, labels = labels_kithungo, 
    main = "Kithungo/kitundu Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Kalawa Ward
enrollment_kalawa <- c(Y$Total_Public[4], Y$Total_Private[4])
labels_kalawa <- c("Public", "Private")

pie(enrollment_kalawa, labels = labels_kalawa, 
    main = "Kalawa Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))
# Kako/waia Ward
enrollment_kako <- c(Y$Total_Public[5], Y$Total_Private[5])
labels_kako <- c("Public", "Private")

pie(enrollment_kako, labels = labels_kako, 
    main = "Kako/waia Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Kisau/kiteta Ward
enrollment_kisau <- c(Y$Total_Public[6], Y$Total_Private[6])
labels_kisau <- c("Public", "Private")

pie(enrollment_kisau, labels = labels_kisau, 
    main = "Kisau/kiteta Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))





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
