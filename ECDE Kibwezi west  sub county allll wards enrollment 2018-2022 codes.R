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

# Updated data for Kibwezi west wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Makindu", "Nguu/Masumba", "Emali/Mulala", "Nguumo", "Kikumbulyu north", "Kikumbulyu south"),
  Public_2022 = c(1853, 1152, 1006, 1365, 943, 1280),
  Private_2022 = c(631, 131, 527, 108, 202, 198),
  Public_2021 = c(1903, 1245, 1070, 1536,1087,1566),
  Private_2021 = c(688, 179, 404, 123, 239, 303),
  Public_2020 = c(1462, 1072, 859, 1116, 756, 1072),
  Private_2020 = c(550, 157, 534, 98, 123, 148),
  Public_2019 = c(1962, 1311, 1011, 1502, 884, 1485),
  Private_2019 = c(262, 40, 248, 89, 310, 74),
  Public_2018 = c(2023, 1305, 1082, 1492, 1111, 1757),
  Private_2018 = c(718, 40, 410, 129, 286, 290)
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
# Makindu Ward
enrollment_makindu <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_makindu <- c("Public", "Private")

pie(enrollment_makindu, labels = labels_makindu, 
    main = "Makindu Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Nguu/masumba Ward
enrollment_nguu <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_nguu <- c("Public", "Private")

pie(enrollment_nguu, labels = labels_nguu, 
    main = "Nguu/masumba Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Emali/mulala Ward
enrollment_emali <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_emali <- c("Public", "Private")

pie(enrollment_emali, labels = labels_emali, 
    main = "Emali/mulala Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))


# Nguumo Ward
enrollment_nguumo <- c(Y$Total_Public[4], Y$Total_Private[4])
labels_nguumo <- c("Public", "Private")

pie(enrollment_nguumo, labels = labels_nguumo, 
    main = "Nguumo Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))


# Kikumbulyu north Ward
enrollment_kikumbulyu <- c(Y$Total_Public[5], Y$Total_Private[5])
labels_kikumbulyu <- c("Public", "Private")

pie(enrollment_kikumbulyu, labels = labels_kikumbulyu, 
    main = "Kikumbulyu north Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Kikumbulyu south Ward
enrollment_kikumbulyu <- c(Y$Total_Public[6], Y$Total_Private[6])
labels_kikumbulyu <- c("Public", "Private")

pie(enrollment_kikumbulyu, labels = labels_kikumbulyu, 
    main = "Kikumbulyu south Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))


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
