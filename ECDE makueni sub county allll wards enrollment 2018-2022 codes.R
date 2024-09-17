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

# Updated data for Makueni wards from 2018 to 2022
Y <- data.frame(
  Ward = c("Wote/Nziu", "Muvau/Kikumini", "Kathonzweni", "Kitise/Kithuki", "Mbitini", "Nzaui/kilili/kalamba", "Mavindini"),
  Public_2022 = c(744, 1006, 1379, 1124, 1357, 1773, 1040),
  Private_2022 = c(988, 254, 131, 8, 102, 202, 47),
  Public_2021 = c(819, 1138, 1403, 1052, 1310, 1846, 935),
  Private_2021 = c(1296, 356, 140, 0, 148, 128, 52),
  Public_2020 = c(507, 967, 1309, 1060, 1196, 1641, 894),
  Private_2020 = c(971, 195, 147, 0, 122, 285, 46),
  Public_2019 = c(720, 1270, 1478, 1187, 1378, 1870, 969),
  Private_2019 = c(966, 195, 204, 0, 130, 293, 59),
  Public_2018 = c(751, 1311, 1597, 1297, 1516, 1967, 1023),
  Private_2018 = c(966, 195, 204, 0, 130, 293, 59)
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
# Woe/Nziu Ward
enrollment_wote <- c(Y$Total_Public[1], Y$Total_Private[1])
labels_wote <- c("Public", "Private")

pie(enrollment_wote, labels = labels_wote, 
    main = "Wote/nziu Ward ECDE Enrollment (2018–2022)", col = c("lightblue", "lightgreen"))

# Muvau/kikumini Ward
enrollment_muvau <- c(Y$Total_Public[2], Y$Total_Private[2])
labels_muvau <- c("Public", "Private")

pie(enrollment_muvau, labels = labels_muvau, 
    main = "Muvau/kikumini Ward ECDE Enrollment (2018–2022)", col = c("red", "green"))

# Kathonzweni Ward
enrollment_kathonzweni <- c(Y$Total_Public[3], Y$Total_Private[3])
labels_kathonzweni <- c("Public", "Private")

pie(enrollment_kathonzweni, labels = labels_kathonzweni, 
    main = "Kathonzweni Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Kitise/kithuki Ward
enrollment_kitise <- c(Y$Total_Public[4], Y$Total_Private[4])
labels_kitise <- c("Public", "Private")

pie(enrollment_kitise, labels = labels_kitise, 
    main = "Kitise/kithuki Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))


# Mbitini Ward
enrollment_mbitini <- c(Y$Total_Public[5], Y$Total_Private[5])
labels_mbitini <- c("Public", "Private")

pie(enrollment_mbitini, labels = labels_mbitini, 
    main = "Mbitini Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))



# Nzaui/kilili/kalamba Ward
enrollment_nzaui <- c(Y$Total_Public[6], Y$Total_Private[6])
labels_nzaui <- c("Public", "Private")

pie(enrollment_nzaui, labels = labels_nzaui, 
    main = "Nzaui/kilili/kalamba Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))

# Mavindini Ward
enrollment_mavindini <- c(Y$Total_Public[7], Y$Total_Private[7])
labels_mavindini <- c("Public", "Private")

pie(enrollment_mavindini, labels = labels_mavindini, 
    main = "Mavindini Ward ECDE Enrollment (2018–2022)", col = c("blue", "green"))


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
