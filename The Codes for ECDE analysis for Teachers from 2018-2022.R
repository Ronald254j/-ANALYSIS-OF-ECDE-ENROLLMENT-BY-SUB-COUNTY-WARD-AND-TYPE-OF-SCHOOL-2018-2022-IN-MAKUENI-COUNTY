# Load data
# Check for any missing data
summary(Y)

# Load necessary libraries
library(ggplot2)

# Complete data input for all years
Y <- data.frame(
  Ward = c('Mbooni', 'Tulimani', 'Kithungo/Kitundu', 'Kalawa', 'KaKo/Waia', 'Kisau/Kiteta', 
           'Mukaa/Kitaingo', 'Kiimakiu/Kalanzoni', 'Kasikeu', 'Ukia', 'Kilungu', 'Ilima', 
           'Kee', 'Wote,Nziu', 'Muvau/Kikumini', 'Kathonzweni', 'Kitise/Kithuki', 'Mbitini', 
           'Nzaui/Kilili/Kalamba', 'Mavindini', 'Makindu', 'Nguu/Masumba', 'Emali/Mulala', 
           'Nguumo', 'Kikumbulyu North', 'Kikumbulyu South', 'Ivingoni/Nzambani', 'Mtito Andei', 
           'Thange', 'Masongaleni'),
# Data for 2018 to 2022 for Public, Private, and Total teachers

  Public_2018 = c(90, 78, 79, 64, 53, 104, 66, 49, 87, 48, 59, 64, 52, 20, 38, 37, 34, 66, 80, 25, 65, 53, 39, 39, 38, 53, 58, 71, 57, 56),
  Private_2018 = c(5, 6, 0, 10, 8, 18, 3, 11, 8, 25, 18, 0, 0, 36, 15, 9, 0, 9, 15, 5, 25, 2, 18, 7, 9, 11, 15, 18, 12, 2),
  Total_2018 = c(95, 84, 79, 74, 61, 122, 69, 60, 95, 73, 77, 64, 52, 56, 53, 46, 34, 75, 95, 30, 90, 55, 57, 46, 47, 64, 73, 89, 69, 58),
  
  Public_2019 = c(90, 78, 79, 68, 55, 104, 66, 69, 86, 48, 66, 62, 53, 20, 38, 37, 34, 67, 75, 25, 70, 62, 49, 48, 38, 53, 58, 71, 58, 72),
  Private_2019 = c(5, 6, 0, 10, 8, 15, 2, 12, 8, 25, 20, 2, 0, 38, 19, 11, 0, 8, 15, 7, 20, 2, 18, 7, 9, 11, 20, 24, 12, 1),
  Total_2019 = c(95, 84, 79, 78, 63, 119, 68, 81, 94, 73, 86, 64, 53, 58, 57, 48, 34, 75, 90, 32, 90, 64, 67, 55, 47, 64, 78, 95, 70, 73),
  
  Public_2020 = c(115, 95, 81, 75, 60, 106, 64, 68, 89, 80, 48, 58, 46, 33, 61, 77, 68, 69, 93, 54, 54, 61, 46, 33, 42, 54, 62, 78, 60, 70),
  Private_2020 = c(5, 6, 0, 8, 7, 11, 2, 12, 12, 44, 10, 2, 0, 38, 13, 7, 0, 10, 9, 4, 23, 5, 13, 4, 8, 8, 14, 12, 17, 2),
  Total_2020 = c(120, 101, 81, 83, 67, 117, 66, 80, 101, 124, 58, 60, 46, 71, 74, 84, 68, 79, 102, 58, 77, 66, 59, 37, 50, 62, 76, 90, 77, 72),
  
  Public_2021 = c(57, 44, 40, 83, 66, 109, 66, 76, 90, 94, 66, 69, 49, 33, 57, 74, 68, 69, 83, 58, 83, 62, 48, 42, 55, 64, 64, 79, 62, 71),
  Private_2021 = c(2, 5, 0, 10, 4, 13, 2, 22, 12, 20, 10, 1, 0, 38, 12, 7, 0, 7, 7, 4, 44, 6, 18, 8, 10, 18, 14, 12, 17, 2),
  Total_2021 = c(59, 49, 40, 93, 70, 122, 68, 98, 102, 114, 76, 70, 49, 87, 69, 81, 68, 76, 90, 58, 127, 68, 66, 50, 65, 82, 78, 91, 79, 73),
  
  Public_2022 = c(95, 82, 73, 78, 64, 103, 67, 93, 102, 94, 63, 68, 54, 32, 58, 77, 69, 72, 89, 55, 87, 67, 48, 43, 50, 64, 64, 62, 79, 72),
  Private_2022 = c(5, 6, 0, 10, 5, 13, 2, 24, 12, 20, 19, 2, 0, 32, 12, 6, 1, 8, 10, 4, 42, 10, 18, 10, 10, 26, 22, 22, 22, 2),
  Total_2022 = c(100, 88, 73, 88, 69, 116, 69, 117, 114, 114, 82, 70, 54, 64, 70, 83, 70, 80, 99, 59, 129, 77, 66, 53, 60, 90, 86, 84, 101, 74)
)


# Summing up the public and private teachers across all years
Y$Total_Public <- rowSums(Y[, grep("Public", names(Y))])
Y$Total_Private <- rowSums(Y[, grep("Private", names(Y))])
Y$Total_Enrollment <- Y$Total_Public + Y$Total_Private

print("Total public and private enrollment for each ward (2018–2022):")
print(Y)

# Overall Total Public and Private teachers (2018–2022)
total_public <- sum(Y$Total_Public)
total_private <- sum(Y$Total_Private)
total_Enrollment <- sum(Y$Total_Enrollment)

print(paste("Total public teachers overall (2018–2022):", total_public))
print(paste("Total private teachers overall (2018–2022):", total_private))
print(paste("Total teachers Enrollment overall (2018–2022):", total_Enrollment))

# Proportions of Public vs Private teachers (2018–2022)
proportion_public <- total_public / total_enrollment
proportion_private <- total_private / total_enrollment

print(paste("Proportion of public enrollment:", proportion_public))
print(paste("Proportion of private enrollment:", proportion_private))

# Bar Plot for Public vs Private Enrollment (2018–2022)
ggplot(Y, aes(x = Ward)) +
  geom_bar(aes(y = Total_Public, fill = "Public"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Private, fill = "Private"), stat = "identity", position = "dodge") +
  labs(title = "Public vs Private Enrollment by Ward (2018–2022)", y = "Number of Enrollments") +
  scale_fill_manual(name = "Enrollment Type", values = c("Private" = "blue", "Public" = "red"))

# Pie Chart for Total Public vs Total Private (2018–2022)
total_enrollments <- c(total_public, total_private)
enrollment_labels <- c("Public", "Private")

pie(total_enrollments, labels = enrollment_labels, col = c("blue", "red"), main = "Total Public vs Private Enrollment (2018–2022)")

# Correlation Analysis
# Correlation between public and private teachers (2018–2022)
cor_public_private <- cor(Y$Total_Public, Y$Total_Private, use = "complete.obs")
print(paste("Correlation between public and private enrollment (2018–2022):", cor_public_private))

# Scatter Plot: Public vs Private teachers (2018–2022)
ggplot(Y, aes(x = Total_Public, y = Total_Private)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot: Public vs Private Enrollment (2018–2022)", x = "Public Enrollment", y = "Private Enrollment")

# Scatter Plot: Public vs Total teachers (2018–2022)
ggplot(Y, aes(x = Total_Public, y = Total_Enrollment)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot: Public vs Total Enrollment (2018–2022)", x = "Public Enrollment", y = "Total Enrollment")

# Scatter Plot: Private vs Total teachers (2018–2022)
ggplot(Y, aes(x = Total_Private, y = Total_Enrollment)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatter Plot: Private vs Total Enrollment (2018–2022)", x = "Private Enrollment", y = "Total Enrollment")

# Calculate Ratios
data <- data %>%
  mutate(
    Ratio_Public_Private_2018 = ifelse(Private_2018 == 0, NA, Public_2018 / Private_2018),
    Ratio_Public_Total_2018 = Public_2018 / Total_2018,
    Ratio_Private_Total_2018 = ifelse(Total_2018 == 0, NA, Private_2018 / Total_2018),
    
    Ratio_Public_Private_2019 = ifelse(Private_2019 == 0, NA, Public_2019 / Private_2019),
    Ratio_Public_Total_2019 = Public_2019 / Total_2019,
    Ratio_Private_Total_2019 = ifelse(Total_2019 == 0, NA, Private_2019 / Total_2019),
    
    Ratio_Public_Private_2020 = ifelse(Private_2020 == 0, NA, Public_2020 / Private_2020),
    Ratio_Public_Total_2020 = Public_2020 / Total_2020,
    Ratio_Private_Total_2020 = ifelse(Total_2020 == 0, NA, Private_2020 / Total_2020),
    
    Ratio_Public_Private_2021 = ifelse(Private_2021 == 0, NA, Public_2021 / Private_2021),
    Ratio_Public_Total_2021 = Public_2021 / Total_2021,
    Ratio_Private_Total_2021 = ifelse(Total_2021 == 0, NA, Private_2021 / Total_2021),
    
    Ratio_Public_Private_2022 = ifelse(Private_2022 == 0, NA, Public_2022 / Private_2022),
    Ratio_Public_Total_2022 = Public_2022 / Total_2022,
    Ratio_Private_Total_2022 = ifelse(Total_2022 == 0, NA, Private_2022 / Total_2022)
  )

# Display ratios for 2022 as an example
print(data %>% select(Ward, Ratio_Public_Private_2022, Ratio_Public_Total_2022, Ratio_Private_Total_2022))

# Plotting the ratios for 2022
ggplot(data, aes(x = Ward)) +
  geom_bar(aes(y = Ratio_Public_Private_2022), stat = "identity", fill = "blue", alpha = 0.6) +
  geom_bar(aes(y = Ratio_Public_Total_2022), stat = "identity", fill = "green", alpha = 0.6) +
  geom_bar(aes(y = Ratio_Private_Total_2022), stat = "identity", fill = "red", alpha = 0.6) +
  labs(title = "Teacher Ratios in 2022 Across Wards", x = "Ward", y = "Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

