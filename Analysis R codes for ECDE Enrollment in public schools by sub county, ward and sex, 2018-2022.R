# Load necessary libraries
library(ggplot2)

# Input the data
data <- data.frame(
  Ward = c("Mbooni", "Tulimani", "Kithungo/kitundu", "Kalawa", "Kako/waia", "Kisau/kiteta", "Mukaa/kitaingo", 
           "Kiima kiu/kalanzoni", "Kasikeu", "Ukia", "Kilungu", "Ilima", "Kee", "Wote/nziu", 
           "Muvau/kikumini", "Kathonzweni", "Kitise/kithuki", "Mbitini", "Nzaui/kilili/kalamba", 
           "Mavindini", "Makindu", "Nguu/masumba", "Emali/mulala", "Nguumo", "Kikumbulyu north", 
           "Kikumbulyu south", "Ivingoni/nzambani", "Mtito andei", "Thange", "Masongaleni"),
  
  Boys_2018 = c(1212, 711, 881, 724, 572, 908, 768, 1078, 1136, 988, 693, 695, 463, 418, 687, 806, 618, 753, 979, 500, 945, 651, 514, 683, 569, 857, 773, 809, 679, 1030),
  Girls_2018 = c(1248, 880, 927, 622, 645, 1034, 787, 1113, 954, 870, 669, 740, 451, 333, 624, 791, 679, 763, 988, 523, 1078, 654, 568, 809, 542, 900, 774, 872, 774, 1022),
  
  Boys_2019 = c(944, 799, 750, 729, 547, 869, 592, 878, 820, 897, 738, 689, 459, 370, 632, 711, 595, 668, 974, 506, 907, 631, 488, 725, 458, 721, 751, 761, 677, 847),
  Girls_2019 = c(1098, 804, 821, 685, 585, 993, 616, 913, 846, 819, 748, 716, 441, 350, 638, 767, 592, 710, 896, 463, 1055, 680, 523, 777, 426, 764, 760, 886, 778, 842),
  
  Boys_2020 = c(710, 663, 575, 606, 503, 865, 543, 830, 777, 876, 628, 726, 461, 273, 512, 699, 559, 615, 868, 455, 808, 555, 458, 563, 381, 548, 591, 752, 614, 723),
  Girls_2020 = c(604, 592, 641, 538, 431, 730, 492, 758, 728, 788, 602, 644, 399, 234, 455, 610, 501, 581, 773, 439, 654, 517, 401, 553, 375, 524, 570, 886, 565, 878),
  
  Boys_2021 = c(792, 908, 753, 731, 643, 1010, 627, 829, 884, 1010, 783, 853, 531, 438, 585, 742, 548, 664, 941, 478, 981, 639, 550, 775, 570, 803, 726, 935, 852, 868),
  Girls_2021 = c(796, 805, 745, 685, 574, 887, 595, 921, 845, 918, 703, 761, 491, 381, 553, 661, 504, 646, 905, 457, 922, 606, 520, 761, 517, 763, 711, 901, 706, 822),
  
  Boys_2022 = c(797, 855, 705, 614, 547, 867, 629, 941, 897, 838, 675, 686, 448, 374, 510, 703, 569, 692, 924, 541, 946, 604, 500, 718, 476, 668, 615, 902, 743, 767),
  Girls_2022 = c(808, 757, 694, 610, 469, 810, 608, 895, 861, 821, 582, 637, 380, 370, 496, 676, 555, 665, 849, 499, 907, 548, 506, 647, 467, 612, 594, 805, 632, 752)
)

# Calculate total boys and girls enrollment for each ward across years
data$Total_Boys <- rowSums(data[, grep("Boys", names(data))])
data$Total_Girls <- rowSums(data[, grep("Girls", names(data))])
data$Total_Enrollment <- data$Total_Boys + data$Total_Girls

# Print total enrollment per ward
print("Total Enrollment (Boys, Girls, and Overall) for each ward across 2018–2022:")
print(data[, c("Ward", "Total_Boys", "Total_Girls", "Total_Enrollment")])

# Overall totals for boys and girls across all wards and years
overall_total_boys <- sum(data$Total_Boys)
overall_total_girls <- sum(data$Total_Girls)
overall_total_enrollment <- overall_total_boys + overall_total_girls

print(paste("Overall total boys enrollment (2018–2022):", overall_total_boys))
print(paste("Overall total girls enrollment (2018–2022):", overall_total_girls))
print(paste("Overall total enrollment (2018–2022):", overall_total_enrollment))

# Calculate ratios of boys to girls and girls to boys
data$Ratio_Boys_to_Girls <- data$Total_Boys / data$Total_Girls
data$Ratio_Girls_to_Boys <- data$Total_Girls / data$Total_Boys

print("Ratios of Boys to Girls and Girls to Boys for each ward:")
print(data[, c("Ward", "Ratio_Boys_to_Girls", "Ratio_Girls_to_Boys")])

# Bar graph for boys and girls enrollment by ward across all years
ggplot(data, aes(x = Ward)) +
  geom_bar(aes(y = Total_Boys, fill = "Boys"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Girls, fill = "Girls"), stat = "identity", position = "dodge") +
  labs(title = "Boys vs Girls Enrollment by Ward (2018–2022)", y = "Number of Enrollments") +
  scale_fill_manual(name = "Gender", values = c("Boys" = "blue", "Girls" = "pink")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the data
head(data)
