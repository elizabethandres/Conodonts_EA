library(readr)
Kat_Hir_Species_data <- read_csv("Emilia 2020/Day 1/8_20_20.csv", 
                                 col_types = cols(collection_no = col_character(), 
                                                  abund_value = col_double()))                               
library(dplyr)
library(tidyr)
library(vegan)
library(gridBase)
library(grid)
library(ggplot2)
install.packages("Rmisc")
library(Rmisc)
Kat_Hir_Rho <- data.frame(Kat_Hir_Species_data)  
#created data frame with Raw Paleodb data
Kat_Hir_Rho_Nab <- Kat_Hir_Rho[-which(is.na(Kat_Hir_Rho$abund_value)), ]
#removed Na in species abundance
with(Kat_Hir_Rho_Nab, rowsum(+!is.na(Kat_Hir_Rho_Nab$abund_value),Kat_Hir_Rho_Nab$abund_unit))
#checked how many of each time of data point there was
K_H_R_Nab <- subset(Kat_Hir_Rho_Nab, Kat_Hir_Rho_Nab$abund_unit!="%-individuals")
K_H_R_Nab <- subset(K_H_R_Nab, K_H_R_Nab$abund_unit!="%-elements")
with(K_H_R_Nab, rowsum(+!is.na(K_H_R_Nab$abund_value),K_H_R_Nab$abund_unit))
#took out percent individuals as a measurement type
Kat_age  <- K_H_R_Nab[ which(K_H_R_Nab$early_interval == "Katian"| K_H_R_Nab$early_interval == "Vormsi" | K_H_R_Nab$early_interval == "Cautleyan" | K_H_R_Nab$early_interval == "Richmondian" ), ]
Hir_age  <- K_H_R_Nab[ which(K_H_R_Nab$early_interval == "Gamachian"| K_H_R_Nab$early_interval == "Hirnantian" ), ]
Rhu_age  <- K_H_R_Nab[ which(K_H_R_Nab$early_interval == "Rhuddanian"), ]
Ash_age  <- K_H_R_Nab[ which(K_H_R_Nab$early_interval == "Ashgill"| K_H_R_Nab$early_interval == "Ashgillian" 
                             | K_H_R_Nab$early_interval == "Katian"| K_H_R_Nab$early_interval == "Vormsi" | K_H_R_Nab$early_interval == "Cautleyan" | K_H_R_Nab$early_interval == "Richmondian" 
                             | K_H_R_Nab$early_interval == "Gamachian"| K_H_R_Nab$early_interval == "Hirnantian"), ]
#Seperated by age
Kat_abund_unit <- table(Kat_age$abund_unit)
Hir_abund_unit <- table(Hir_age$abund_unit)
Rhu_abund_unit <- table(Rhu_age$abund_unit)
Ash_abund_unit <- table(Ash_age$abund_unit)
#Type of data collected 
Kat_Com <- data.frame(Kat_age$collection_no, Kat_age$identified_name, Kat_age$abund_value)
Hir_Com <- data.frame(Hir_age$collection_no, Hir_age$identified_name, Hir_age$abund_value)
Rhu_Com <- data.frame(Rhu_age$collection_no, Rhu_age$identified_name, Rhu_age$abund_value)
Ash_Com <- data.frame(Ash_age$collection_no, Ash_age$identified_name, Ash_age$abund_value)
#Created areas with only nessacary columns
names(Kat_Com)[names(Kat_Com) == "Kat_age.collection_no"] <- "Col.no"
names(Kat_Com)[names(Kat_Com) == "Kat_age.abund_value"] <- "Abund"
names(Kat_Com)[names(Kat_Com) == "Kat_age.identified_name"] <- "Species.name"
#Changed column names for Kat
names(Hir_Com)[names(Hir_Com) == "Hir_age.collection_no"] <- "Col.no"
names(Hir_Com)[names(Hir_Com) == "Hir_age.abund_value"] <- "Abund"
names(Hir_Com)[names(Hir_Com) == "Hir_age.identified_name"] <- "Species.name"
#Changed Column names for Hir
names(Rhu_Com)[names(Rhu_Com) == "Rhu_age.collection_no"] <- "Col.no"
names(Rhu_Com)[names(Rhu_Com) == "Rhu_age.abund_value"] <- "Abund"
names(Rhu_Com)[names(Rhu_Com) == "Rhu_age.identified_name"] <- "Species.name"
#changed column names for Rhu
names(Ash_Com)[names(Ash_Com) == "Ash_age.collection_no"] <- "Col.no"
names(Ash_Com)[names(Ash_Com) == "Ash_age.abund_value"] <- "Abund"
names(Ash_Com)[names(Ash_Com) == "Ash_age.identified_name"] <- "Species.name"
#changed column names for Ashgill
Kat_wide <- pivot_wider(data = Kat_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0)
Hir_wide <- pivot_wider(data = Hir_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0)
Ash_wide <- pivot_wider(data = Ash_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0)
Rhu_wide <- pivot_wider(data = Rhu_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0, values_fn = length)
#made correct format to use with Vegan
Rhu_Com_nodup <- Rhu_Com[-c(355,356,358),]
#error in the data ment there were duplicates of these 3 rows
Rhu_wide <- pivot_wider(data = Rhu_Com_nodup, names_from = "Species.name", values_from = "Abund", values_fill = 0,)
#Put Rhu into correct format to be used with vegan
Rhu_wide <- data.frame(Rhu_wide, row.names = 1)
Kat_wide <- data.frame(Kat_wide, row.names = 1)
Hir_wide <- data.frame(Hir_wide, row.names = 1)
Ash_wide <- data.frame(Ash_wide, row.names = 1)
#made the row names into the col.no
Hir_wide_species_num <- pivot_wider(data = Hir_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0, values_fn = length)
Kat_wide_species_num <- pivot_wider(data = Kat_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0, values_fn = length )
Rhu_wide_species_num <- pivot_wider(data = Rhu_Com_nodup, names_from = "Species.name", values_from = "Abund", values_fill = 0, values_fn = length)
Ash_wide_species_num <- pivot_wider(data = Ash_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0, values_fn =length)
#these dfs only show a 1 or 0 for presenec or absence of the species in question
Rhu_wide_species_num <- data.frame(Rhu_wide_species_num, row.names = 1)
Kat_wide_species_num <- data.frame(Kat_wide_species_num, row.names = 1)
Hir_wide_species_num <- data.frame(Hir_wide_species_num, row.names = 1)
Ash_wide_species_num <- data.frame(Ash_wide_species_num, row.names = 1)
#made the row names into the col.no
Hir_Species_no. <- rowSums(Hir_wide_species_num[,1:21])
Kat_Species_no. <- rowSums(Kat_wide_species_num[,1:31])
Rhu_Species_no. <- rowSums(Rhu_wide_species_num[,1:22])
Ash_Species_no. <- rowSums(Ash_wide_species_num[,1:44])
#created tables with number species per collection
write.csv(Hir_Species_no. , "C:/Users/elori/Desktop/Summer2020/Emilia/Hir_species_no_8_20.csv")
write.csv(Kat_Species_no. , "C:/Users/elori/Desktop/Summer2020/Emilia/Kat_species_no_8_20.csv")
write.csv(Rhu_Species_no. , "C:/Users/elori/Desktop/Summer2020/Emilia/Rhu_species_no_8_20.csv")
write.csv(Ash_Species_no. , "C:/Users/elori/Desktop/Summer2020/Emilia/Ash_species_no_8_20.csv")
#Saved a copy of these tables
Rhu_Species_no._mean <- mean(Rhu_Species_no.)
Hir_Species_no._mean <- mean(Hir_Species_no.)
Kat_Species_no._mean <- mean(Kat_Species_no.)
Ash_Species_no._mean <- mean(Ash_Species_no.)
#shows average species per collection
Rhu_shannon_index <- diversity(Rhu_wide)
Hir_shannon_index <- diversity(Hir_wide)
Kat_shannon_index <- diversity(Kat_wide)
Ash_shannon_index <- diversity(Ash_wide)
#diversity index achieved
Rhu_shannon_index_mean <- mean(Rhu_shannon_index)
Hir_shannon_index_mean <- mean(Hir_shannon_index)
Kat_shannon_index_mean <- mean(Kat_shannon_index)
Ash_shannon_index_mean <- mean(Ash_shannon_index)
#mean Shannon Index diversity for each
Rhu_Evenness <- (Rhu_shannon_index)/log(specnumber(Rhu_wide))
Hir_Evenness <- (Hir_shannon_index)/log(specnumber(Hir_wide))
Kat_Evenness <- (Kat_shannon_index)/log(specnumber(Kat_wide))
Ash_Evenness <- (Ash_shannon_index)/log(specnumber(Ash_wide))
# calculated Pielou's evenness for each
Rhu._Evenness <- na.omit(Rhu_Evenness)
Hir._Evenness <- na.omit(Hir_Evenness)
Kat._Evenness <- na.omit(Kat_Evenness)
Ash._Evenness <- na.omit(Ash_Evenness)
#removed Na vaules from eveness
#Note: These were the rows removed:142519, 142543, 152555, 42552, 142630, 142633, 142698, 142694, 142699,  142701, 142702, 142704, 142705, 142758, 142774, 181287, 181288, 181289, 181290, 181291, 181294, 181295, 181296, 181297, 181300, 181302, 181303 
Rhu_Evenness_mean <- mean(Rhu._Evenness)
Hir_Evenness_mean <- mean(Hir._Evenness)
Kat_Evenness_mean <- mean(Kat._Evenness)
Ash_Evenness_mean <- mean(Ash._Evenness)
#mean Evenness for each time period
Rhu_simpson_index <- diversity(Rhu_wide, index = "simpson")
Hir_simpson_index <- diversity(Hir_wide, index = "simpson")
Kat_simpson_index <- diversity(Kat_wide, index = "simpson")
Ash_simpson_index <- diversity(Ash_wide, index = "simpson")
#diversity index achieved
Rhu_simpson_index_mean <- mean(Rhu_simpson_index)
Hir_simpson_index_mean <- mean(Hir_simpson_index)
Kat_simpson_index_mean <- mean(Kat_simpson_index)
Ash_simpson_index_mean <- mean(Ash_simpson_index)
#mean Shannon Index diversity for each
Kat_order_simpson <- sort(Kat_simpson_index, decreasing = TRUE)
Hir_order_simpson <- sort(Hir_simpson_index, decreasing = TRUE)
Rhu_order_simpson <- sort(Rhu_simpson_index, decreasing = TRUE)
Ash_order_simpson <- sort(Ash_simpson_index, decreasing = TRUE)
#Sorted order for a bar chart
barplot(Kat_order_simpson, xlab = "Collection Number", ylab = "Simpson Index", main = "Katian Simpson Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Rhu_order_simpson, xlab = "Collection Number", ylab = "Simpson Index", main = "Rhuddanian Simpson Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Hir_order_simpson, xlab = "Collection Number", ylab = "Simpson Index", main = "Hirnantian Simpson Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Ash_order_simpson, xlab = "Collection Number", ylab = "Simpson Index", main = "Katian and Hirnantian Simpson Index", col="#69b3a2", las=2, cex.names=.5)
#plotted data for simpson index
Kat_order_shannon <- sort(Kat_shannon_index, decreasing = TRUE)
Hir_order_shannon <- sort(Hir_shannon_index, decreasing = TRUE)
Rhu_order_shannon <- sort(Rhu_shannon_index, decreasing = TRUE)
Ash_order_shannon <- sort(Ash_shannon_index, decreasing = TRUE)
#Sorted order for a bar chart
barplot(Kat_order_shannon, xlab = "Collection Number", ylab = "Shannon Index", main = "Katian Shannon Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Rhu_order_shannon, xlab = "Collection Number", ylab = "Shannon Index", main = "Rhuddanian Shannon Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Hir_order_shannon, xlab = "Collection Number", ylab = "Shannon Index", main = "Hirnantian Shannon Index", col="#69b3a2", las=2, cex.names=.5)
barplot(Ash_order_shannon, xlab = "Collection Number", ylab = "Shannon Index", main = "Katian and Hirnantian Shannon Index", col="#69b3a2", las=2, cex.names=.5)
#plotted data for shannon index
Kat_order_Evenness <- sort(Kat_Evenness, decreasing = TRUE)
Hir_order_Evenness <- sort(Hir_Evenness, decreasing = TRUE)
Rhu_order_Evenness <- sort(Rhu_Evenness, decreasing = TRUE)
Ash_order_Evenness <- sort(Ash_Evenness, decreasing = TRUE)
#Sorted order for a bar chart
barplot(Kat_order_Evenness, xlab = "Collection Number", ylab = "Pielou's Evenness", main = "Katian Evenness", col="#69b3a2", las=2, cex.names=.5)
barplot(Rhu_order_Evenness, xlab = "Collection Number", ylab = "Pielou's Evenness", main = "Rhuddanian Evenness", col="#69b3a2", las=2, cex.names=.5)
barplot(Hir_order_Evenness, xlab = "Collection Number", ylab = "Pielou's Evenness", main = "Hirnantian Evenness", col="#69b3a2", las=2, cex.names=.5)
barplot(Ash_order_Evenness, xlab = "Collection Number", ylab = "Pielou's Evenness", main = "Katian and Hirnantian Evenness", col = "#69b3a2", las = 2, cex.names = .5)
#plotted data for simpson index
Kat_order_Species_no. <- sort(Kat_Species_no., decreasing = TRUE)
Hir_order_Species_no. <- sort(Hir_Species_no., decreasing = TRUE)
Rhu_order_Species_no. <- sort(Rhu_Species_no., decreasing = TRUE)
Ash_order_species_no. <- sort(Ash_Species_no., decreasing = TRUE)
#Sorted order for a bar chart
barplot(Kat_order_Species_no., xlab = "Collection Number", ylab = "Species Number", main = "Katian Species Count", col="#69b3a2", las=2, cex.names=.5)
barplot(Rhu_order_Species_no., xlab = "Collection Number", ylab = "Species Number", main = "Rhuddanian Species Count", col="#69b3a2", las=2, cex.names=.5)
barplot(Hir_order_Species_no., xlab = "Collection Number", ylab = "Species Number", main = "Hirnantian Species Count", col="#69b3a2", las=2, cex.names=.5)
barplot(Ash_order_species_no., xlab = "Collection Number", ylab = "Species Number", main = "Katian and Hirnantian Species Count", col="#69b3a2", las=2, cex.names=.5)
#plotted data for Species Count index
Rhu_Species_type <- colSums(Rhu_wide)
Rhu_order_Species_type <- sort(Rhu_Species_type, decreasing = TRUE)
#prepare Rhu data to plot
par(mar = c(9, 4, 4, 4))
#increase size bottem margin so names fit
barplot(Rhu_order_Species_type, xlab = "", ylab = "Count", main = "Rhuddanian Species Type", col="#69b3a2", cex.names= .6, las=2 )
#plot Rhu species type
Hir_Species_type <- colSums(Hir_wide)
Hir_order_Species_type <- sort(Hir_Species_type, decreasing = TRUE)
barplot(Hir_order_Species_type, xlab = "", ylab = "Count", main = "Hirnantian Species Type", col="#69b3a2", las=2, cex.names=.6)
#Plot Hir Species type
Kat_Species_type <- colSums(Kat_wide)
Kat_order_Species_type <- sort(Kat_Species_type, decreasing = TRUE)
barplot(Kat_order_Species_type, xlab = "", ylab = "Count", main = "Katian Species Type", col="#69b3a2", las=2, cex.names=.6)
#Barplot Kat speies type
Ash_Species_type <- colSums(Ash_wide)
Ash_order_Species_type <- sort(Ash_Species_type, decreasing = TRUE)
barplot(Ash_order_Species_type, xlab = "", ylab = "Count", main = "Katian and Hirnantian Species Type", col="#69b3a2", las=2, cex.names=.6)
#Barplot Ash speies type
Late_Ord_age <- K_H_R_Nab[ which(K_H_R_Nab$early_interval == "Late Ordovician"), ]
write.csv(Late_Ord_age , "C:/Users/elori/Desktop/Summer2020/Emilia/Late_Ord_age_data.csv")
Late_Ord_Com <- data.frame(Late_Ord_age$collection_no, Late_Ord_age$identified_name, Late_Ord_age$abund_value)
names(Late_Ord_Com)[names(Late_Ord_Com) == "Late_Ord_age.collection_no"] <- "Col.no"
names(Late_Ord_Com)[names(Late_Ord_Com) == "Late_Ord_age.abund_value"] <- "Abund"
names(Late_Ord_Com)[names(Late_Ord_Com) == "Late_Ord_age.identified_name"] <- "Species.name"
Late_Ord_wide <- pivot_wider(data = Late_Ord_Com, names_from = "Species.name", values_from = "Abund", values_fill = 0)
write.csv(Late_Ord_wide , "C:/Users/elori/Desktop/Summer2020/Emilia/Late_Ord_Collections.csv")
#Below calculating confidence intervals for Simpson diversity index
CI_Rhu_order_simpson <- CI(Rhu_order_simpson)
CI_Ash_order_simpson <- CI(Ash_order_simpson)
CI_Hir_order_simpson <- CI(Hir_order_simpson)
CI_Kat_order_simpson <- CI(Kat_order_simpson)
#Below calculating confidence intervals for shannon diversity index
CI_Rhu_order_shannon <- CI(Rhu_order_shannon)
CI_Ash_order_shannon <- CI(Ash_order_shannon)
CI_Hir_order_shannon <- CI(Hir_order_shannon)
CI_Kat_order_shannon <- CI(Kat_order_shannon)
#Below calculating confidence intervals for evenness 
CI_Rhu_order_evenness <- CI(Rhu._Evenness)
CI_Ash_order_evenness <- CI(Ash._Evenness)
CI_Hir_order_evenness <- CI(Hir._Evenness)
CI_Kat_order_evenness <- CI(Kat._Evenness)
