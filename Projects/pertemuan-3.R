#Import data
anime_clean=read.csv(file.choose(""),header = TRUE,stringsAsFactors = TRUE,na.strings = c("", "NA") )
anime_clean
View(anime_clean)

#Ubah Not available pada airing_date ke NA
anime_clean$airing_date[anime_clean$airing_date == "Not available"] <- NA
#Hapus baris airing_date bernilai NA
anime_clean <- na.omit(anime_clean)

#Identifikasi tren/pola
#Plot Time Series
library(dplyr)
library(stringr)

##Ambil awal tahunnya
anime_clean <- anime_clean %>%
  mutate(
    start_date_raw = str_trim(str_split_fixed(airing_date, " to ", 2)[,1]),
    
    # Ekstrak tahun 4 digit pertama
    year = str_extract(start_date_raw, "[0-9]{4}"),
    year = as.numeric(year)
  )

summary(anime_clean$year)

##Hitung anime per tahunnya
anime_per_year <- anime_clean %>%
  group_by(year) %>%
  summarise(jumlah = n())

time_series_data <- ts(anime_per_year$jumlah, 
                       start = min(anime_per_year$year), 
                       end = max(anime_per_year$year),
                       frequency = 1)

plot(time_series_data, 
     main = "Jumlah Anime per Tahun", 
     ylab = "Jumlah Anime", 
     xlab = "Tahun",
     col = "blue",
     lwd = 2)

#Scatterplot
library(ggplot2)

ggplot(anime_clean, aes(x = members, y = scored_by)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Score vs Members") +
  xlab("Jumlah Members") + 
  ylab("Jumlah Dinilai") +
  theme_minimal()

#Facet Grid atau Facet Wrap
ggplot(anime_clean, aes(x = members, y = scored_by)) +
  geom_point(color = "black", alpha = 0.7) +
  facet_wrap(~ demographic) + 
  ggtitle("Score vs Members (Facet Demographic)") + 
  xlab("Jumlah Members") +
  ylab("Jumlah Dinilai") +
  theme_minimal()

#Korelasi
var1 <- anime_clean$score
var2 <- anime_clean$ranked

correlation <- cor(var1, var2)
print(correlation)

numeric_data <- anime_clean[sapply(anime_clean, is.numeric)]

correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

#Visualisasi Menggunakan Scatterplot
lm_model <- lm(score ~ ranked, data = numeric_data)

plot(numeric_data$ranked, numeric_data$score, 
     main = "Scatterplot of Ranked vs Score",
     xlab = "Ranked",
     ylab = "Score")

abline(lm_model, col = "red", lwd = 2)

#Visualisasi Menggunakan Library "corrplot"
install.packages("corrplot")
library(corrplot)

corrplot(correlation_matrix, method = "circle")

corrplot(correlation_matrix, method = "color", tl.cex = 0.8) 

#Visualisasi Variabel yang Memiliki Kategori
plot(anime_clean$ranked, anime_clean$score,
     col = as.factor(anime_clean$demographic),
     main = "Score vs Ranked (by Demographic)",
     xlab = "Ranked",
     ylab = "Score")

legend("topright", legend = unique(anime_clean$demographic), col = 1:length(unique(anime_clean$demographic)), pch = 1)

#Visualisasi Matriks Korelasi
install.packages("GGally")
library(GGally)

ggpairs(numeric_data[, c("ranked", "scored_by", "score", "popularity")])
