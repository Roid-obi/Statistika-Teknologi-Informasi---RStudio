# ============================================================
# Data Preparation
# Dataset: MyAnimeList Anime
# ============================================================

# Impor data CSV
data1 <- read.csv(file.choose(""), header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
View(data1)

# Melihat struktur data
str(data1)

# Ringkasan statistik dasar
summary(data1)

# ============================================================
# MISSING VALUE
# ============================================================

# Mengecek missing values
sum(is.na(data1))

# Ambil semua data kecuali kolom id
data <- data1[,-1]
head(data)

# Menghapus baris dengan missing values
data_cleaned <- na.omit(data)
head(data_cleaned)

# Save hasil baru
write.csv(data_cleaned, "anime_clean.csv", row.names = FALSE)

# Mengisi missing values dengan 0
data_zero <- data
data_zero[is.na(data_zero)] <- 0
head(data_zero)

# Mengisi missing values dengan mean (khusus kolom numerik)
data_mean <- data
for (i in 1:ncol(data_mean)) {
  if (is.numeric(data_mean[[i]])) {
    data_mean[[i]][is.na(data_mean[[i]])] <- mean(data_mean[[i]], na.rm = TRUE)
  }
}
head(data_mean)

# Mengisi missing value dengan median
data_median <- data
for (i in 1:ncol(data_median)) {
  if (is.numeric(data_median[[i]])) {
    data_median[[i]][is.na(data_median[[i]])] <- median(data_median[[i]], na.rm = TRUE)
  }
}
head(data_median)

# Mengisi missing value dengan modus (untuk kolom kategori)
get_mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data_mode <- data
for (i in 1:ncol(data_mode)) {
  if (is.factor(data_mode[[i]]) | is.character(data_mode[[i]])) {
    mode_value <- get_mode(data_mode[[i]])
    data_mode[[i]][is.na(data_mode[[i]])] <- mode_value
  }
}
head(data_mode)

# ============================================================
# MISSING VALUE dengan tidyr::fill()
# ============================================================
library(dplyr)
library(tidyr)
library(forcats)

# Mengisi missing values pada kolom score & ranked
data_filled <- data1 %>%
  fill(score, ranked, .direction = "downup")
head(data_filled)

# Mengganti missing values dengan nilai tetap
data_replaced <- data %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(across(where(is.factor), ~fct_explicit_na(., na_level = "Tidak ada")))
head(data_replaced)

# ============================================================
# OUTLIER HANDLING
# ============================================================

# Contoh: mengidentifikasi outliers pada kolom score
Q1 <- quantile(data$score, 0.25, na.rm = TRUE)
Q3 <- quantile(data$score, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
IQR_value

# Menghapus outliers pada score
data_no_outliers <- data[data$score >= (Q1 - 1.5 * IQR_value) & 
                           data$score <= (Q3 + 1.5 * IQR_value), ]
head(data_no_outliers)

# Menghapus outliers untuk semua variabel numerik
data_bersih_outlier <- data %>%
  filter(if_all(where(is.numeric),
                ~ . >= (quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)) &
                  . <= (quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE))))
head(data_bersih_outlier)

# ============================================================
# DATA TRANSFORMATION
# ============================================================

# Konversi kolom episodes ke numerik
datatrans <- data1 %>%
  mutate(across(c(episodes), ~as.numeric(.), .names = "num_{.col}"))
head(datatrans)

# Membuat kolom baru, contoh: score per member
datagabung <- data1 %>%
  mutate(score_per_member = score / members)
head(datagabung)

# ============================================================
# DATA INTEGRATION (contoh)
# ============================================================

# dataset utama
data_main <- data.frame(
  id = 1:5,
  title = c("A", "B", "C", "D", "E"),
  score = c(7.5, 8.2, 6.9, 9.0, 8.8)
)

# dataset tambahan
data_tambah <- data.frame(
  id = 1:5,
  popularity = c(100, 200, 150, 300, 250)
)

# gabung berdasarkan kolom "id"
data_combined <- merge(data_main, data_tambah, by = "id")
data_combined
