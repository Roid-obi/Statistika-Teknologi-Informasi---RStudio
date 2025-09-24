# =====================================
# PERCOBAAN OPERASI VEKTOR & DATA FRAME
# Dataset: anime_entries.csv
# =====================================

# 1. IMPORT DATA
df <- read.csv("C:/Users/User/Downloads/anime_entries.csv",
               stringsAsFactors = FALSE)

# 2. CEK NAMA KOLOM
cat("\n===== Nama Kolom Dataset =====\n")
print(colnames(df))

# 3. AMBIL BEBERAPA VEKTOR
judul <- df$title_name[1:5]   # judul anime
tipe <- df$item_type[1:5]     # tipe (TV, Movie, OVA, dll)
rating <- df$score[1:5]       # skor rating
episodes <- df$episodes[1:5]  # jumlah episode

# 4. CETAK VEKTOR
cat("\n===== Vektor Judul =====\n")
print(judul)

cat("\n===== Vektor Tipe =====\n")
print(tipe)

cat("\n===== Vektor Rating =====\n")
print(rating)

cat("\n===== Vektor Episodes =====\n")
print(episodes)

# 5. GABUNGKAN KE DATA FRAME SEDERHANA
anime_simple <- data.frame(
  Judul = judul,
  Tipe = tipe,
  Rating = rating,
  Episodes = episodes
)

cat("\n===== Data Frame Sederhana =====\n")
print(anime_simple)
