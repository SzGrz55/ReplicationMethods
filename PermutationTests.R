if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(readxl)
library(dplyr)
library(e1071)

# Krok 1: Wczytanie danych
file_path <- "dane.xlsx"  
data <- read_excel(file_path)

# Wyświetlenie nagłówków danych
print(names(data))

# Filtracja wierszy dla odpowiednich województw
data_malopolskie <- data[2:23, 2:5]  
data_mazowieckie <- data[24:65, 2:5]  

# Konwersja na typ numeric
data_malopolskie <- data_malopolskie %>% mutate(across(everything(), as.numeric))
data_mazowieckie <- data_mazowieckie %>% mutate(across(everything(), as.numeric))

# Usunięcie wierszy z wartościami NA
data_malopolskie <- na.omit(data_malopolskie)
data_mazowieckie <- na.omit(data_mazowieckie)

# Sprawdzenie danych wejściowych
print(summary(data_malopolskie))
print(summary(data_mazowieckie))

# Krok 2: Przekształcenie danych - w excelu

# Krok 3: Funkcje pomocnicze do testów permutacyjnych
manual_perm_test <- function(x, y, stat = "mean", n_perm = 1000) {
  if (any(is.na(x)) || any(is.na(y))) {
    print("NA values found in input data")
    return(NA)
  }
  
  # Obliczenie różnicy obserwowanej
  observed_diff <- switch(stat,
                          mean = mean(x) - mean(y),
                          var = var(x) - var(y),
                          skew = skewness(x) - skewness(y),
                          kurtosis = kurtosis(x) - kurtosis(y))
  
  perm_diffs <- numeric(n_perm)
  pooled <- c(x, y)
  n_x <- length(x)
  n_y <- length(y)
  
  for (i in 1:n_perm) {
    perm <- sample(pooled)
    group1 <- perm[1:n_x]
    group2 <- perm[(n_x + 1):(n_x + n_y)]
    perm_diffs[i] <- switch(stat,
                            mean = mean(group1) - mean(group2),
                            var = var(group1) - var(group2),
                            skew = skewness(group1) - skewness(group2),
                            kurtosis = kurtosis(group1) - kurtosis(group2))
  }
  
  p_value <- mean(abs(perm_diffs) >= abs(observed_diff))
  return(p_value)
}

# Testowanie funkcji permutacyjnej na przykładowych danych
example_x <- data_malopolskie$cena_za_m2
example_y <- data_mazowieckie$cena_za_m2

print(manual_perm_test(example_x, example_y, stat = "mean"))

# Krok 4: Testy permutacyjne i klasyczne dla każdej zmiennej
results <- data.frame(
  variable = c("cena_za_m2", "wynagrodzenie", "ludnosc_na_km_2", "szkoly_na_1000"),
  p_mean_perm = c(
    manual_perm_test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2, stat = "mean"),
    manual_perm_test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie, stat = "mean"),
    manual_perm_test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2, stat = "mean"),
    manual_perm_test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000, stat = "mean")
  ),
  p_var_perm = c(
    manual_perm_test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2, stat = "var"),
    manual_perm_test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie, stat = "var"),
    manual_perm_test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2, stat = "var"),
    manual_perm_test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000, stat = "var")
  ),
  p_skew_perm = c(
    manual_perm_test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2, stat = "skew"),
    manual_perm_test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie, stat = "skew"),
    manual_perm_test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2, stat = "skew"),
    manual_perm_test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000, stat = "skew")
  ),
  p_kurtosis_perm = c(
    manual_perm_test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2, stat = "kurtosis"),
    manual_perm_test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie, stat = "kurtosis"),
    manual_perm_test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2, stat = "kurtosis"),
    manual_perm_test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000, stat = "kurtosis")
  ),
  p_mean_ttest = c(
    t.test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2)$p.value,
    t.test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie)$p.value,
    t.test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2)$p.value,
    t.test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000)$p.value
  ),
  p_var_f_test = c(
    var.test(data_malopolskie$cena_za_m2, data_mazowieckie$cena_za_m2)$p.value,
    var.test(data_malopolskie$wynagrodzenie, data_mazowieckie$wynagrodzenie)$p.value,
    var.test(data_malopolskie$ludnosc_na_km_2, data_mazowieckie$ludnosc_na_km_2)$p.value,
    var.test(data_malopolskie$szkoly_na_1000, data_mazowieckie$szkoly_na_1000)$p.value
  )
)

# Krok 5: Testy korelacji
manual_cor_test <- function(x, y, n_perm = 1000) {
  if (any(is.na(x)) || any(is.na(y))) {
    print("NA values found in input data")
    return(NA)
  }
  observed_cor <- cor(x, y)
  perm_cors <- numeric(n_perm)
  
  for (i in 1:n_perm) {
    perm_x <- sample(x)
    perm_cors[i] <- cor(perm_x, y)
  }
  
  p_value <- mean(abs(perm_cors) >= abs(observed_cor))
  return(p_value)
}

# Testy korelacji dla Małopolskiego
cor_results <- data.frame(
  pair = c("cena_za_m2 ~ wynagrodzenie", "cena_za_m2 ~ ludnosc_na_km_2", "cena_za_m2 ~ szkoly_na_1000", 
           "wynagrodzenie ~ ludnosc_na_km_2", "wynagrodzenie ~ szkoly_na_1000", "ludnosc_na_km_2 ~ szkoly_na_1000"),
  p_perm = c(
    manual_cor_test(data_malopolskie$cena_za_m2, data_malopolskie$wynagrodzenie),
    manual_cor_test(data_malopolskie$cena_za_m2, data_malopolskie$ludnosc_na_km_2),
    manual_cor_test(data_malopolskie$cena_za_m2, data_malopolskie$szkoly_na_1000),
    manual_cor_test(data_malopolskie$wynagrodzenie, data_malopolskie$ludnosc_na_km_2),
    manual_cor_test(data_malopolskie$wynagrodzenie, data_malopolskie$szkoly_na_1000),
    manual_cor_test(data_malopolskie$ludnosc_na_km_2, data_malopolskie$szkoly_na_1000)
  ),
  p_cor_test = c(
    cor.test(data_malopolskie$cena_za_m2, data_malopolskie$wynagrodzenie)$p.value,
    cor.test(data_malopolskie$cena_za_m2, data_malopolskie$ludnosc_na_km_2)$p.value,
    cor.test(data_malopolskie$cena_za_m2, data_malopolskie$szkoly_na_1000)$p.value,
    cor.test(data_malopolskie$wynagrodzenie, data_malopolskie$ludnosc_na_km_2)$p.value,
    cor.test(data_malopolskie$wynagrodzenie, data_malopolskie$szkoly_na_1000)$p.value,
    cor.test(data_malopolskie$ludnosc_na_km_2, data_malopolskie$szkoly_na_1000)$p.value
  )
)

# Krok 6: Wyświetlenie wyników
print(results)
print(cor_results)
