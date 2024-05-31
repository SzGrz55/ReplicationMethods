#install.packages(c("moments", "normtest", "tseries", "MASS", "readxl"))
#install.packages("fitdistrplus")
library(moments)
#library(normtest)
library(tseries)
library(MASS)
library(readxl)
library(fitdistrplus)



# Etap 0 - wczytanie danych

# Wczytanie danych 
dane <- read_excel("MRwAD/dane.xlsx")

# Wyświetlenie kilku pierwszych wierszy danych
print(head(dane))




# Etap 1 - statystyki i wykresy

# Obliczenie statystyk opisowych dla każdej zmiennej
statystyki <- apply(dane[, -1], 2, function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE)
  )
})

# Przekształcenie wyników do ramki danych
statystyki_df <- as.data.frame(t(statystyki))
statystyki_df$variable <- rownames(statystyki_df)
statystyki_df <- statystyki_df[, c(6, 1:5)]
rownames(statystyki_df) <- NULL

# Wyświetlenie tabeli
print(statystyki_df)




# Narysowanie histogramów i wykresów gęstości dla każdej zmiennej
par(mfrow=c(3, 2))  
for (i in 2:ncol(dane)) {
  hist(dane[[i]], main=paste("Histogram of", colnames(dane)[i]), xlab=colnames(dane)[i])
  plot(density(dane[[i]]), main=paste("Density plot of", colnames(dane)[i]), xlab=colnames(dane)[i])
}








# Etap 2 - doposaowanie rozkładu do danych (test normalności, doposanowanie, przetestowanie z dobranym rozkładem)


# Przeprowadzenie testów zgodności z rozkładem normalnym
for (i in 2:ncol(dane)) {
  cat("\nTesting normality for", colnames(dane)[i], "\n")
  print(shapiro.test(dane[[i]]))
  print(ks.test(dane[[i]], "pnorm", mean=mean(dane[[i]], na.rm=TRUE), sd=sd(dane[[i]], na.rm=TRUE)))
  print(jarque.bera.test(dane[[i]]))
}




# Dopasowanie różnych rozkładów do zmiennej ludnosc_na_km_2


wybrana_zmienna <- dane$ludnosc_na_km_2

# Dopasowanie rozkładu gamma
fit_gamma <- fitdistr(wybrana_zmienna, "gamma")
print(fit_gamma)

# Dopasowanie rozkładu log-normalnego
fit_lognormal <- fitdistr(wybrana_zmienna, "lognormal")
print(fit_lognormal)

# Dopasowanie rozkładu t-Studenta (trzeba znormalizować dane)
fit_t <- fitdistr(wybrana_zmienna, "t")
print(fit_t)

# Dopasowanie rozkładu chi-kwadrat
fit_chisq <- fitdistr(wybrana_zmienna, "chi-squared", start=list(df=mean(wybrana_zmienna)))
print(fit_chisq)

# Dopasowanie rozkładu Weibull
fit_weibull <- fitdistr(wybrana_zmienna, "weibull")
print(fit_weibull)



# Testy Kołmogorowa-Smirnowa dla różnych rozkładów

# Gamma
ks_test_gamma <- ks.test(wybrana_zmienna, "pgamma", shape=fit_gamma$estimate[1], rate=fit_gamma$estimate[2])
print(ks_test_gamma)

# Log-normalny
ks_test_lognormal <- ks.test(wybrana_zmienna, "plnorm", meanlog=fit_lognormal$estimate[1], sdlog=fit_lognormal$estimate[2])
print(ks_test_lognormal)

# t-Studenta
ks_test_t <- ks.test(wybrana_zmienna, "pt", df=fit_t$estimate[3], ncp=0)
print(ks_test_t)

# Chi-kwadrat
ks_test_chisq <- ks.test(wybrana_zmienna, "pchisq", df=fit_chisq$estimate[1])
print(ks_test_chisq)

# Weibull
ks_test_weibull <- ks.test(wybrana_zmienna, "pweibull", shape=fit_weibull$estimate[1], scale=fit_weibull$estimate[2])
print(ks_test_weibull)









# Etap 3 - tworzymy empirczyne wartości krytyczne przy pomocy Monte Carlo

# Parametry rozkładu log-normalnego
meanlog <- fit_lognormal$estimate[1]
sdlog <- fit_lognormal$estimate[2]

# Podzielenie danych na dwie podgrupy na podstawie mediany ceny za m2
subset1 <- dane$ludnosc_na_km_2[dane$cena_za_m2 > median(dane$cena_za_m2)]
subset2 <- dane$ludnosc_na_km_2[dane$cena_za_m2 <= median(dane$cena_za_m2)]

n1 <- length(subset1)
n2 <- length(subset2)
t_stats <- numeric(10000)

# Symulacja Monte Carlo
set.seed(123)  
for (i in 1:10000) {
  sample1 <- rlnorm(n1, meanlog, sdlog)
  sample2 <- rlnorm(n2, meanlog, sdlog)
  t_test <- t.test(sample1, sample2)
  t_stats[i] <- t_test$statistic
}

# Wyznaczenie empirycznej wartości krytycznej
alpha <- 0.05
empirical_critical_value <- quantile(t_stats, 1 - alpha)
theoretical_critical_value <- qt(1 - alpha, df=n1 + n2 - 2)

result <- list(empirical = empirical_critical_value, theoretical = theoretical_critical_value)
print(result)

# Podsumowanie wyników i porównanie wartości krytycznych
cat("Empiryczna wartość krytyczna:", result$empirical, "\n")
cat("Teoretyczna wartość krytyczna:", result$theoretical, "\n")






# Etap 4 - test t-studenta dla naszych danych i weryfikacja czy ma znaczenie którą wartość krytyczną przyjmiemy

# Przeprowadzenie testu równości dwóch wartości oczekiwanych dla rzeczywistych danych
test_result <- t.test(subset1, subset2)
cat("Statystyka tesotwa:", test_result$statistic, "\n")
cat("p-value:", test_result$p.value, "\n")


# Wnioski: w naszym przypadku nie ma znaczenia czy weźmiemy wartość krytyczną teoretyczną czy empiryczną, albowiem w jednym i drugim przypadku odrzucamy H0.


