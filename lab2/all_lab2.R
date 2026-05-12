if (!require("tidyverse")) install.packages("tidyverse")
if (!require("boot"))      install.packages("boot")

library(tidyverse)
library(boot)
library(dplyr)

df <- read.csv("cleaned_df.csv", stringsAsFactors = FALSE)

set.seed(42)

cat(sprintf("Датасет: %d рядків x %d стовпців\n\n", nrow(df), ncol(df)))


z <- qnorm(0.975)

manual_ci_mean <- function(data_vector, group_name) {
  data_vector <- na.omit(data_vector)
  n <- length(data_vector)
  
  if (n == 0) return(NULL)
  
  mean_val <- mean(data_vector)
  se <- sd(data_vector) / sqrt(n)
  
  lower <- mean_val - z * se
  upper <- mean_val + z * se
  
  cat(sprintf("%-30s | N = %-6d | Mean = %8.4f | SE = %.6f | 95%% CI = [%.4f; %.4f]\n",
              group_name, n, mean_val, se, lower, upper))
}

manual_ci_prop <- function(successes, n, group_name) {
  p_hat <- successes / n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  lower <- p_hat - z * se
  upper <- p_hat + z * se
  
  cat(sprintf("%-30s | N = %-6d | Prop = %8.4f | SE = %.6f | 95%% CI = [%.4f; %.4f]\n",
              group_name, n, p_hat, se, lower, upper))
}

cat("=== ГРУПА 1: Базові оцінки характеристик (Точкові оцінки та ДІ) ===\n")

df$is_answered_logical <- as.logical(df$is_answered)
total_answered <- sum(df$is_answered_logical, na.rm = TRUE)
total_n <- sum(!is.na(df$is_answered_logical))

manual_ci_prop(total_answered, total_n, "1. Частка is_answered")

manual_ci_mean(df$difficulty_score, "2. Оцінка складності")
manual_ci_mean(df$quality_score, "3. Оцінка якості")
manual_ci_mean(df$score, "4. Рейтинг запитання")
manual_ci_mean(df$accepted_answer_score, "5. Оцінка прийнятої відповіді")
manual_ci_mean(df$top_answer_score, "6. Оцінка топової відповіді")
cat("\n")

cat("=== ГРУПА 2: Перевірка дослідницьких гіпотез (підсилення EDA) ===\n")

cat("\n--- Гіпотеза 1: Рейтинг (Score) за наявністю коду ---\n")
manual_ci_mean(df$score[df$has_code == TRUE], "З кодом (TRUE)")
manual_ci_mean(df$score[df$has_code == FALSE], "Без коду (FALSE)")

cat("\n--- Гіпотеза 2: Частка відповідей 2021 vs 2024 ---\n")
df_2021 <- df %>% filter(creation_year == 2021 & !is.na(is_answered_logical))
manual_ci_prop(sum(df_2021$is_answered_logical), nrow(df_2021), "2021 рік")

df_2024 <- df %>% filter(creation_year == 2024 & !is.na(is_answered_logical))
manual_ci_prop(sum(df_2024$is_answered_logical), nrow(df_2024), "2024 рік")

cat("\n--- Гіпотеза 3: Вплив довжини запитання на Score ---\n")
manual_ci_mean(df$score[df$body_word_count < 100], "Короткі (<100)")
manual_ci_mean(df$score[df$body_word_count >= 100 & df$body_word_count <= 200], "Оптимальні (100-200)")
manual_ci_mean(df$score[df$body_word_count > 200], "Довгі (>200)")

cat("\n=== Гіпотеза 4: Якість (Quality) у ТОП-5 мовах ===\n")
top_langs <- c("c++", "python", "c#", "javascript", "java")
for (lang in top_langs) {
  manual_ci_mean(df$quality_score[df$programming_language == lang], lang)
}

cat("\n--- Гіпотеза 5: Вплив ChatGPT на мови (Python vs Rust) ---\n")
df$chatgpt_era <- ifelse(as.Date(df$creation_date) >= as.Date("2022-12-01"), "Post-ChatGPT", "Pre-ChatGPT")

manual_ci_mean(df$quality_score[df$programming_language == "python" & df$chatgpt_era == "Pre-ChatGPT"], "Python (До ШІ)")
manual_ci_mean(df$quality_score[df$programming_language == "python" & df$chatgpt_era == "Post-ChatGPT"], "Python (Після ШІ)")
manual_ci_mean(df$quality_score[df$programming_language == "rust"   & df$chatgpt_era == "Post-ChatGPT"], "Rust (Після ШІ)")
cat("\n")


resp_time <- na.omit(df$first_response_time_hours)

median_func <- function(data, indices) {
  return(median(data[indices]))
}

boot_median <- boot(data = resp_time, statistic = median_func, R = 2000)

boot_ci_median <- boot.ci(boot_median, type = "perc")
print("ДІ для медіанного часу відповіді")
print(boot_ci_median)

cor_func <- function(data, indices) {
  d <- data[indices, ]
  r_pearson <- cor(d$view_count, d$score, method = "pearson")
  
  var_r <- ((1 - r_pearson^2)^2) / (nrow(d) - 3)
  
  return(c(r_pearson, var_r))
}

df_cor <- df %>% select(view_count, score) %>% drop_na()

boot_cor <- boot(data = df_cor, statistic = cor_func, R = 2000)

boot_ci_cor <- boot.ci(boot_cor, type = c("norm", "basic", "perc", "stud"))
print("ДІ для кореляції Перегляди vs Оцінка")
print(boot_ci_cor)


words_count <- na.omit(df$body_word_count)

boot_median_words <- boot(data = words_count, statistic = median_func, R = 2000)
boot_ci_median_words <- boot.ci(boot_median_words, type = "perc")
print("ДІ для медіанної довжини запитання")
print(boot_ci_median_words)

cor_score_top_func <- function(data, indices) {
  d <- data[indices, ]
  r_p <- cor(d$score, d$top_answer_score, method = "pearson")
  var_r <- ((1 - r_p^2)^2) / (nrow(d) - 3)
  return(c(r_p, var_r))
}

cor_rep_diff_func <- function(data, indices) {
  d <- data[indices, ]
  r_p <- cor(d$owner_reputation, d$difficulty_score, method = "pearson")
  var_r <- ((1 - r_p^2)^2) / (nrow(d) - 3)
  return(c(r_p, var_r))
}

df_scores <- df %>% select(score, top_answer_score) %>% drop_na()
boot_cor_scores <- boot(data = df_scores, statistic = cor_score_top_func, R = 2000)
boot_ci_scores <- boot.ci(boot_cor_scores, type = c("norm", "basic", "perc", "stud"))
print("ДІ для кореляції Оцінка vs Оцінка топ-відповіді")
print(boot_ci_scores)

df_rep <- df %>% select(owner_reputation, difficulty_score) %>% drop_na()
boot_cor_rep <- boot(data = df_rep, statistic = cor_rep_diff_func, R = 2000)
boot_ci_rep <- boot.ci(boot_cor_rep, type = c("norm", "basic", "perc", "stud"))
print("ДІ для кореляції Репутація vs Складність")
print(boot_ci_rep)


views <- na.omit(df$view_count)
boot_median_views <- boot(data = views, statistic = median_func, R = 2000)
print("ДІ для медіани Переглядів")
print(boot.ci(boot_median_views, type = "perc"))

reputation <- na.omit(df$owner_reputation)
boot_median_rep <- boot(data = reputation, statistic = median_func, R = 2000)
print("ДІ для медіани Репутації")
print(boot.ci(boot_median_rep, type = "perc"))


cor_views_diff_func <- function(data, indices) {
  d <- data[indices, ]
  r_p <- cor(d$view_count, d$difficulty_score, method = "pearson")
  var_r <- ((1 - r_p^2)^2) / (nrow(d) - 3)
  return(c(r_p, var_r))
}

cor_ans_topscore_func <- function(data, indices) {
  d <- data[indices, ]
  r_p <- cor(d$answer_count, d$top_answer_score, method = "pearson")
  var_r <- ((1 - r_p^2)^2) / (nrow(d) - 3)
  return(c(r_p, var_r))
}

df_views_diff <- df %>% select(view_count, difficulty_score) %>% drop_na()
boot_cor_views_diff <- boot(data = df_views_diff, statistic = cor_views_diff_func, R = 2000)
print("ДІ для кореляції Перегляди vs Складність")
print(boot.ci(boot_cor_views_diff, type = c("norm", "basic", "perc", "stud")))

df_ans_topscore <- df %>% select(answer_count, top_answer_score) %>% drop_na()
boot_cor_ans_topscore <- boot(data = df_ans_topscore, statistic = cor_ans_topscore_func, R = 2000)
print("ДІ для кореляції К-сть відповідей vs Оцінка топ-відповіді")
print(boot.ci(boot_cor_ans_topscore, type = c("norm", "basic", "perc", "stud")))


cat("Розмір датасету:", nrow(df), "x", ncol(df), "\n\n")

wald_test <- function(theta_hat, theta_0, se, alternative = "two.sided") {
  z <- (theta_hat - theta_0) / se
  p <- switch(alternative,
              "two.sided" = 2 * (1 - pnorm(abs(z))),
              "greater"   = 1 - pnorm(z),
              "less"      = pnorm(z))
  list(z = z, p = p)
}

print_result <- function(name, n_info, theta_hat, se, z, p, alt, decision_threshold = 0.05) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(name, "\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat(n_info, "\n")
  cat(sprintf("Точкова оцінка = %.5f,  SE = %.6f\n", theta_hat, se))
  cat(sprintf("Альтернатива: %s\n", alt))
  cat(sprintf("Z-статистика = %.4f\n", z))
  cat(sprintf("p-value = %.4e\n", p))
  cat(sprintf("Рішення на рівні α = %.2f: %s\n",
              decision_threshold,
              ifelse(p < decision_threshold, "ВІДКИДАЄМО H0", "НЕ ВІДКИДАЄМО H0")))
}

p_values <- list()


n1 <- nrow(df)
p_hat1 <- mean(df$is_answered)
se_h0_1 <- sqrt(0.5 * 0.5 / n1)
res1 <- wald_test(p_hat1, 0.5, se_h0_1, "greater")

print_result(
  name = "H1: Частка is_answered перевищує 0.5",
  n_info = sprintf("n = %d, p_hat = %.5f", n1, p_hat1),
  theta_hat = p_hat1, se = se_h0_1,
  z = res1$z, p = res1$p, alt = "p > 0.5 (одностороння)"
)
p_values$H1 <- res1$p


py <- df$quality_score[df$programming_language == "python"]
py <- py[!is.na(py)]
ja <- df$quality_score[df$programming_language == "java"]
ja <- ja[!is.na(ja)]

n_py <- length(py); n_ja <- length(ja)
m_py <- mean(py);   m_ja <- mean(ja)
v_py <- var(py);    v_ja <- var(ja)
diff2 <- m_py - m_ja
se2 <- sqrt(v_py / n_py + v_ja / n_ja)
res2 <- wald_test(diff2, 0, se2, "greater")

print_result(
  name = "H2: Середня quality_score Python > Java",
  n_info = sprintf("Python: n=%d, mean=%.5f, var=%.6f\nJava:   n=%d, mean=%.5f, var=%.6f\nРізниця = %.5f",
                   n_py, m_py, v_py, n_ja, m_ja, v_ja, diff2),
  theta_hat = diff2, se = se2,
  z = res2$z, p = res2$p, alt = "μ_Py > μ_Ja (одностороння)"
)
p_values$H2 <- res2$p


s_kod <- df$score[df$has_code == TRUE]
s_kod <- s_kod[!is.na(s_kod)]
s_nok <- df$score[df$has_code == FALSE]
s_nok <- s_nok[!is.na(s_nok)]

n_k <- length(s_kod); n_nk <- length(s_nok)
m_k <- mean(s_kod);   m_nk <- mean(s_nok)
v_k <- var(s_kod);    v_nk <- var(s_nok)
diff3 <- m_k - m_nk
se3 <- sqrt(v_k / n_k + v_nk / n_nk)
res3 <- wald_test(diff3, 0, se3, "greater")

print_result(
  name = "H3: Середня score (з кодом) > середня score (без коду)",
  n_info = sprintf("З кодом:  n=%d, mean=%.4f, var=%.3f\nБез коду: n=%d, mean=%.4f, var=%.3f\nРізниця = %.4f",
                   n_k, m_k, v_k, n_nk, m_nk, v_nk, diff3),
  theta_hat = diff3, se = se3,
  z = res3$z, p = res3$p, alt = "μ_код > μ_безкод (одностороння)"
)
p_values$H3 <- res3$p


buden <- df$body_word_count[df$creation_weekday %in% c(0,1,2,3,4)]
buden <- buden[!is.na(buden)]
vykh <- df$body_word_count[df$creation_weekday %in% c(5,6)]
vykh <- vykh[!is.na(vykh)]

n_b <- length(buden); n_v <- length(vykh)
m_b <- mean(buden);   m_v <- mean(vykh)
v_b <- var(buden);    v_v <- var(vykh)
diff4 <- m_b - m_v
se4 <- sqrt(v_b / n_b + v_v / n_v)
res4 <- wald_test(diff4, 0, se4, "two.sided")

print_result(
  name = "H4: Різниця body_word_count між буднями та вихідними",
  n_info = sprintf("Будні:   n=%d, mean=%.3f, var=%.2f\nВихідні: n=%d, mean=%.3f, var=%.2f\nРізниця = %.4f",
                   n_b, m_b, v_b, n_v, m_v, v_v, diff4),
  theta_hat = diff4, se = se4,
  z = res4$z, p = res4$p, alt = "μ_буд ≠ μ_вих (двостороння)"
)
p_values$H4 <- res4$p


syst_langs <- c("c", "c++", "rust")
mass_langs <- c("python", "javascript")

syst <- df$has_code[df$programming_language %in% syst_langs]
mass <- df$has_code[df$programming_language %in% mass_langs]

n_s <- length(syst); n_m <- length(mass)
p_s <- mean(syst);   p_m <- mean(mass)
p_pool5 <- (sum(syst) + sum(mass)) / (n_s + n_m)
se5 <- sqrt(p_pool5 * (1 - p_pool5) * (1/n_s + 1/n_m))
diff5 <- p_s - p_m
res5 <- wald_test(diff5, 0, se5, "greater")

print_result(
  name = "H5: Частка з кодом у системних мовах > у масових",
  n_info = sprintf("Системні (C, C++, Rust): n=%d, p_hat=%.5f\nМасові (Py, JS):          n=%d, p_hat=%.5f\nPooled p_hat = %.5f, Різниця = %.5f",
                   n_s, p_s, n_m, p_m, p_pool5, diff5),
  theta_hat = diff5, se = se5,
  z = res5$z, p = res5$p, alt = "p_сист > p_масов (одностороння)"
)
p_values$H5 <- res5$p


ct <- table(df$has_code, df$is_answered)
cat("\nТаблиця спряженості has_code x is_answered:\n")
print(addmargins(ct))

g_kod <- df$is_answered[df$has_code == TRUE]
g_nok <- df$is_answered[df$has_code == FALSE]
n_k6 <- length(g_kod); n_nk6 <- length(g_nok)
p_k6 <- mean(g_kod);   p_nk6 <- mean(g_nok)
p_pool6 <- (sum(g_kod) + sum(g_nok)) / (n_k6 + n_nk6)
se6 <- sqrt(p_pool6 * (1 - p_pool6) * (1/n_k6 + 1/n_nk6))
diff6 <- p_k6 - p_nk6
res6 <- wald_test(diff6, 0, se6, "two.sided")

print_result(
  name = "H6: Незалежність has_code та is_answered",
  n_info = sprintf("p(відп|код)    = %.5f, n=%d\np(відп|безкод) = %.5f, n=%d\nPooled p_hat   = %.5f, Різниця = %.5f",
                   p_k6, n_k6, p_nk6, n_nk6, p_pool6, diff6),
  theta_hat = diff6, se = se6,
  z = res6$z, p = res6$p, alt = "≠ (двостороння)"
)
p_values$H6 <- res6$p


cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("ЗВЕДЕНА ТАБЛИЦЯ p-значень — для Учасниці 4 (множинне тестування)\n")
cat(strrep("=", 70), "\n\n", sep = "")

p_vec <- unlist(p_values)
print(data.frame(
  hypothesis = names(p_vec),
  p_value    = formatC(p_vec, format = "e", digits = 4),
  reject_H0_uncorrected = ifelse(p_vec < 0.05, "ТАК", "НІ"),
  row.names = NULL
))

saveRDS(p_values, file = "p_values_participant3.rds")
cat("\np-значення збережено у файл: p_values_participant3.rds\n")


cat(strrep("-", 70), "\n")
cat("БЛОК 1. Тести Волда для медіан (SE від бутстрепу, B = 2000)\n")
cat(strrep("-", 70), "\n\n")

boot_median_stat <- function(data, indices) median(data[indices])

wald_test_median <- function(theta_hat, theta_0, se, alternative = "two.sided") {
  z_stat <- (theta_hat - theta_0) / se
  p_val  <- switch(alternative,
                   "two.sided" = 2 * (1 - pnorm(abs(z_stat))),
                   "greater"   = 1 - pnorm(z_stat),
                   "less"      = pnorm(z_stat)
  )
  list(z = z_stat, p = p_val)
}

print_wald <- function(label, n, median_hat, median_0, se_boot, z, p, alt,
                       alpha = 0.05) {
  cat(sprintf("  Гіпотеза: %s\n", label))
  cat(sprintf("  n = %d  |  Оцінка медіани = %.4f  |  H0: медіана = %.4f\n",
              n, median_hat, median_0))
  cat(sprintf("  SE (bootstrap) = %.5f\n", se_boot))
  cat(sprintf("  Альтернатива: %s\n", alt))
  cat(sprintf("  Z = %.4f  |  p-value = %.4e\n", z, p))
  cat(sprintf("  Рішення (α = %.2f): %s\n\n",
              alpha,
              ifelse(p < alpha, ">>> ВІДКИДАЄМО H0 <<<", "не відкидаємо H0")))
}

p_wald_medians <- numeric()


resp_time_m <- na.omit(df$first_response_time_hours)
boot_rt   <- boot(data = resp_time_m, statistic = boot_median_stat, R = 2000)
m1_hat    <- median(resp_time_m)
m1_se     <- sd(boot_rt$t[, 1])
res_m1    <- wald_test_median(m1_hat, theta_0 = 3, m1_se, alternative = "less")

cat("M1. Медіана часу відповіді\n")
print_wald(
  label      = "median(first_response_time_hours) < 3 год",
  n          = length(resp_time_m),
  median_hat = m1_hat,
  median_0   = 3,
  se_boot    = m1_se,
  z          = res_m1$z,
  p          = res_m1$p,
  alt        = "median < 3 (одностороння)"
)
p_wald_medians["M1_resp_time"] <- res_m1$p


words_m  <- na.omit(df$body_word_count)
boot_w <- boot(data = words_m, statistic = boot_median_stat, R = 2000)
m2_hat <- median(words_m)
m2_se  <- sd(boot_w$t[, 1])
res_m2 <- wald_test_median(m2_hat, theta_0 = 100, m2_se, alternative = "two.sided")

cat("M2. Медіана довжини запитання\n")
print_wald(
  label      = "median(body_word_count) != 100",
  n          = length(words_m),
  median_hat = m2_hat,
  median_0   = 100,
  se_boot    = m2_se,
  z          = res_m2$z,
  p          = res_m2$p,
  alt        = "median != 100 (двостороння)"
)
p_wald_medians["M2_word_count"] <- res_m2$p


views_m  <- na.omit(df$view_count)
boot_v <- boot(data = views_m, statistic = boot_median_stat, R = 2000)
m3_hat <- median(views_m)
m3_se  <- sd(boot_v$t[, 1])
res_m3 <- wald_test_median(m3_hat, theta_0 = 100, m3_se, alternative = "two.sided")

cat("M3. Медіана кількості переглядів\n")
print_wald(
  label      = "median(view_count) != 100",
  n          = length(views_m),
  median_hat = m3_hat,
  median_0   = 100,
  se_boot    = m3_se,
  z          = res_m3$z,
  p          = res_m3$p,
  alt        = "median != 100 (двостороння)"
)
p_wald_medians["M3_view_count"] <- res_m3$p


rep_vec <- na.omit(df$owner_reputation)
boot_r  <- boot(data = rep_vec, statistic = boot_median_stat, R = 2000)
m4_hat  <- median(rep_vec)
m4_se   <- sd(boot_r$t[, 1])
res_m4  <- wald_test_median(m4_hat, theta_0 = 100, m4_se, alternative = "less")

cat("M4. Медіана репутації автора\n")
print_wald(
  label      = "median(owner_reputation) < 100",
  n          = length(rep_vec),
  median_hat = m4_hat,
  median_0   = 100,
  se_boot    = m4_se,
  z          = res_m4$z,
  p          = res_m4$p,
  alt        = "median < 100 (одностороння)"
)
p_wald_medians["M4_reputation"] <- res_m4$p


cat(strrep("-", 70), "\n")
cat("БЛОК 2. Збір p-значень від усіх учасників\n")
cat(strrep("-", 70), "\n\n")


p_participant3 <- c(
  H1_is_answered_gt_half    = 1.75e-07,
  H2_quality_python_gt_java = 2.22e-44,
  H3_score_code_gt_nocode   = 2.2e-16,
  H4_words_weekday_vs_wknd  = 0.9258,
  H5_code_syst_gt_mass      = 3.76e-18,
  H6_hascode_isanswered_ind = 1.29e-14
)


se_from_perc_ci <- function(lower, upper) (upper - lower) / (2 * 1.96)

r_score_top   <- 0.658;  se_score_top   <- se_from_perc_ci(0.5748,  0.7222)
r_rep_diff    <- -0.579; se_rep_diff    <- se_from_perc_ci(-0.6058, -0.5527)
r_views_score <- 0.353;  se_views_score <- se_from_perc_ci(0.1690,  0.5375)
r_views_diff  <- 0.373;  se_views_diff  <- se_from_perc_ci(0.3118,  0.4338)
r_ans_top     <- 0.383;  se_ans_top     <- se_from_perc_ci(0.3521,  0.4143)

p_participant2 <- c(
  C1_score_vs_top_ans_score   = 2 * (1 - pnorm(abs(r_score_top   / se_score_top))),
  C2_reputation_vs_difficulty = 2 * (1 - pnorm(abs(r_rep_diff    / se_rep_diff))),
  C3_views_vs_score           = 2 * (1 - pnorm(abs(r_views_score / se_views_score))),
  C4_views_vs_difficulty      = 2 * (1 - pnorm(abs(r_views_diff  / se_views_diff))),
  C5_answers_vs_top_score     = 2 * (1 - pnorm(abs(r_ans_top     / se_ans_top)))
)

cat("p-значення Учасника 2 (відновлені з бутстреп-ДІ):\n")
for (nm in names(p_participant2))
  cat(sprintf("  %-35s : %.4e\n", nm, p_participant2[nm]))


s_code  <- na.omit(df$score[df$has_code == TRUE])
s_noc   <- na.omit(df$score[df$has_code == FALSE])
d1_hat  <- mean(s_code) - mean(s_noc)
se_d1   <- sqrt(var(s_code)/length(s_code) + var(s_noc)/length(s_noc))
p_u1_h1 <- 2 * (1 - pnorm(abs(d1_hat / se_d1)))

df21    <- subset(df, creation_year == 2021 & !is.na(is_answered))
df24    <- subset(df, creation_year == 2024 & !is.na(is_answered))
p21     <- mean(as.logical(df21$is_answered)); n21 <- nrow(df21)
p24     <- mean(as.logical(df24$is_answered)); n24 <- nrow(df24)
pp      <- (sum(as.logical(df21$is_answered)) + sum(as.logical(df24$is_answered))) / (n21 + n24)
se_d2   <- sqrt(pp * (1 - pp) * (1/n21 + 1/n24))
p_u1_h2 <- 2 * (1 - pnorm(abs((p21 - p24) / se_d2)))

s_opt   <- na.omit(df$score[df$body_word_count >= 100 & df$body_word_count <= 200])
s_long  <- na.omit(df$score[df$body_word_count > 200])
d3_hat  <- mean(s_opt) - mean(s_long)
se_d3   <- sqrt(var(s_opt)/length(s_opt) + var(s_long)/length(s_long))
p_u1_h3 <- 2 * (1 - pnorm(abs(d3_hat / se_d3)))

df$chatgpt_era <- ifelse(as.Date(df$creation_date) >= as.Date("2022-12-01"), "Post", "Pre")
py_pre  <- na.omit(df$quality_score[df$programming_language == "python" & df$chatgpt_era == "Pre"])
py_post <- na.omit(df$quality_score[df$programming_language == "python" & df$chatgpt_era == "Post"])
d5_hat  <- mean(py_pre) - mean(py_post)
se_d5   <- sqrt(var(py_pre)/length(py_pre) + var(py_post)/length(py_post))
p_u1_h5 <- 2 * (1 - pnorm(abs(d5_hat / se_d5)))

p_participant1 <- c(
  U1_H1_score_code_vs_nocode  = p_u1_h1,
  U1_H2_answered_2021_vs_2024 = p_u1_h2,
  U1_H3_score_optimal_vs_long = p_u1_h3,
  U1_H5_quality_pre_post_gpt  = p_u1_h5
)

cat("\np-значення Учасника 1 (обчислені на льоту):\n")
for (nm in names(p_participant1))
  cat(sprintf("  %-35s : %.4e\n", nm, p_participant1[nm]))

cat("\np-значення Учасника 4 (Wald для медіан):\n")
for (nm in names(p_wald_medians))
  cat(sprintf("  %-35s : %.4e\n", nm, p_wald_medians[nm]))


cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("БЛОК 3. МНОЖИННЕ ТЕСТУВАННЯ\n")
cat(strrep("=", 70), "\n\n")

all_p    <- c(p_participant3, p_participant2, p_participant1, p_wald_medians)
k_total  <- length(all_p)

cat(sprintf("Загальна кількість гіпотез: k = %d\n", k_total))
cat(sprintf("FWER без корекції (α=0.05): 1 - 0.95^%d = %.3f\n\n",
            k_total, 1 - 0.95^k_total))

p_bonferroni <- p.adjust(all_p, method = "bonferroni")
p_holm       <- p.adjust(all_p, method = "holm")
p_bh         <- p.adjust(all_p, method = "BH")

results_df <- data.frame(
  Гіпотеза      = names(all_p),
  p_raw         = all_p,
  p_Bonferroni  = p_bonferroni,
  p_Holm        = p_holm,
  p_BH          = p_bh,
  Відх_raw      = ifelse(all_p        < 0.05, "ТАК", "НІ"),
  Відх_Bonf     = ifelse(p_bonferroni < 0.05, "ТАК", "НІ"),
  Відх_Holm     = ifelse(p_holm       < 0.05, "ТАК", "НІ"),
  Відх_BH       = ifelse(p_bh         < 0.05, "ТАК", "НІ"),
  row.names = NULL,
  stringsAsFactors = FALSE
)

cat(strrep("-", 70), "\n")
cat("ЗВЕДЕНА ТАБЛИЦЯ РЕЗУЛЬТАТІВ МНОЖИННОГО ТЕСТУВАННЯ\n")
cat(strrep("-", 70), "\n\n")

cat(sprintf("%-40s | %-10s | %-12s | %-6s | %-6s | %-6s | %-6s\n",
            "Гіпотеза", "p (raw)", "p (Bonf.)", "Bonf.", "Holm", "BH", "Raw"))
cat(strrep("-", 105), "\n")
for (i in seq_len(nrow(results_df))) {
  cat(sprintf("%-40s | %-10s | %-12s | %-6s | %-6s | %-6s | %-6s\n",
              substr(results_df$Гіпотеза[i], 1, 40),
              formatC(results_df$p_raw[i],        format = "e", digits = 2),
              formatC(results_df$p_Bonferroni[i], format = "e", digits = 2),
              results_df$Відх_Bonf[i],
              results_df$Відх_Holm[i],
              results_df$Відх_BH[i],
              results_df$Відх_raw[i]
  ))
}


cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("БЛОК 4. ЧИ ЗМІНИЛИСЯ ВИСНОВКИ?\n")
cat(strrep("=", 70), "\n\n")

n_reject_raw  <- sum(all_p        < 0.05)
n_reject_bonf <- sum(p_bonferroni < 0.05)
n_reject_holm <- sum(p_holm       < 0.05)
n_reject_bh   <- sum(p_bh         < 0.05)

cat(sprintf("  Без корекції:    %d / %d відхилень H0\n", n_reject_raw,  k_total))
cat(sprintf("  Bonferroni:      %d / %d відхилень H0\n", n_reject_bonf, k_total))
cat(sprintf("  Holm:            %d / %d відхилень H0\n", n_reject_holm, k_total))
cat(sprintf("  BH (FDR <= 0.05): %d / %d відхилень H0\n\n", n_reject_bh, k_total))

survived_bonf <- results_df$Гіпотеза[results_df$Відх_raw  == "ТАК" & results_df$Відх_Bonf == "НІ"]
survived_holm <- results_df$Гіпотеза[results_df$Відх_raw  == "ТАК" & results_df$Відх_Holm == "НІ"]
survived_bh   <- results_df$Гіпотеза[results_df$Відх_raw  == "ТАК" & results_df$Відх_BH   == "НІ"]

cat("Гіпотези, що ПЕРЕСТАЛИ бути значущими після Bonferroni:\n")
if (length(survived_bonf) == 0) cat("  (жодна)\n") else
  for (h in survived_bonf) cat(sprintf("  > %s\n", h))

cat("\nГіпотези, що ПЕРЕСТАЛИ бути значущими після Holm:\n")
if (length(survived_holm) == 0) cat("  (жодна)\n") else
  for (h in survived_holm) cat(sprintf("  > %s\n", h))

cat("\nГіпотези, що ПЕРЕСТАЛИ бути значущими після BH:\n")
if (length(survived_bh) == 0) cat("  (жодна)\n") else
  for (h in survived_bh) cat(sprintf("  > %s\n", h))

cat("\nГіпотези, НЕ відхилені навіть без корекції:\n")
not_rej <- results_df$Гіпотеза[results_df$Відх_raw == "НІ"]
for (h in not_rej) {
  idx <- which(results_df$Гіпотеза == h)
  cat(sprintf("  > %-40s  p_raw = %.4f\n", h, results_df$p_raw[idx]))
}


write.csv(results_df, "multiple_testing_results.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nТаблицю збережено у multiple_testing_results.csv\n")

saveRDS(list(
  all_p_values  = all_p,
  p_bonferroni  = p_bonferroni,
  p_holm        = p_holm,
  p_bh          = p_bh,
  results_table = results_df,
  wald_median_tests = list(
    M1 = list(median_hat = m1_hat, se_boot = m1_se, z = res_m1$z, p = res_m1$p),
    M2 = list(median_hat = m2_hat, se_boot = m2_se, z = res_m2$z, p = res_m2$p),
    M3 = list(median_hat = m3_hat, se_boot = m3_se, z = res_m3$z, p = res_m3$p),
    M4 = list(median_hat = m4_hat, se_boot = m4_se, z = res_m4$z, p = res_m4$p)
  )
), file = "participant4_results.rds")
cat("Повні результати збережено у participant4_results.rds\n\n")