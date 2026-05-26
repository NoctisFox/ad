if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")
if (!require("car")) install.packages("car")
if (!require("ggcorrplot")) install.packages("ggcorrplot")


library(tidyverse)
library(sandwich)      # vcovHC()
library(lmtest)        # coeftest(), waldtest()
library(car)           # linearHypothesis(), vif()
library(ggcorrplot)    # ggcorrplot()


if (!exists("hc3")) hc3 <- function(m) vcovHC(m, type = "HC3")

cat("\n", strrep("=", 65), "\n", sep = "")
cat("1. ІНДИВІДУАЛЬНА ЗНАЧУЩІСТЬ КЛЮЧОВИХ КОЕФІЦІЄНТІВ\n")
cat(strrep("=", 65), "\n", sep = "")

key_vars <- c("has_code", "ln_reputation", "ln_word_count",
              "post_chatgpt", "is_weekend", "difficulty_score", "quality_score")

# Функція для форматованого виводу значущості
print_significance <- function(ct, model_name, vars = key_vars) {
  cat(sprintf("\n", model_name))
  cat(sprintf("%-35s %8s %8s %10s %5s\n",
              "Регресор", "b", "HC3 SE", "p-value", "Sig"))
  cat(strrep("-", 70), "\n")
  for (v in vars) {
    if (v %in% rownames(ct)) {
      b   <- ct[v, "Estimate"]
      se  <- ct[v, "Std. Error"]
      pv  <- ct[v, "Pr(>|t|)"]
      sig <- ifelse(pv < 0.01, "***",
                    ifelse(pv < 0.05, "**",
                           ifelse(pv < 0.10, "*", "н.з.")))
      cat(sprintf("%-35s %+8.4f %8.4f %10.4f %5s\n", v, b, se, pv, sig))
    }
  }
}
key_vars4 <- c(key_vars, "ln_reputation_sq", "ln_word_count_sq")

# Виводимо для всіх п'яти моделей
print_significance(ct1, "Модель (1) — Базова")
print_significance(ct2, "Модель (2) — +Контрольні змінні")
print_significance(ct3, "Модель (3) — +Dummy мов")
print_significance(ct4, "Модель (4) — +Поліноми", vars = key_vars4)

# Для Моделі 5 — додаємо взаємодію
key_vars5 <- c(key_vars, "has_code:ln_reputation",
               "ln_reputation_sq", "ln_word_count_sq")
print_significance(ct5, "Модель (5) — +Взаємодія", vars = key_vars5)

cat("\n", strrep("=", 65), "\n", sep = "")
cat("2. F-ТЕСТИ (Wald) ГРУП КОЕФІЦІЄНТІВ\n")
cat(strrep("=", 65), "\n", sep = "")

# Допоміжна функція для читабельного виводу F-тесту
report_ftest <- function(ft, label, h0_text) {
  f_val <- ft[2, "F"]
  df1   <- ft[2, "Df"]
  df2   <- ft[2, "Res.Df"]
  pv    <- ft[2, "Pr(>F)"]
  sig   <- ifelse(pv < 0.01, "***",
                  ifelse(pv < 0.05, "**",
                         ifelse(pv < 0.10, "*", "не відхиляємо H₀")))
  cat(sprintf("\n[%s] H₀: %s\n", label, h0_text))
  cat(sprintf("  F(%d, %d) = %.3f,  p = %.4g  %s\n",
              df1, df2, f_val, pv, sig))
}

# 2.1 Поліноми вище 1-го порядку
# H₀: ln_reputation² = 0 ТА ln_word_count² = 0
ft_poly <- linearHypothesis(
  model_4,
  c("ln_reputation_sq = 0", "ln_word_count_sq = 0"),
  vcov = hc3(model_4), test = "F"
)
report_ftest(ft_poly, "2.1",
             "ln_reputation² = ln_word_count² = 0 (Модель 4)")

# 2.2 Квадратичний терм репутації окремо
ft_rep_sq <- linearHypothesis(
  model_4,
  "ln_reputation_sq = 0",
  vcov = hc3(model_4), test = "F"
)
report_ftest(ft_rep_sq, "2.2",
             "ln_reputation² = 0 (Модель 4)")

# 2.3 Квадратичний терм обсягу тексту окремо
ft_wc_sq <- linearHypothesis(
  model_4,
  "ln_word_count_sq = 0",
  vcov = hc3(model_4), test = "F"
)
report_ftest(ft_wc_sq, "2.3",
             "ln_word_count² = 0 (Модель 4)")

# 2.4 Взаємодія has_code × ln_reputation
ft_inter <- linearHypothesis(
  model_5,
  "has_code:ln_reputation = 0",
  vcov = hc3(model_5), test = "F"
)
report_ftest(ft_inter, "2.4",
             "has_code:ln_reputation = 0 (Модель 5)")

# 2.5 Загальний ефект has_code (сам регресор + взаємодія)
ft_code_all <- linearHypothesis(
  model_5,
  c("has_code = 0", "has_code:ln_reputation = 0"),
  vcov = hc3(model_5), test = "F"
)
report_ftest(ft_code_all, "2.5",
             "has_code = has_code:ln_reputation = 0 (Модель 5)")

# 2.6 Усі dummy-змінні мов програмування
lang_dummies <- grep("^programming_language", names(coef(model_3)), value = TRUE)
ft_lang <- linearHypothesis(
  model_3, lang_dummies,
  vcov = hc3(model_3), test = "F"
)
report_ftest(ft_lang, "2.6",
             sprintf("усі %d dummy мов = 0 (Модель 3)", length(lang_dummies)))

# 2.7 Часові ефекти: post_chatgpt та is_weekend разом
ft_time <- linearHypothesis(
  model_2,
  c("post_chatgpt = 0", "is_weekend = 0"),
  vcov = hc3(model_2), test = "F"
)
report_ftest(ft_time, "2.7",
             "post_chatgpt = is_weekend = 0 (Модель 2)")

# 2.8 Поліноми репутації вищого порядку: Модель 6
ft_rep_hi <- linearHypothesis(
  model_6,
  c("ln_reputation_sq = 0", "ln_reputation_cu = 0"),
  vcov = hc3(model_6), test = "F"
)
report_ftest(ft_rep_hi, "2.8",
             "ln_reputation² = ln_reputation³ = 0 (Модель 6)")

cat("\n")

cat("\n", strrep("=", 65), "\n", sep = "")
cat("3. АНАЛІЗ МУЛЬТИКОЛІНЕАРНОСТІ\n")
cat(strrep("=", 65), "\n", sep = "")

# 3.1 Кореляційна матриця числових регресорів
cor_vars <- df %>%
  select(has_code, ln_reputation, ln_word_count,
         difficulty_score, quality_score, is_weekend, post_chatgpt,
         is_veteran, ln_reputation_sq, ln_word_count_sq)

cor_mat <- round(cor(cor_vars, use = "complete.obs"), 3)
cat("\n[3.1] Кореляційна матриця числових регресорів:\n")
print(cor_mat)

# 3.2 VIF для Моделі 2 (без категорій та поліномів)
cat("\n[3.2] VIF для Моделі 2 (базові регресори без поліномів):\n")
vif2 <- vif(model_2)
print(round(vif2, 3))

# 3.3 GVIF для Моделі 4 (з квадратами)
cat("\n[3.3] GVIF для Моделі 4 (поліноми — очікується вищий VIF через x та x²):\n")
vif4 <- vif(model_4)
print(round(vif4, 3))

# 3.4 VIF для Моделі 5 (з взаємодією)
cat("\n[3.4] GVIF для Моделі 5 (взаємодія has_code × ln_reputation):\n")
vif5 <- vif(model_5)
print(round(vif5, 3))


# 4.1 Heatmap кореляційної матриці (без поліномів)
cor_core <- df %>%
  select(has_code, ln_reputation, ln_word_count,
         difficulty_score, quality_score, is_weekend, post_chatgpt) %>%
  cor(use = "complete.obs")

rownames(cor_core) <- colnames(cor_core) <- c(
  "Наявність коду", "ln(репутація)",
  "ln(слова)", "Складність", "Якість",
  "Вихідний", "Після ChatGPT"
)

p_corr <- ggcorrplot(cor_core,
                     method   = "square",
                     type     = "lower",
                     lab      = TRUE,
                     lab_size = 3,
                     colors   = c("#d73027", "white", "#1a9641"),
                     outline.color = "white",
                     title    = "Кореляційна матриця ключових регресорів",
                     ggtheme  = theme_minimal(base_size = 12)
) +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    axis.text.x  = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y  = element_text(size = 9)
  )

ggsave("fig_corr_matrix.png", p_corr, width = 8, height = 7, dpi = 150)


# 4.2 VIF-графік для Моделей 2 і 4
vif2_df <- data.frame(
  Регресор = names(vif2),
  VIF      = as.numeric(vif2),
  Модель   = "Модель (2)"
)

vif4 <- vif(model_4)

if (is.matrix(vif4)) {
  vif4_df <- data.frame(
    Регресор = rownames(vif4),
    VIF      = vif4[, "GVIF"],
    Df       = vif4[, "Df"],
    GVIF_adj = vif4[, "GVIF^(1/(2*Df))"],
    Модель   = "Модель (4)"
  )
  
} else {
  
  vif4_df <- data.frame(
    Регресор = names(vif4),
    VIF      = as.numeric(vif4),
    Модель   = "Модель (4)"
  )
}

vif_plot_df <- bind_rows(
  vif2_df %>% filter(Регресор %in% c("has_code", "ln_reputation",
                                     "ln_word_count", "difficulty_score",
                                     "quality_score", "is_weekend",
                                     "post_chatgpt")),
  vif4_df %>% filter(Регресор %in% c("has_code", "ln_reputation",
                                     "ln_reputation_sq", "ln_word_count",
                                     "ln_word_count_sq", "difficulty_score",
                                     "quality_score", "is_weekend",
                                     "post_chatgpt"))
) %>%
  mutate(
    Регресор = factor(Регресор, levels = unique(Регресор)),
    Колір    = case_when(VIF > 10 ~ "високий",
                         VIF > 5  ~ "помірний",
                         TRUE     ~ "прийнятний")
  )

p_vif <- ggplot(vif_plot_df,
                aes(x = fct_reorder(Регресор, VIF), y = VIF, fill = Колір)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 5,
             linetype = "dashed",
             colour = "#f16913",
             linewidth = 0.8) +
  geom_hline(yintercept = 10,
             linetype = "solid",
             colour = "#d73027",
             linewidth = 0.8) +
  annotate("text", x = 0.7, y = 5.4, label = "VIF = 5",
           colour = "#f16913", size = 3.2, hjust = 0) +
  annotate("text", x = 0.7, y = 10.6, label = "VIF = 10",
           colour = "#d73027", size = 3.2, hjust = 0) +
  coord_flip() +
  facet_wrap(~ Модель, scales = "free_x") +
  scale_fill_manual(values = c("прийнятний" = "#41ab5d",
                               "помірний"   = "#f16913",
                               "високий"  = "#d73027")) +
  labs(
    title    = "Фактор інфляції дисперсії (VIF) для Моделей (2) та (4)",
    subtitle = "Поліноми закономірно підвищують VIF для репутації та обсягу тексту",
    x = NULL, y = "VIF",
    fill = "Рівень"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("fig_vif.png", p_vif, width = 10, height = 6, dpi = 150)


# 4.3 Графік результатів F-тестів
ftest_summary <- tibble(
  Тест = c(
    "2.1 ln_rep² = ln_wc² = 0 (М4)",
    "2.2 ln_rep² = 0 (М4)",
    "2.3 ln_wc² = 0 (М4)",
    "2.4 has_code:ln_rep = 0 (М5)",
    "2.5 has_code = has_code:ln_rep = 0 (М5)",
    "2.6 Усі dummy мов = 0 (М3)",
    "2.7 post_chatgpt = is_weekend = 0 (М2)"
  ),
  F_stat = c(
    ft_poly[2,    "F"],
    ft_rep_sq[2,  "F"],
    ft_wc_sq[2,   "F"],
    ft_inter[2,   "F"],
    ft_code_all[2,"F"],
    ft_lang[2,    "F"],
    ft_time[2,    "F"]
  ),
  p_val = c(
    ft_poly[2,    "Pr(>F)"],
    ft_rep_sq[2,  "Pr(>F)"],
    ft_wc_sq[2,   "Pr(>F)"],
    ft_inter[2,   "Pr(>F)"],
    ft_code_all[2,"Pr(>F)"],
    ft_lang[2,    "Pr(>F)"],
    ft_time[2,    "Pr(>F)"]
  )
) %>%
  mutate(
    Значущий = p_val < 0.05,
    label    = ifelse(p_val < 0.001,
                      sprintf("F=%.1f\np<0.001", F_stat),
                      sprintf("F=%.2f\np=%.3f", F_stat, p_val))
  )

p_ftests <- ggplot(ftest_summary,
                   aes(x = fct_reorder(Тест, F_stat),
                       y = log10(F_stat + 1),
                       fill = Значущий)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 3) +
  coord_flip(ylim = c(0, max(log10(ftest_summary$F_stat + 1)) * 1.35)) +
  scale_fill_manual(values = c("TRUE" = "#3182bd", "FALSE" = "#bdbdbd"),
                    labels = c("TRUE" = "p < 0.05 (значущий)",
                               "FALSE" = "p ≥ 0.05")) +
  labs(
    title    = "F-статистики для тестів на спільну значущість груп коефіцієнтів",
    subtitle = "Вісь X: log₁₀(F + 1) для кращої видимості різних масштабів",
    x = NULL, y = "log₁₀(F + 1)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("fig_ftests.png", p_ftests, width = 10, height = 5.5, dpi = 150)


cat("\n", strrep("=", 65), "\n", sep = "")
cat("5. ПІДСУМКОВА ТАБЛИЦЯ F-ТЕСТІВ\n")
cat(strrep("=", 65), "\n", sep = "")
cat(sprintf("%-50s %6s %6s %10s %5s\n",
            "Нульова гіпотеза (H₀)", "df₁", "df₂", "F", "p"))
cat(strrep("-", 80), "\n")

print_frow <- function(ft, label, df_shift = 2) {
  f_val <- ft[df_shift, "F"]
  df1   <- ft[df_shift, "Df"]
  df2   <- ft[df_shift, "Res.Df"]
  pv    <- ft[df_shift, "Pr(>F)"]
  sig   <- ifelse(pv < 0.01, "***", ifelse(pv < 0.05, "**",
                                           ifelse(pv < 0.10, "*", "")))
  cat(sprintf("%-50s %6d %6d %10.2f %s\n", label, df1, df2, f_val,
              ifelse(pv < 0.001, "<0.001***",
                     sprintf("%.4f%s", pv, sig))))
}

print_frow(ft_poly,     "ln_rep² = ln_wc² = 0 (М4)")
print_frow(ft_rep_sq,   "ln_rep² = 0 (М4)")
print_frow(ft_wc_sq,    "ln_wc² = 0 (М4)")
print_frow(ft_inter,    "has_code:ln_rep = 0 (М5)")
print_frow(ft_code_all, "has_code = has_code:ln_rep = 0 (М5)")
print_frow(ft_lang,     sprintf("Усі %d dummy мов = 0 (М3)", length(lang_dummies)))
print_frow(ft_time,     "post_chatgpt = is_weekend = 0 (М2)")
print_frow(ft_rep_hi,   "ln_rep² = ln_rep³ = 0 (М6)")

