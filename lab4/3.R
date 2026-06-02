# install.packages(c("tidyverse", "mgcv", "gratia", "cowplot", "sandwich", "lmtest"))
library(tidyverse)
library(mgcv)       
library(gratia)     # візуалізація GAM (draw, smooth_estimates)
library(cowplot)
library(sandwich)   # HC3 стандартні помилки (для robustness таблиці)
library(lmtest)     # coeftest



csv_candidates <- c("cleaned_df.csv", "cleaned_df копія.csv")
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) stop("Не знайдено cleaned_df.csv. Покладіть файл у робочу директорію.")
cat("Читаю датасет з файлу:", csv_path, "\n")

df_raw <- read_csv(csv_path, show_col_types = FALSE)
cat("Розмір датасету:", nrow(df_raw), "рядків\n")

df <- df_raw %>%
  mutate(
    ln_response_time = log(first_response_time_hours + 1),
    has_code         = as.integer(has_code),
    ln_reputation    = log(owner_reputation + 1),
    ln_word_count    = log(body_word_count),
    is_weekend       = as.integer(creation_weekday %in% c(5L, 6L)),
    post_chatgpt     = as.integer(creation_year >= 2023),
    programming_language = relevel(factor(programming_language),
                                   ref = "javascript")
  ) %>%
  filter(!is.na(first_response_time_hours),
         is.finite(ln_word_count))          # log(0) = -Inf -> виключаємо

cat("Робоча вибірка після фільтрів:", nrow(df), "\n")


set.seed(42)
train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
df_train  <- df[train_idx, ]
df_val    <- df[-train_idx, ]
cat("Навчальна:", nrow(df_train), "| Валідаційна:", nrow(df_val), "\n\n")




cat("Оцінка GAM (повна модель)...\n")
gam_full <- bam(
  ln_response_time ~
    # непараметричні сплайни:
    s(ln_reputation, bs = "cr", k = 10) +
    s(ln_word_count, bs = "cr", k = 10) +
    # параметрична частина:
    has_code +
    has_code:ln_reputation +
    difficulty_score +
    quality_score +
    is_weekend +
    post_chatgpt +
    programming_language,
  data   = df_train,
  family = gaussian(),
  method = "fREML",     # fast REML — оптимальний для великих n
  discrete = TRUE       # дискретизація предикторів — прискорює bam()
)


print(summary(gam_full))


cat("\n--- gam.check (вузли k) ---\n")
gam.check(gam_full, rep = 500)    # k-test: p > 0.05 => k достатнє


med_rep   <- median(df_train$ln_reputation)
med_wc    <- median(df_train$ln_word_count)
mode_code <- as.integer(round(mean(df_train$has_code)))
med_diff  <- median(df_train$difficulty_score)
med_qual  <- median(df_train$quality_score)
mode_wknd <- as.integer(round(mean(df_train$is_weekend)))
mode_chat <- as.integer(round(mean(df_train$post_chatgpt)))

cat(sprintf("\nРівні фіксації для графіків:\n"))
cat(sprintf("  ln_reputation    (медіана) = %.4f\n", med_rep))
cat(sprintf("  ln_word_count    (медіана) = %.4f\n", med_wc))
cat(sprintf("  has_code         (мода)    = %d\n",   mode_code))
cat(sprintf("  difficulty_score (медіана) = %.4f\n", med_diff))
cat(sprintf("  quality_score    (медіана) = %.4f\n", med_qual))
cat(sprintf("  is_weekend       (мода)    = %d\n",   mode_wknd))
cat(sprintf("  post_chatgpt     (мода)    = %d\n",   mode_chat))
cat(sprintf("  programming_language       = javascript (базова)\n\n"))


build_newdata_gam <- function(grid_x, x_name) {
  tibble(
    ln_reputation    = if (x_name == "ln_reputation") grid_x else med_rep,
    ln_word_count    = if (x_name == "ln_word_count") grid_x else med_wc,
    has_code         = mode_code,
    difficulty_score = med_diff,
    quality_score    = med_qual,
    is_weekend       = mode_wknd,
    post_chatgpt     = mode_chat,
    programming_language = factor("javascript",
                                  levels = levels(df_train$programming_language))
  )
}


grid_rep <- seq(min(df_train$ln_reputation),
                max(df_train$ln_reputation),
                length.out = 300)

nd_rep  <- build_newdata_gam(grid_rep, "ln_reputation")
pr_rep  <- predict(gam_full, newdata = nd_rep, se.fit = TRUE)

plot_rep <- tibble(
  x    = grid_rep,
  yhat = pr_rep$fit,
  lo   = pr_rep$fit - 1.96 * pr_rep$se.fit,
  hi   = pr_rep$fit + 1.96 * pr_rep$se.fit
)

p_rep <- ggplot(plot_rep, aes(x = x, y = yhat)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#31a354", alpha = 0.25) +
  geom_line(colour = "#31a354", linewidth = 1.2) +
  labs(
    title    = "GAM: ln(час відповіді) від репутації автора",
    subtitle = paste0(
      "Фіксація: ln_word_count = ", round(med_wc, 2),
      "; has_code = ", mode_code,
      "; programming_language = javascript\n(95% довірча смуга)"
    ),
    x = "ln(owner_reputation + 1)",
    y = "ln(first_response_time + 1)"
  ) +
  theme_minimal(base_size = 13)

ggsave("fig_gam_reputation.png", p_rep, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_gam_reputation.png\n")


grid_wc <- seq(min(df_train$ln_word_count),
               max(df_train$ln_word_count),
               length.out = 300)

nd_wc  <- build_newdata_gam(grid_wc, "ln_word_count")
pr_wc  <- predict(gam_full, newdata = nd_wc, se.fit = TRUE)

plot_wc <- tibble(
  x    = grid_wc,
  yhat = pr_wc$fit,
  lo   = pr_wc$fit - 1.96 * pr_wc$se.fit,
  hi   = pr_wc$fit + 1.96 * pr_wc$se.fit
)

p_wc <- ggplot(plot_wc, aes(x = x, y = yhat)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#31a354", alpha = 0.25) +
  geom_line(colour = "#31a354", linewidth = 1.2) +
  labs(
    title    = "GAM: ln(час відповіді) від обсягу тексту питання",
    subtitle = paste0(
      "Фіксація: ln_reputation = ", round(med_rep, 2),
      "; has_code = ", mode_code,
      "; programming_language = javascript\n(95% довірча смуга)"
    ),
    x = "ln(body_word_count)",
    y = "ln(first_response_time + 1)"
  ) +
  theme_minimal(base_size = 13)

ggsave("fig_gam_wordcount.png", p_wc, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_gam_wordcount.png\n")


p_gam_combined <- cowplot::plot_grid(p_rep, p_wc, ncol = 2, labels = c("A", "B"))
ggsave("fig_gam_combined.png", p_gam_combined, width = 18, height = 5.5, dpi = 150)
cat("Збережено: fig_gam_combined.png\n")



p_smooths <- draw(gam_full,
                  select    = c("s(ln_reputation)", "s(ln_word_count)"),
                  residuals = FALSE) &
  theme_minimal(base_size = 12)

ggsave("fig_gam_smooths.png", p_smooths,
       width = 14, height = 5.5, dpi = 150)
cat("Збережено: fig_gam_smooths.png\n\n")


cat(" АНАЛІЗ СТІЙКОСТІ (Robustness) \n\n")

fit_gam_spec <- function(formula_str, data) {
  bam(as.formula(formula_str), data = data,
      family = gaussian(), method = "fREML", discrete = TRUE)
}

gam_m0 <- fit_gam_spec(
  "ln_response_time ~ s(ln_reputation, bs='cr', k=10) +
                      s(ln_word_count,  bs='cr', k=10) +
                      has_code",
  df_train
)

gam_m1 <- fit_gam_spec(
  "ln_response_time ~ s(ln_reputation, bs='cr', k=10) +
                      s(ln_word_count,  bs='cr', k=10) +
                      has_code +
                      difficulty_score + quality_score",
  df_train
)

gam_m2 <- fit_gam_spec(
  "ln_response_time ~ s(ln_reputation, bs='cr', k=10) +
                      s(ln_word_count,  bs='cr', k=10) +
                      has_code +
                      difficulty_score + quality_score +
                      is_weekend + post_chatgpt",
  df_train
)

gam_m3 <- fit_gam_spec(
  "ln_response_time ~ s(ln_reputation, bs='cr', k=10) +
                      s(ln_word_count,  bs='cr', k=10) +
                      has_code +
                      has_code:ln_reputation +
                      difficulty_score + quality_score +
                      is_weekend + post_chatgpt",
  df_train
)


models_rob <- list(
  "M0: базова"           = gam_m0,
  "M1: +якість"          = gam_m1,
  "M2: +час"             = gam_m2,
  "M3: +взаємодія"       = gam_m3,
  "M4: повна (мова)"     = gam_full
)

# Витягуємо параметрові коефіцієнти та EDF сплайнів
extract_rob_row <- function(model, name) {
  s   <- summary(model)
  # параметрична частина
  pc  <- s$p.coef
  pse <- sqrt(diag(s$p.cov))
  pp  <- s$p.pv
  
  beta_code <- if ("has_code" %in% names(pc)) pc["has_code"] else NA_real_
  se_code   <- if ("has_code" %in% names(pse)) pse["has_code"] else NA_real_
  p_code    <- if ("has_code" %in% names(pp)) pp["has_code"] else NA_real_
  
  # EDF сплайнів
  edf_rep <- s$edf[grep("ln_reputation", rownames(s$s.table))[1]]
  edf_wc  <- s$edf[grep("ln_word_count",  rownames(s$s.table))[1]]
  
  tibble(
    Специфікація         = name,
    `β(has_code)`        = round(beta_code, 4),
    `SE`                 = round(se_code, 4),
    `p-value`            = signif(p_code, 3),
    `EDF(s(ln_rep))`     = round(edf_rep, 2),
    `EDF(s(ln_wc))`      = round(edf_wc, 2),
    `Adj.R²`             = round(s$r.sq, 4)
  )
}

rob_table <- imap_dfr(models_rob, ~ extract_rob_row(.x, .y))

cat("Таблиця стійкості β(has_code) по специфікаціях:\n")
print(rob_table, n = Inf)
write_csv(rob_table, "gam_robustness_table.csv")
cat("\nЗбережено: gam_robustness_table.csv\n\n")


rob_plot_df <- rob_table %>%
  mutate(
    lo  = `β(has_code)` - 1.96 * SE,
    hi  = `β(has_code)` + 1.96 * SE,
    spec_f = factor(Специфікація, levels = rev(Специфікація))
  )

p_rob <- ggplot(rob_plot_df,
                aes(x = `β(has_code)`, y = spec_f)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi),
                 height = 0.3, colour = "#2c7fb8", linewidth = 1) +
  geom_point(colour = "#2c7fb8", size = 3) +
  labs(
    title    = "Аналіз стійкості: ефект has_code по специфікаціях GAM",
    subtitle = "Горизонтальні відрізки — 95% довірчі інтервали",
    x = "β(has_code)",
    y = NULL
  ) +
  theme_minimal(base_size = 13)

ggsave("fig_gam_robustness.png", p_rob, width = 10, height = 5, dpi = 150)
cat("Збережено: fig_gam_robustness.png\n")



cat("\n ПОРІВНЯННЯ КОЕФІЦІЄНТІВ: GAM (Лаб.4) vs PLR (Лаб.4) vs OLS (Лаб.3)\n")

# Читаємо CSV PLR порівняння, якщо існує
plr_csv <- "plr_vs_lab3_coefs.csv"
if (file.exists(plr_csv)) {
  plr_comp <- read_csv(plr_csv, show_col_types = FALSE)
} else {
  plr_comp <- NULL
  cat("(plr_vs_lab3_coefs.csv не знайдено — пропускаємо стовпець PLR)\n")
}


s_full  <- summary(gam_full)
gam_pc  <- s_full$p.table[, "Estimate"]
gam_pse <- s_full$p.table[, "Std. Error"]
gam_pp  <- s_full$p.table[, "Pr(>|t|)"]

common_vars <- c("has_code", "has_code:ln_reputation",
                 "difficulty_score", "quality_score",
                 "is_weekend", "post_chatgpt")

# Тепер розміри колонок співпадатимуть
gam_coef_df <- tibble(
  Регресор          = names(gam_pc),
  `GAM β`           = round(gam_pc, 4),
  `GAM SE`          = round(gam_pse, 4),
  `GAM p-value`     = signif(gam_pp, 3)
) %>%
  filter(Регресор %in% common_vars)

# Об'єднуємо з результатами PLR та OLS для Учасника 5
if (!is.null(plr_comp)) {
  final_comparison <- plr_comp %>%
    left_join(gam_coef_df, by = "Регресор")
  
  cat("\nФінальна порівняльна таблиця (OLS vs PLR vs GAM):\n")
  print(final_comparison)
  
  write_csv(final_comparison, "final_models_comparison.csv")
  cat("\nЗбережено: final_models_comparison.csv (передайте цей файл Учаснику 5)\n")
} else {
  print(gam_coef_df)

cat("Розрахунок MSE на валідаційній вибірці...\n")

# Перевіряємо, що рівні фактора збігаються
df_val_gam <- df_val %>%
  mutate(programming_language = factor(
    as.character(programming_language),
    levels = levels(df_train$programming_language)
  )) %>%
  filter(!is.na(programming_language),
         is.finite(ln_word_count))

pred_gam_val  <- predict(gam_full, newdata = df_val_gam)
mse_gam_val   <- mean((df_val_gam$ln_response_time - pred_gam_val)^2,
                      na.rm = TRUE)

# Train MSE
pred_gam_train <- gam_full$fitted.values
mse_gam_train  <- mean((df_train$ln_response_time - pred_gam_train)^2)
r2_gam         <- summary(gam_full)$r.sq

cat(sprintf("R² GAM  (навч.):  %.4f\n", r2_gam))
cat(sprintf("MSE GAM (навч.):  %.4f\n", mse_gam_train))
cat(sprintf("MSE GAM (вал.):   %.4f\n", mse_gam_val))


cat("\n", strrep("=", 65), "\n", sep = "")
cat("ПІДСУМОК GAM (УЧАСНИК 3)\n")
cat(strrep("=", 65), "\n", sep = "")

s_full <- summary(gam_full)
summary_gam <- data.frame(
  Показник = c(
    "n (навчальна)",
    "Число параметричних регресорів",
    "Число сплайнів",
    "EDF s(ln_reputation)",
    "EDF s(ln_word_count)",
    "Adj. R² (навч.)",
    "MSE (навч.)",
    "MSE (вал.)"
  ),
  Значення = c(
    as.character(nrow(df_train)),
    as.character(length(s_full$p.coef) - 1),   # -1 intercept
    "2",
    sprintf("%.3f", s_full$edf[grep("ln_reputation", rownames(s_full$s.table))[1]]),
    sprintf("%.3f", s_full$edf[grep("ln_word_count",  rownames(s_full$s.table))[1]]),
    sprintf("%.4f", r2_gam),
    sprintf("%.4f", mse_gam_train),
    sprintf("%.4f", mse_gam_val)
  )
)
print(summary_gam, row.names = FALSE)

cat("  gam_full        -> повна GAM модель (M4)\n")
cat("  rob_table       -> таблиця стійкості (для звіту)\n")
cat("  comparison_all  -> порівняння GAM / PLR / OLS\n")
cat("  mse_gam_val     -> для зведеної таблиці (Учасник 5)\n")

cat("\nЗбережені файли:\n")
for (f in c("fig_gam_reputation.png",
            "fig_gam_wordcount.png",
            "fig_gam_combined.png",
            "fig_gam_smooths.png",
            "fig_gam_robustness.png",
            "gam_robustness_table.csv",
            "gam_vs_plr_vs_ols.csv")) {
  cat(" ", f, "\n")
}