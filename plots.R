setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)

# summary_full <- readRDS("mc_summary.rds")

est_levels <- c("pols","re","fe","cre","ht")
est_labels <- c("POLS","RE","FE","CRE","HT")

summary_full$estimator <- factor(summary_full$estimator,
                                 levels = est_levels,
                                 labels = est_labels)

param_levels <- c("beta_u","beta_c","gamma_u","gamma_c")
param_labels <- c(expression(beta[u]~"(exog. TV)"),
                  expression(beta[c]~"(endog. TV)"),
                  expression(gamma[u]~"(exog. TI)"),
                  expression(gamma[c]~"(endog. TI)"))

summary_full$parameter <- factor(summary_full$parameter,
                                 levels = param_levels)

est_colors <- c(
  "POLS" = "#999999",
  "RE"   = "#E69F00",
  "FE"   = "#56B4E9",
  "CRE"  = "#009E73",
  "HT"   = "#D55E00"
)

my_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title    = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"))

# Figure 1: bias and RMSE of gamma_c


gc <- summary_full[summary_full$parameter == "gamma_c", ]

gc_long <- rbind(
  data.frame(pi = gc$pi, estimator = gc$estimator,
             metric = "Bias", value = gc$bias),
  data.frame(pi = gc$pi, estimator = gc$estimator,
             metric = "RMSE", value = gc$rmse)
)
gc_long$metric <- factor(gc_long$metric, levels = c("Bias","RMSE"))

p1 <- ggplot(gc_long, aes(x = pi, y = value,
                          color = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_color_manual(values = est_colors) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  scale_x_continuous(breaks = unique(gc_long$pi)) +
  labs(
    x = expression(pi~"(instrument relevance)"),
    y = expression("Estimate of"~gamma[c]),
    title = expression("Finite-sample performance for"~gamma[c]~"= 1"),
    subtitle = "N = 500, T = 10, 500 Monte Carlo replications"
  ) +
  my_theme

ggsave("fig1_gamma_c.png", p1, width = 9, height = 4.5, dpi = 200)


# Figure 2: Bias of all four parameters

p2 <- ggplot(summary_full,
             aes(x = pi, y = bias,
                 color = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ parameter, scales = "free_y",
             labeller = label_parsed) +
  scale_color_manual(values = est_colors) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  scale_x_continuous(breaks = unique(summary_full$pi)) +
  labs(
    x = expression(pi~"(instrument relevance)"),
    y = "Bias",
    title = "Bias of all parameters",
    subtitle = "True value = 1 for all coefficients"
  ) +
  my_theme

ggsave("fig2_bias_all.png", p2, width = 9, height = 6, dpi = 200)


# Figure 3: RMSE of all four parameters

p3 <- ggplot(summary_full,
             aes(x = pi, y = rmse,
                 color = estimator, shape = estimator)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ parameter, scales = "free_y",
             labeller = label_parsed) +
  scale_color_manual(values = est_colors) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  scale_x_continuous(breaks = unique(summary_full$pi)) +
  labs(
    x = expression(pi~"(instrument relevance)"),
    y = "RMSE",
    title = "RMSE of all parameters"
  ) +
  my_theme

ggsave("fig3_rmse_all.png", p3, width = 9, height = 6, dpi = 200)

print(p1)
print(p2)
print(p3)

cat("\nSaved figures:\n",
    "  fig1_gamma_c.png  -- headline\n",
    "  fig2_bias_all.png -- bias, all params\n",
    "  fig3_rmse_all.png -- RMSE, all params\n", sep = "")