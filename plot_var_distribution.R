# ============================================================================
# SEM各变量得分占比随年份变化图
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
# 1. 定义变量及其标签 ----
# ============================================================================

var_info <- tribble(
  ~var,                  ~label,                          ~latent,
  "satis_way_of_sh",    "Satisfaction with SH sorting",  "ATT",
  "ws_attitude",        "Attitude toward sorting",       "ATT",
  "pr_atten",           "Public publicity effect",       "SN",
  "regulate_law",       "Legal regulation effect",       "SN",
  "regulate_commu_rule", "Community rule effect",         "SN",
  "if_neighbor_ws",     "Neighbor influence",            "SN",
  "if_family_ws",       "Family influence",              "SN",
  "if_know_method",     "If know method",                "PBC",
  "if_sign_sort",       "If clear signs",                "PBC",
  "category_trouble",   "Too many categories",           "PBC",
  "wil_of_engage",      "Willingness (BI)",              "DV",
  "seper_recyc",        "Separation behavior (BEH)",     "DV"
)

# ============================================================================
# 2. 计算各变量各年份的得分占比 ----
# ============================================================================

all_vars <- var_info$var

long_data <- ws_full %>%
  select(year, all_of(all_vars)) %>%
  pivot_longer(cols = all_of(all_vars), names_to = "variable", values_to = "score") %>%
  filter(!is.na(score)) %>%
  mutate(score = factor(as.integer(score))) %>%
  group_by(year, variable, score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year, variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  left_join(var_info, by = c("variable" = "var"))

# 设定 facet 顺序：按潜变量分组
long_data$label <- factor(long_data$label, levels = var_info$label)

# ============================================================================
# 3. 绘制堆叠条形图 ----
# ============================================================================

p <- ggplot(long_data, aes(x = factor(year), y = prop, fill = score)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ label, nrow = 3) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1, name = "Score") +
  labs(x = "Year", y = "Proportion") +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

ggsave("data_proc/sem_var_distribution_by_year.png", plot = p,
       width = 7, height = 4, dpi = 300)

cat("图已保存到 images/sem_var_distribution_by_year.pdf/.png\n")
