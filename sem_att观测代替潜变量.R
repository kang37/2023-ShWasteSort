# 基础模型 ----
# 定义TPB结构方程模型
tpb_sem_model <- '
  # 测量模型
  SN  =~ pr_atten + if_no_ws_guilty + regulate_law + regulate_commu_rule + 
         if_neighbor_ws + if_family_ws
  PBC =~ if_know_method + if_sign_sort + category_trouble + time_cost_troub

  # 结构路径
  wil_of_engage ~ ws_attitude + SN + PBC
  seper_recyc ~ wil_of_engage
  
  # 局部误差协方差
  category_trouble ~~ time_cost_troub
  if_neighbor_ws ~~ if_family_ws
'

# 定义有序变量
ordered_vars <- c(
  "ws_attitude", "threat", "pr_atten", "if_no_ws_guilty", "regulate_law", "regulate_commu_rule", "if_neighbor_ws", "if_family_ws", "if_know_method", "if_sign_sort", "category_trouble", "time_cost_troub", "wil_of_engage"
)

# 拟合多组SEM（配置不变性模型）
fit_sem <- sem(
  model = tpb_sem_model,
  data = ws_full,
  group = "year",
  estimator = "WLSMV",
  ordered = ordered_vars,
  parameterization = "theta"
)
lavInspect(fit_sem, "cor.lv")

# 查看拟合指数
fit_indices <- fitMeasures(fit_sem, c("chisq", "df", "pvalue", "cfi", 
                                      "tli", "rmsea", "srmr"))
print(round(fit_indices, 3))

# ============================================================================
# 提取路径系数 ----
# ============================================================================

# 提取标准化参数估计
std_estimates <- parameterEstimates(fit_sem, standardized = TRUE)

# 筛选回归路径
path_results <- std_estimates %>%
  filter(op == "~", !is.na(std.all)) %>%
  select(group, lhs, rhs, est, se, z, pvalue, std.all)

# 添加年份标签
year_labels <- c("2019", "2020", "2021", "2022", "2023")
path_results$year <- year_labels[path_results$group]

# 显示结果
cat("\n各年份路径系数:\n")
print(path_results %>% select(year, lhs, rhs, std.all, pvalue))

# ============================================================================
# 绘制路径系数随年份变化图 ----
# ============================================================================
plot_data <- path_results %>%
  filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
  mutate(
    # 路径标签（英文，简短）
    Path = case_when(
      lhs == "wil_of_engage" & rhs == "ws_attitude" ~ "ws_attitude → BI",
      lhs == "wil_of_engage" & rhs == "SN" ~ "SN → BI",
      lhs == "wil_of_engage" & rhs == "PBC" ~ "PBC → BI",
      lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "BI → BEH",
      TRUE ~ "Other"
    ),
    # 路径标签（完整，用于facet标题）
    Path_full = case_when(
      lhs == "wil_of_engage" & rhs == "ws_attitude" ~ "ws_attitude → Intention",
      lhs == "wil_of_engage" & rhs == "SN" ~ "Subjective Norm → Intention",
      lhs == "wil_of_engage" & rhs == "PBC" ~ "Perceived Control → Intention",
      lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "Intention → Behavior",
      TRUE ~ "Other"
    ),
    # 显著性（二分类）
    Significant = ifelse(pvalue < 0.05, "Significant (p < 0.05)", "Not significant"),
    Significant = factor(Significant, levels = c("Significant (p < 0.05)", "Not significant")),
    
    # 年份数值
    year_num = as.numeric(year),
    
    # 文本标签
    sig_label = ifelse(pvalue < 0.05, "*", "")
  ) %>%
  filter(Path != "Other") %>%
  # 设置facet顺序
  mutate(Path_full = factor(Path_full, levels = c(
    "ws_attitude → Intention",
    "Subjective Norm → Intention", 
    "Perceived Control → Intention",
    "Intention → Behavior"
  )))


ggplot(plot_data, aes(x = year_num, y = std.all)) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4
  ) +
  geom_line(linewidth = 0.9, color = "grey") +
  geom_point(aes(fill = Significant, shape = Significant),
             size = 3.5, stroke = 1) +
  facet_wrap(~ Path_full, nrow = 1) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_shape_manual(
    values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)
  ) +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "black", "Not significant" = "white")) +
  labs(
    x = "Year", y = "Standardized Coefficient (β)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 13, margin = margin(b = 10)
    ),
    strip.background = element_rect(fill = "gray85", color = "gray50"),
    # strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3.5)),
    shape = guide_legend(override.aes = list(size = 3.5))
  )

# 拓展 ----
# 定义TPB结构方程模型
tpb_sem_model <- '
  # 测量模型
  SN  =~ pr_atten + if_no_ws_guilty + regulate_law + regulate_commu_rule + 
         if_neighbor_ws + if_family_ws
  PBC =~ if_know_method + if_sign_sort + category_trouble + time_cost_troub

  # 结构路径
  wil_of_engage ~ ws_attitude + SN + PBC
  reuse_bag + save_energy + seper_recyc ~ wil_of_engage
  
  # 局部误差协方差
  category_trouble ~~ time_cost_troub
  if_neighbor_ws ~~ if_family_ws
'

# 定义有序变量
ordered_vars <- c(
  "reuse_bag", 
  "ws_attitude", "threat", "pr_atten", "if_no_ws_guilty", "regulate_law", "regulate_commu_rule", "if_neighbor_ws", "if_family_ws", "if_know_method", "if_sign_sort", "category_trouble", "time_cost_troub", "wil_of_engage"
)

# 拟合多组SEM（配置不变性模型）
fit_sem <- sem(
  model = tpb_sem_model,
  data = ws_full %>% filter(!is.na(ws_full$reuse_bag)),
  group = "year",
  estimator = "WLSMV",
  ordered = ordered_vars,
  parameterization = "theta"
)
lavInspect(fit_sem, "cor.lv")

# 查看拟合指数
fit_indices <- fitMeasures(fit_sem, c("chisq", "df", "pvalue", "cfi", 
                                      "tli", "rmsea", "srmr"))
print(round(fit_indices, 3))

# 提取路径系数

# 提取标准化参数估计
std_estimates <- parameterEstimates(fit_sem, standardized = TRUE)

# 筛选回归路径
path_results <- std_estimates %>%
  filter(op == "~", !is.na(std.all)) %>%
  select(group, lhs, rhs, est, se, z, pvalue, std.all)

# 添加年份标签
year_labels <- c("2019", "2020", "2021", "2022", "2023")
path_results$year <- year_labels[path_results$group]

# 显示结果
cat("\n各年份路径系数:\n")
print(path_results %>% select(year, lhs, rhs, std.all, pvalue))

# ============================================================================
# 绘制路径系数随年份变化图 ----
# ============================================================================
plot_data <- path_results %>%
  filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
  mutate(
    # 路径标签（英文，简短）
    Path = case_when(
      lhs == "wil_of_engage" & rhs == "ws_attitude" ~ "ws_attitude → BI",
      lhs == "wil_of_engage" & rhs == "SN" ~ "SN → BI",
      lhs == "wil_of_engage" & rhs == "PBC" ~ "PBC → BI",
      lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "BI → BEH",
      TRUE ~ "Other"
    ),
    # 路径标签（完整，用于facet标题）
    Path_full = case_when(
      lhs == "wil_of_engage" & rhs == "ws_attitude" ~ "ws_attitude → Intention",
      lhs == "wil_of_engage" & rhs == "SN" ~ "Subjective Norm → Intention",
      lhs == "wil_of_engage" & rhs == "PBC" ~ "Perceived Control → Intention",
      lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "Intention → Behavior",
      TRUE ~ "Other"
    ),
    # 显著性（二分类）
    Significant = ifelse(pvalue < 0.05, "Significant (p < 0.05)", "Not significant"),
    Significant = factor(Significant, levels = c("Significant (p < 0.05)", "Not significant")),
    
    # 年份数值
    year_num = as.numeric(year),
    
    # 文本标签
    sig_label = ifelse(pvalue < 0.05, "*", "")
  ) %>%
  filter(Path != "Other") %>%
  # 设置facet顺序
  mutate(Path_full = factor(Path_full, levels = c(
    "ws_attitude → Intention",
    "Subjective Norm → Intention", 
    "Perceived Control → Intention",
    "Intention → Behavior"
  )))


ggplot(plot_data, aes(x = year_num, y = std.all)) +
  geom_hline(
    yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4
  ) +
  geom_line(linewidth = 0.9, color = "grey") +
  geom_point(aes(fill = Significant, shape = Significant),
             size = 3.5, stroke = 1) +
  facet_wrap(~ Path_full, nrow = 1) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_shape_manual(
    values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)
  ) +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "black", "Not significant" = "white")) +
  labs(
    x = "Year", y = "Standardized Coefficient (β)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 13, margin = margin(b = 10)
    ),
    strip.background = element_rect(fill = "gray85", color = "gray50"),
    # strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3.5)),
    shape = guide_legend(override.aes = list(size = 3.5))
  )

