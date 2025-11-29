# ============================================================================
# 多组结构方程模型：TPB路径系数年度变化分析
# ============================================================================

# 加载必要的包 ----
library(tidyverse)
library(lavaan)
library(ggplot2)

# 数据读取与准备 ----
# 读取各年份数据
ws_19 <- readxl::read_excel("./RawData/SHWS2019.xlsx")
ws_20 <- readxl::read_excel("./RawData/SHWS2020.xlsx")
ws_21 <- readxl::read_excel("./RawData/SHWS2021.xlsx")
ws_22 <- readxl::read_excel("./RawData/SHWS2022.xlsx")
ws_23 <- readxl::read_excel("./RawData/SHWS2023.xlsx")

# 读取列名映射
namelist19 <- read_csv("./ProcData/namelist19.csv")
namelist20 <- read_csv("./ProcData/namelist20.csv")
namelist21 <- read_csv("./ProcData/namelist21.csv")
namelist22 <- read_csv("./ProcData/namelist22.csv")
namelist23 <- read_csv("./ProcData/namelist23.csv")

# 统一列名
ws_19 <- rename_with(ws_19, ~c(namelist19$new_name))
ws_20 <- rename_with(ws_20, ~c(namelist20$new_name))
ws_21 <- rename_with(ws_21, ~c(namelist21$new_name))
ws_22 <- rename_with(ws_22, ~c(namelist22$new_name))
ws_23 <- rename_with(ws_23, ~c(namelist23$new_name))

# 处理缺失值
ws_19[ws_19 == -3 | ws_19 == 0 | ws_19 == -2] <- NA
ws_20[ws_20 == -3 | ws_20 == 0 | ws_20 == -2] <- NA
ws_21[ws_21 == -3 | ws_21 == 0 | ws_21 == -2] <- NA
ws_22[ws_22 == -3 | ws_22 == 0 | ws_22 == -2] <- NA
ws_23[ws_23 == -3 | ws_23 == 0 | ws_23 == -2] <- NA

# 添加年份变量
ws_19$year <- 2019
ws_20$year <- 2020
ws_21$year <- 2021
ws_22$year <- 2022
ws_23$year <- 2023

# 合并数据
share_colname <- Reduce(intersect, list(
  colnames(ws_19), colnames(ws_20), colnames(ws_21), 
  colnames(ws_22), colnames(ws_23)
))

ws_all <- rbind(
  select(ws_19, all_of(share_colname)),
  select(ws_20, all_of(share_colname)),
  select(ws_21, all_of(share_colname)),
  select(ws_22, all_of(share_colname)),
  select(ws_23, all_of(share_colname))
)

# 将年份转换为因子
ws_all$year <- as.factor(ws_all$year)

# 逆转编码
lst_5to1 <- c(
  "satis_way_of_commu", "satis_way_of_sh", "wil_of_engage", "seper_recyc",
  "scal_no_my_busi", "scal_pr_atten", "scal_know_sort", "scal_sign_sort",
  "scal_tidy_sort", "scal_near_sort", "scal_kind_trouble", "scal_t_trouble",
  "scal_green_acc", "scal_guilty", "scal_law", "scal_commu_rule", "scal_fine",
  "scal_nb_WS", "scal_family_WS", "scal_reward_gracc", "scal_reward"
)

for (i in lst_5to1) {
  ws_all[i] <- 6 - ws_all[i]
}

# 逆转态度变量
ws_all$atti_WS_rev <- ifelse(ws_all$atti_WS %in% 1:5, 6 - ws_all$atti_WS, NA)

cat(sprintf("样本量: %d (5年合计)\n", nrow(ws_all)))
cat(sprintf("各年份样本量:\n"))
print(table(ws_all$year))

# ============================================================================
# 多组结构方程模型 ----
# ============================================================================

# 定义TPB结构方程模型
tpb_sem_model <- '
  # 测量模型
  ATT =~ satis_way_of_commu + satis_way_of_sh + atti_WS_rev + scal_threat
  SN  =~ scal_pr_atten + scal_guilty + scal_law + scal_commu_rule + 
         scal_nb_WS + scal_family_WS
  PBC =~ scal_know_sort + scal_sign_sort + scal_kind_trouble + scal_t_trouble

  # 结构路径
  wil_of_engage ~ ATT + SN + PBC
  seper_recyc ~ wil_of_engage

  # 局部误差协方差
  satis_way_of_commu ~~ satis_way_of_sh
  scal_kind_trouble ~~ scal_t_trouble
  scal_nb_WS ~~ scal_family_WS
'

# 定义有序变量
ordered_vars <- c(
  "scal_threat", "scal_no_my_busi", "scal_know_sort", "scal_sign_sort",
  "scal_tidy_sort", "scal_near_sort", "scal_kind_trouble", "scal_t_trouble",
  "scal_pr_atten", "scal_guilty", "scal_law", "scal_commu_rule", "scal_fine",
  "scal_nb_WS", "scal_family_WS", "satis_way_of_commu", "satis_way_of_sh",
  "atti_WS_rev", "seper_recyc", "wil_of_engage"
)

# 拟合多组SEM（配置不变性模型）
fit_sem <- sem(
  model = tpb_sem_model,
  data = ws_all,
  group = "year",
  estimator = "WLSMV",
  ordered = ordered_vars,
  parameterization = "theta"
)

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

# 保存为CSV
# write.csv(path_results, "path_coefficients_by_year.csv", row.names = FALSE)

# ============================================================================
# 绘制路径系数随年份变化图 ----
# ============================================================================
plot_data <- path_results %>%
  filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
  mutate(
    # 路径标签（英文，简短）
    Path = case_when(
      lhs == "wil_of_engage" & rhs == "ATT" ~ "ATT → BI",
      lhs == "wil_of_engage" & rhs == "SN" ~ "SN → BI",
      lhs == "wil_of_engage" & rhs == "PBC" ~ "PBC → BI",
      lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "BI → BEH",
      TRUE ~ "Other"
    ),
    # 路径标签（完整，用于facet标题）
    Path_full = case_when(
      lhs == "wil_of_engage" & rhs == "ATT" ~ "Attitude → Intention",
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
    "Attitude → Intention",
    "Subjective Norm → Intention", 
    "Perceived Control → Intention",
    "Intention → Behavior"
  )))
ggplot(plot_data, aes(x = year_num, y = std.all)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_line(linewidth = 0.9, color = "grey") +
  geom_point(aes(fill = Significant, shape = Significant),
             size = 3.5, stroke = 1) +
  facet_wrap(~ Path_full, nrow = 1) +
  scale_x_continuous(breaks = 2019:2023) +
  scale_shape_manual(values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)) +
  scale_fill_manual(values = c("Significant (p < 0.05)" = "black", "Not significant" = "white")) +
  labs(
    title = "TPB Path Coefficients Over Time (2019-2023)",
    x = "Year",
    y = "Standardized Coefficient (β)"
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13, margin = margin(b = 10)),
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


# ============================================================================
# 输出数值汇总表 ----
# ============================================================================
summary_table <- plot_data %>%
  select(year, Path, std.all, pvalue, sig_label) %>%
  arrange(Path, year) %>%
  mutate(
    std.all = round(std.all, 3),
    pvalue = round(pvalue, 4)
  )

# 宽格式表格（便于比较）
summary_table %>%
  select(year, Path, std.all) %>%
  pivot_wider(names_from = year, values_from = std.all)


# 保存宽格式表
write.csv(wide_table, "path_coefficients_wide.csv", row.names = FALSE)

cat("\n")
cat("="*80, "\n", sep = "")
cat("分析完成！\n")
cat("="*80, "\n", sep = "")
cat("\n生成的文件:\n")
cat("  1. path_coefficients_by_year.csv - 详细路径系数表\n")
cat("  2. path_coefficients_wide.csv - 宽格式对比表\n")
cat("  3. path_coefficients_trend.png - 趋势图\n\n")




# 步骤1：拟合配置不变性模型（基线模型，路径可以不同）
fit_configural <- sem(
  model = tpb_sem_model,
  data = ws_all,
  group = "year",
  estimator = "WLSMV",
  ordered = ordered_vars,
  parameterization = "theta"
)

cat("✓ 配置不变性模型拟合完成\n")

# 步骤2：拟合结构路径不变性模型（约束模型，强制路径相等）
fit_constrained <- sem(
  model = tpb_sem_model,
  data = ws_all,
  group = "year",
  estimator = "WLSMV",
  ordered = ordered_vars,
  parameterization = "theta",
  group.equal = "regressions"  # ← 关键：强制所有回归路径跨年份相等
)

cat("✓ 路径不变性模型拟合完成\n")

# 步骤3：比较两个模型
cat("\n【整体路径不变性检验】\n")
comparison <- lavTestLRT(fit_configural, fit_constrained)
print(comparison)

# 提取关键信息
chisq_diff <- comparison$`Chisq diff`[2]
df_diff <- comparison$Df[2] - comparison$Df[1]
p_value <- comparison$`Pr(>Chisq)`[2]

cat("\n检验结果解读：\n")
cat(sprintf("  Δχ² = %.3f\n", chisq_diff))
cat(sprintf("  Δdf = %d\n", df_diff))
cat(sprintf("  p值 = %.5f\n", p_value))

if (p_value < 0.001) {
  cat("\n结论：路径系数在不同年份间存在极显著差异（p < 0.001）***\n")
} else if (p_value < 0.01) {
  cat("\n结论：路径系数在不同年份间存在显著差异（p < 0.01）**\n")
} else if (p_value < 0.05) {
  cat("\n结论：路径系数在不同年份间存在边缘显著差异（p < 0.05）*\n")
} else {
  cat("\n结论：路径系数在不同年份间无显著差异（p > 0.05）\n")
}

# ============================================================================
# 补充：逐条路径检验（更详细）
# ============================================================================

cat("\n\n【逐条路径不变性检验】\n")
cat("----------------------------------------------------------------\n")

# 定义要检验的路径
paths_to_test <- c(
  "wil_of_engage ~ ATT",
  "wil_of_engage ~ SN",
  "wil_of_engage ~ PBC",
  "seper_recyc ~ wil_of_engage"
)

path_names <- c(
  "ATT → BI",
  "SN → BI",
  "PBC → BI",
  "BI → BEH"
)

# 初始化结果表
invariance_results <- data.frame()

for (i in 1:length(paths_to_test)) {
  path <- paths_to_test[i]
  path_name <- path_names[i]
  
  cat(sprintf("\n正在检验路径：%s\n", path_name))
  
  # 拟合部分不变性模型（释放当前路径，其他路径保持相等）
  fit_partial <- sem(
    model = tpb_sem_model,
    data = ws_all,
    group = "year",
    estimator = "WLSMV",
    ordered = ordered_vars,
    parameterization = "theta",
    group.equal = "regressions",
    group.partial = path  # ← 释放这条路径
  )
  
  # 比较完全约束模型 vs 部分约束模型
  comp <- lavTestLRT(fit_constrained, fit_partial)
  
  chisq_diff <- comp$`Chisq diff`[2]
  df_diff <- comp$Df[2] - comp$Df[1]
  pval <- comp$`Pr(>Chisq)`[2]
  
  sig <- ifelse(pval < 0.001, "***",
                ifelse(pval < 0.01, "**",
                       ifelse(pval < 0.05, "*", "n.s.")))
  
  # 保存结果
  invariance_results <- rbind(invariance_results, data.frame(
    路径 = path_name,
    Δχ² = round(chisq_diff, 3),
    Δdf = df_diff,
    p值 = round(pval, 5),
    显著性 = sig,
    stringsAsFactors = FALSE
  ))
  
  cat(sprintf("  Δχ² = %.3f, Δdf = %d, p = %.5f %s\n", 
              chisq_diff, df_diff, pval, sig))
}

cat("\n【路径不变性检验汇总表】\n")
print(invariance_results)