# ============================================================================
# SEM单模型测试
# ============================================================================

library(lavaan)
library(dplyr)
library(ggplot2)

# ============================================================================
# 1. 指定观测变量 ----
# ============================================================================

att_vars <- c("satis_way_of_sh", "ws_attitude")
sn_vars  <- c("pr_atten", "regulate_law", "regulate_commu_rule",
               "if_neighbor_ws", "if_family_ws")
pbc_vars <- c("if_know_method", "if_sign_sort", "category_trouble")

# ============================================================================
# 2. 构建模型字符串 ----
# ============================================================================

build_sem_model <- function(att_vars, pbc_vars, sn_vars) {
  att_formula <- paste("ATT =~", paste(att_vars, collapse = " + "))
  sn_formula  <- paste("SN  =~", paste(sn_vars, collapse = " + "))
  pbc_formula <- paste("PBC =~", paste(pbc_vars, collapse = " + "))

  structural_paths <- "
  # 结构路径
  wil_of_engage ~ ATT + SN + PBC
  seper_recyc ~ wil_of_engage"

  # 局部误差协方差（根据变量存在与否添加）
  error_cov <- character(0)

  if (all(c("satis_way_of_commu", "satis_way_of_sh") %in% att_vars)) {
    error_cov <- c(error_cov, "satis_way_of_commu ~~ satis_way_of_sh")
  }
  if (all(c("category_trouble", "time_cost_troub") %in% pbc_vars)) {
    error_cov <- c(error_cov, "category_trouble ~~ time_cost_troub")
  }
  if (all(c("if_neighbor_ws", "if_family_ws") %in% sn_vars)) {
    error_cov <- c(error_cov, "if_neighbor_ws ~~ if_family_ws")
  }

  error_cov_section <- if (length(error_cov) > 0) {
    paste("\n  \n  # 局部误差协方差\n  ", paste(error_cov, collapse = "\n  "))
  } else {
    ""
  }

  paste0(
    "  # 测量模型\n  ",
    att_formula, "\n  ",
    sn_formula, "\n  ",
    pbc_formula, "\n",
    structural_paths,
    error_cov_section
  )
}

model_string <- build_sem_model(att_vars, pbc_vars, sn_vars)

cat("模型公式:\n")
cat(model_string, "\n\n")

# ============================================================================
# 3. 定义有序变量并检查 ----
# ============================================================================

ordered_vars_all <- c(
  "satis_way_of_commu", "satis_way_of_sh",
  "ws_attitude", "threat",
  "pr_atten", "if_no_ws_guilty",
  "regulate_law", "regulate_commu_rule",
  "if_neighbor_ws", "if_family_ws",
  "if_know_method", "if_sign_sort",
  "category_trouble", "time_cost_troub",
  "wil_of_engage"
)

model_vars <- c(att_vars, pbc_vars, sn_vars)
ordered_vars_used <- intersect(ordered_vars_all, model_vars)

check_vars <- c(model_vars, "wil_of_engage", "seper_recyc")
missing_vars <- setdiff(check_vars, names(ws_full))

if (length(missing_vars) > 0) {
  cat("以下变量在ws_full数据中不存在:\n")
  print(missing_vars)
  stop("缺少必要变量，无法继续运行。")
} else {
  cat("所有变量检查通过！\n\n")
}

# ============================================================================
# 4. 拟合模型 ----
# ============================================================================

cat("正在拟合模型...\n")

warnings_list <- character(0)

fit <- withCallingHandlers(
  sem(
    model = model_string,
    data = ws_full,
    group = "year",
    estimator = "WLSMV",
    ordered = ordered_vars_used,
    parameterization = "theta"
  ),
  warning = function(w) {
    warnings_list <<- c(warnings_list, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

if (length(warnings_list) > 0) {
  cat("\n警告信息:\n")
  for (w in warnings_list) cat("  -", w, "\n")
}

# ============================================================================
# 5. 查看结果 ----
# ============================================================================

if (lavInspect(fit, "converged")) {
  cat("\n模型成功收敛！\n\n")

  # 拟合指数
  fit_indices <- fitMeasures(fit, c(
    "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"
  ))
  cat("拟合指数:\n")
  print(round(fit_indices, 3))

  # 路径系数
  std_estimates <- parameterEstimates(fit, standardized = TRUE)
  path_results <- std_estimates %>%
    filter(op == "~", !is.na(std.all)) %>%
    select(group, lhs, rhs, est, se, z, pvalue, std.all)

  year_labels <- c("2019", "2020", "2021", "2022", "2023")
  path_results$year <- year_labels[path_results$group]

  cat("\n路径系数（标准化）:\n")
  print(path_results %>% select(year, lhs, rhs, std.all, pvalue))
  # 输出结果。
  write.csv(
    path_results %>% select(year, lhs, rhs, std.all, pvalue), 
    "data_proc/sem_res.csv"
  )

  # ============================================================================
  # 6. 绘制路径系数变化图 ----
  # ============================================================================

  plot_data <- path_results %>%
    filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
    mutate(
      Path_full = case_when(
        lhs == "wil_of_engage" & rhs == "ATT" ~ "Attitude -> Intention",
        lhs == "wil_of_engage" & rhs == "SN"  ~ "Subjective Norm -> Intention",
        lhs == "wil_of_engage" & rhs == "PBC" ~ "Perceived Control -> Intention",
        lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "Intention -> Behavior",
        TRUE ~ "Other"
      ),
      Significant = factor(
        ifelse(pvalue < 0.05, "Significant (p < 0.05)", "Not significant"),
        levels = c("Significant (p < 0.05)", "Not significant")
      ),
      year_num = as.numeric(year)
    ) %>%
    filter(Path_full != "Other") %>%
    mutate(Path_full = factor(Path_full, levels = c(
      "Attitude -> Intention",
      "Subjective Norm -> Intention",
      "Perceived Control -> Intention",
      "Intention -> Behavior"
    )))

  path_colors <- c(
    "Attitude -> Intention"           = "#4DBB15",
    "Subjective Norm -> Intention"    = "#4DBBD5",
    "Perceived Control -> Intention"  = "#00A087",
    "Intention -> Behavior"           = "#E64B35"
  )

  plot_data <- plot_data %>%
    mutate(
      point_fill = ifelse(
        pvalue < 0.05,
        path_colors[as.character(Path_full)],
        "white"
      )
    )

  p <- ggplot(plot_data, aes(x = year_num, y = std.all, color = Path_full)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(fill = point_fill, shape = Significant), size = 3.5, stroke = 1) +
    facet_wrap(~ Path_full, ncol = 1) +
    scale_x_continuous(breaks = 2019:2023) +
    scale_color_manual(values = path_colors, guide = "none") +
    scale_shape_manual(values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)) +
    scale_fill_identity() +
    labs(
      # title = paste0(
      #   "ATT: ", paste(att_vars, collapse = ", "),
      #   "\nSN: ", paste(sn_vars, collapse = ", "),
      #   "\nPBC: ", paste(pbc_vars, collapse = ", ")
      # ),
      x = "Year",
      y = "Standardized Coefficient"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 9, margin = margin(b = 10)),
      axis.text.x = element_text(angle = 90), 
      strip.background = element_rect(fill = "gray85", color = "gray50"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
    ) +
    guides(
      shape = guide_legend(override.aes = list(
        size = 3.5, fill = c("black", "white"), color = "black"
      ))
    ) 

  print(p)

  ggsave("data_proc/single_model_path_plot.pdf", plot = p, width = 12, height = 4)
  cat("\n图已保存到: data_proc/single_model_path_plot.pdf\n")

} else {
  cat("\n模型未收敛。\n")
}
