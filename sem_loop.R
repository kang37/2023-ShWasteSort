# ============================================================================
# SEM模型循环测试：测试ATT、PBC、SN的所有观测变量组合
# ============================================================================

library(lavaan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# ============================================================================
# 1. 定义候选变量 ----
# ============================================================================

# ATT（态度）候选观测变量
att_candidates <- c(
  "satis_way_of_commu",  # 对社区垃圾收集方式满意度
  "satis_way_of_sh",     # 对上海市垃圾分类方式满意度
  "ws_attitude",         # 对垃圾分类态度
  "threat"               # 垃圾成为环保大问题
)

# PBC（知觉行为控制）候选观测变量
pbc_candidates <- c(
  "if_know_method",      # 知道方法就会分类
  "if_sign_sort",        # 标识清楚会投放
  "category_trouble",    # 分类种类多觉得麻烦
  "time_cost_troub"      # 时间成本麻烦
)

# SN（主观规范）候选观测变量
sn_candidates <- c(
  "pr_atten",            # 公众宣传提高关注度
  "if_no_ws_guilty",     # 不分类会内疚
  "regulate_law",        # 法律法规约束作用
  "regulate_commu_rule", # 小区规范约束作用
  "if_neighbor_ws",      # 邻居分类我也会做
  "if_family_ws"         # 家人分类我也会做
)

# ============================================================================
# 2. 生成所有可能的变量组合 ----
# ============================================================================

generate_combinations <- function(vars, min_size = 2) {
  all_combs <- list()
  for (k in min_size:length(vars)) {
    combs <- combn(vars, k, simplify = FALSE)
    all_combs <- c(all_combs, combs)
  }
  return(all_combs)
}

# 选择配置。
att_combinations <- generate_combinations(att_candidates, min_size = 2)
pbc_combinations <- generate_combinations(pbc_candidates, min_size = 2)
sn_combinations <- generate_combinations(sn_candidates, min_size = 2)

# 或更改配置：ATT2-3个观测变量，PBC3-4个，SN2个。
att_combinations <- generate_combinations(att_candidates, min_size = 2)
att_combinations <- att_combinations[sapply(att_combinations, length) <= 3]

pbc_combinations <- generate_combinations(pbc_candidates, min_size = 3)
pbc_combinations <- pbc_combinations[sapply(pbc_combinations, length) <= 4]

sn_combinations <- generate_combinations(sn_candidates, min_size = 2)
sn_combinations <- sn_combinations[sapply(sn_combinations, length) <= 2]

# 生成所有模型组合（三维笛卡尔积）
all_model_combinations <- expand.grid(
  att_id = seq_along(att_combinations),
  pbc_id = seq_along(pbc_combinations),
  sn_id = seq_along(sn_combinations),
  stringsAsFactors = FALSE
)

cat("\n", strrep("=", 80), "\n")
cat("SEM模型循环测试（ATT + PBC + SN组合）\n")
cat(strrep("=", 80), "\n")
cat("候选变量组合数:\n")
cat("  - ATT组合数:", length(att_combinations), "\n")
cat("  - PBC组合数:", length(pbc_combinations), "\n")
cat("  - SN组合数:", length(sn_combinations), "\n")
cat("  - 总模型数:", nrow(all_model_combinations), "\n")
cat(strrep("=", 80), "\n\n")

# 如果模型数量太多，给出警告
if (nrow(all_model_combinations) > 1000) {
  cat("⚠️  警告：模型数量较多（", nrow(all_model_combinations), "个）\n")
  cat("预计运行时间：", round(nrow(all_model_combinations) * 10 / 60, 1), "分钟\n")
  cat("建议：先测试部分组合，或增加min_size限制\n\n")
  
  # 提供限制选项
  cat("如需限制模型数量，可以修改以下代码：\n")
  cat("# 例如：只测试每个潜变量有3-4个观测变量的组合\n")
  cat("# att_combinations <- att_combinations[sapply(att_combinations, length) %in% 3:4]\n")
  cat("# pbc_combinations <- pbc_combinations[sapply(pbc_combinations, length) %in% 2:3]\n")
  cat("# sn_combinations <- sn_combinations[sapply(sn_combinations, length) %in% 3:5]\n\n")
  
  readline(prompt = "按Enter继续，或按Ctrl+C取消")
}

# ============================================================================
# 3. 构建SEM模型字符串函数 ----
# ============================================================================

build_sem_model <- function(att_vars, pbc_vars, sn_vars) {
  # 测量模型
  att_formula <- paste("ATT =~", paste(att_vars, collapse = " + "))
  sn_formula  <- paste("SN  =~", paste(sn_vars, collapse = " + "))
  pbc_formula <- paste("PBC =~", paste(pbc_vars, collapse = " + "))
  
  # 结构路径（固定）
  structural_paths <- "
  # 结构路径
  wil_of_engage ~ ATT + SN + PBC
  seper_recyc ~ wil_of_engage"
  
  # 局部误差协方差（根据变量存在与否添加）
  error_cov <- character(0)
  
  # ATT内部误差协方差
  if (all(c("satis_way_of_commu", "satis_way_of_sh") %in% att_vars)) {
    error_cov <- c(error_cov, "satis_way_of_commu ~~ satis_way_of_sh")
  }
  
  # PBC内部误差协方差
  if (all(c("category_trouble", "time_cost_troub") %in% pbc_vars)) {
    error_cov <- c(error_cov, "category_trouble ~~ time_cost_troub")
  }
  
  # SN内部误差协方差
  if (all(c("if_neighbor_ws", "if_family_ws") %in% sn_vars)) {
    error_cov <- c(error_cov, "if_neighbor_ws ~~ if_family_ws")
  }
  
  # 合并所有部分
  error_cov_section <- if (length(error_cov) > 0) {
    paste("\n  \n  # 局部误差协方差\n  ", paste(error_cov, collapse = "\n  "))
  } else {
    ""
  }
  
  model_string <- paste0(
    "  # 测量模型\n  ",
    att_formula, "\n  ",
    sn_formula, "\n  ",
    pbc_formula, "\n",
    structural_paths,
    error_cov_section
  )
  
  return(model_string)
}

# ============================================================================
# 4. 拟合单个模型并提取结果 ----
# ============================================================================

fit_single_model <- function(model_id, att_vars, pbc_vars, sn_vars, data, 
                             ordered_vars_all) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("正在测试模型", model_id, "/", nrow(all_model_combinations), "\n")
  cat("ATT变量:", paste(att_vars, collapse = ", "), "\n")
  cat("PBC变量:", paste(pbc_vars, collapse = ", "), "\n")
  cat("SN变量:", paste(sn_vars, collapse = ", "), "\n")
  cat(strrep("=", 80), "\n")
  
  # 构建模型字符串
  model_string <- build_sem_model(att_vars, pbc_vars, sn_vars)
  
  # 确定有序变量（只包含模型中实际使用的变量）
  model_vars <- c(att_vars, pbc_vars, sn_vars)
  ordered_vars_used <- intersect(ordered_vars_all, model_vars)
  
  # 捕获警告和错误
  warnings_list <- character(0)
  error_msg <- NA_character_
  fit_result <- NULL
  fit_result_converged <- FALSE
  
  # 尝试拟合模型
  tryCatch({
    fit_result <- withCallingHandlers(
      sem(
        model = model_string,
        data = data,
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
  }, error = function(e) {
    error_msg <<- conditionMessage(e)
    cat("❌ 拟合过程发生错误:", conditionMessage(e), "\n")
  })
  
  # 检查模型是否成功拟合并收敛
  fit_indices <- NULL
  path_results <- NULL
  
  if (!is.null(fit_result)) {
    # 检查收敛状态
    converged <- FALSE
    tryCatch({
      converged <- lavInspect(fit_result, "converged")
    }, error = function(e) {
      converged <<- FALSE
      warnings_list <<- c(warnings_list, paste("无法检查收敛状态:", conditionMessage(e)))
    })
    
    if (converged) {
      # 模型收敛，提取拟合指数
      tryCatch({
        fit_indices <- fitMeasures(fit_result, c(
          "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"
        ))
        
        # 提取路径系数
        std_estimates <- parameterEstimates(fit_result, standardized = TRUE)
        path_results <- std_estimates %>%
          filter(op == "~", !is.na(std.all)) %>%
          select(group, lhs, rhs, est, se, z, pvalue, std.all)
        
        year_labels <- c("2019", "2020", "2021", "2022", "2023")
        path_results$year <- year_labels[path_results$group]
        
        fit_result_converged <- TRUE
        cat("✓ 模型成功收敛\n")
        
      }, error = function(e) {
        cat("⚠️  模型收敛但提取结果时出错:", conditionMessage(e), "\n")
        warnings_list <<- c(warnings_list, paste("提取结果错误:", conditionMessage(e)))
        fit_result_converged <<- FALSE
      })
      
    } else {
      cat("⚠️  模型未收敛\n")
      warnings_list <- c(warnings_list, "模型未收敛")
      fit_result_converged <- FALSE
    }
    
  } else {
    cat("❌ 模型拟合失败\n")
  }
  
  # 返回结果
  return(list(
    model_id = model_id,
    att_vars = att_vars,
    pbc_vars = pbc_vars,
    sn_vars = sn_vars,
    model_string = model_string,
    fit_indices = fit_indices,
    path_results = path_results,
    warnings = warnings_list,
    error = error_msg,
    converged = fit_result_converged
  ))
}

# ============================================================================
# 5. 绘制路径系数变化图 ----
# ============================================================================

plot_path_coefficients <- function(path_results, model_id, att_vars, pbc_vars, sn_vars) {
  
  if (is.null(path_results) || nrow(path_results) == 0) {
    return(NULL)
  }
  
  plot_data <- path_results %>%
    filter(lhs %in% c("wil_of_engage", "seper_recyc")) %>%
    mutate(
      Path_full = case_when(
        lhs == "wil_of_engage" & rhs == "ATT" ~ "Attitude → Intention",
        lhs == "wil_of_engage" & rhs == "SN" ~ "Subjective Norm → Intention",
        lhs == "wil_of_engage" & rhs == "PBC" ~ "Perceived Control → Intention",
        lhs == "seper_recyc" & rhs == "wil_of_engage" ~ "Intention → Behavior",
        TRUE ~ "Other"
      ),
      Significant = ifelse(
        pvalue < 0.05, 
        "Significant (p < 0.05)", 
        "Not significant"
      ),
      Significant = factor(
        Significant, 
        levels = c("Significant (p < 0.05)", "Not significant")
      ),
      year_num = as.numeric(year)
    ) %>%
    filter(Path_full != "Other") %>%
    mutate(Path_full = factor(Path_full, levels = c(
      "Attitude → Intention",
      "Subjective Norm → Intention", 
      "Perceived Control → Intention",
      "Intention → Behavior"
    )))
  
  # 绘图
  p <- ggplot(plot_data, aes(x = year_num, y = std.all)) +
    geom_hline(
      yintercept = 0, linetype = "dashed", 
      color = "gray40", linewidth = 0.4
    ) +
    geom_line(linewidth = 0.9, color = "grey") +
    geom_point(
      aes(fill = Significant, shape = Significant),
      size = 3.5, stroke = 1
    ) +
    facet_wrap(~ Path_full, nrow = 1) +
    scale_x_continuous(breaks = 2019:2023) +
    scale_shape_manual(
      values = c("Significant (p < 0.05)" = 21, "Not significant" = 21)
    ) +
    scale_fill_manual(
      values = c("Significant (p < 0.05)" = "black", "Not significant" = "white")
    ) +
    labs(
      title = paste0(
        "Model ", model_id, 
        "\nATT: ", paste(att_vars, collapse = ", "),
        "\nSN: ", paste(sn_vars, collapse = ", "),
        "\nPBC: ", paste(pbc_vars, collapse = ", ")
      ),
      x = "Year", 
      y = "Standardized Coefficient (β)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(
        hjust = 0.5, face = "bold", size = 9, margin = margin(b = 10)
      ),
      strip.background = element_rect(fill = "gray85", color = "gray50"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5)
    ) +
    guides(
      fill = guide_legend(override.aes = list(size = 3.5)),
      shape = guide_legend(override.aes = list(size = 3.5))
    )
  
  return(p)
}

# ============================================================================
# 6. 主循环：测试所有模型 ----
# ============================================================================

# 定义所有有序变量
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

# 检查变量是否存在
check_vars <- c(att_candidates, pbc_candidates, sn_candidates,
                "wil_of_engage", "seper_recyc")
missing_vars <- setdiff(check_vars, names(ws_full))

if (length(missing_vars) > 0) {
  cat("\n⚠️  警告：以下变量在ws_full数据中不存在:\n")
  print(missing_vars)
  cat("\n请检查变量名是否正确！\n")
  stop("缺少必要变量，无法继续运行。")
} else {
  cat("\n✓ 所有变量检查通过！\n\n")
}

# 初始化结果列表
all_results <- list()
all_plots <- list()

# 初始化计数器
n_converged <- 0
n_failed <- 0

# 循环测试所有模型
for (i in 1:nrow(all_model_combinations)) {
  
  att_id <- all_model_combinations$att_id[i]
  pbc_id <- all_model_combinations$pbc_id[i]
  sn_id <- all_model_combinations$sn_id[i]
  
  att_vars <- att_combinations[[att_id]]
  pbc_vars <- pbc_combinations[[pbc_id]]
  sn_vars <- sn_combinations[[sn_id]]
  
  # 拟合模型
  result <- fit_single_model(
    model_id = i,
    att_vars = att_vars,
    pbc_vars = pbc_vars,
    sn_vars = sn_vars,
    data = ws_full,
    ordered_vars_all = ordered_vars_all
  )
  
  all_results[[i]] <- result
  
  # 绘制图形（仅对成功拟合的模型）
  if (result$converged) {
    n_converged <- n_converged + 1
    
    plot <- plot_path_coefficients(
      result$path_results, i, att_vars, pbc_vars, sn_vars
    )
    all_plots[[i]] <- plot
    
    # 打印拟合指数
    cat("\n拟合指数:\n")
    print(round(result$fit_indices, 3))
    
    # 打印警告（如果有）
    if (length(result$warnings) > 0) {
      cat("\n⚠️  警告信息:\n")
      for (w in result$warnings) {
        cat("  -", w, "\n")
      }
    }
  } else {
    n_failed <- n_failed + 1
    
    if (!is.na(result$error)) {
      cat("\n❌ 错误:", result$error, "\n")
    }
    if (length(result$warnings) > 0) {
      cat("\n⚠️  警告信息:\n")
      for (w in result$warnings) {
        cat("  -", w, "\n")
      }
    }
  }
  
  # 进度显示
  if (i %% 50 == 0 || i == nrow(all_model_combinations)) {
    cat("\n", strrep("-", 80), "\n")
    cat(sprintf("进度: %d/%d (%.1f%%), 成功: %d, 失败: %d\n", 
                i, nrow(all_model_combinations), 
                i/nrow(all_model_combinations)*100,
                n_converged, n_failed))
    cat(strrep("-", 80), "\n")
  }
}

# ============================================================================
# 7. 汇总结果（添加模型字符串列）----
# ============================================================================

# 创建汇总表
summary_table <- map_dfr(all_results, function(res) {
  
  if (res$converged && !is.null(res$fit_indices)) {
    tibble(
      model_id = res$model_id,
      att_vars = paste(res$att_vars, collapse = "; "),
      pbc_vars = paste(res$pbc_vars, collapse = "; "),
      sn_vars = paste(res$sn_vars, collapse = "; "),
      n_att = length(res$att_vars),
      n_pbc = length(res$pbc_vars),
      n_sn = length(res$sn_vars),
      model_formula = res$model_string,  # ⭐ 新增：模型公式字符串
      chisq = res$fit_indices["chisq"],
      df = res$fit_indices["df"],
      pvalue = res$fit_indices["pvalue"],
      cfi = res$fit_indices["cfi"],
      tli = res$fit_indices["tli"],
      rmsea = res$fit_indices["rmsea"],
      srmr = res$fit_indices["srmr"],
      n_warnings = length(res$warnings),
      has_pos_def_warning = any(grepl(
        "not positive definite|singular|Heywood|converge", 
        res$warnings, ignore.case = TRUE
      )),
      converged = TRUE
    )
  } else {
    tibble(
      model_id = res$model_id,
      att_vars = paste(res$att_vars, collapse = "; "),
      pbc_vars = paste(res$pbc_vars, collapse = "; "),
      sn_vars = paste(res$sn_vars, collapse = "; "),
      n_att = length(res$att_vars),
      n_pbc = length(res$pbc_vars),
      n_sn = length(res$sn_vars),
      model_formula = res$model_string,  # ⭐ 即使失败也保存模型公式
      chisq = NA, df = NA, pvalue = NA,
      cfi = NA, tli = NA, rmsea = NA, srmr = NA,
      n_warnings = length(res$warnings),
      has_pos_def_warning = NA,
      converged = FALSE
    )
  }
})

# 按拟合优度排序
summary_table_sorted <- summary_table %>%
  filter(converged, !has_pos_def_warning) %>%
  arrange(desc(cfi), desc(tli), rmsea)

cat("\n", strrep("=", 80), "\n")
cat("所有模型汇总（按CFI、TLI降序、RMSEA升序排列）\n")
cat(strrep("=", 80), "\n\n")
cat(sprintf("总模型数: %d\n", nrow(summary_table)))
cat(sprintf("成功收敛: %d (%.1f%%)\n", n_converged, n_converged/nrow(summary_table)*100))
cat(sprintf("未收敛/失败: %d (%.1f%%)\n\n", n_failed, n_failed/nrow(summary_table)*100))

# 显示前20个最佳模型（不显示model_formula，因为太长）
print(summary_table_sorted %>% 
        select(-model_formula) %>%  # 显示时先隐藏公式列
        head(20))

# 保存结果
write.csv(
  summary_table, 
  "data_proc/sem_model_comparison_full.csv", 
  row.names = FALSE
)

write.csv(
  summary_table_sorted, 
  "data_proc/sem_model_comparison_best.csv", 
  row.names = FALSE
)

cat("\n✓ 汇总表已保存（包含model_formula列）\n")

# ============================================================================
# 8. 输出最佳模型详细信息 ----
# ============================================================================

if (nrow(summary_table_sorted) > 0) {
  best_model_id <- summary_table_sorted$model_id[1]
  best_result <- all_results[[best_model_id]]
  
  cat("\n", strrep("=", 80), "\n")
  cat("最佳模型（Model", best_model_id, "）详细信息\n")
  cat(strrep("=", 80), "\n\n")
  
  cat("模型字符串:\n")
  cat(best_result$model_string, "\n\n")
  
  cat("拟合指数:\n")
  print(round(best_result$fit_indices, 3))
  
  cat("\n路径系数（标准化）:\n")
  print(best_result$path_results %>% 
          select(year, lhs, rhs, std.all, pvalue))
  
  # 保存最佳模型的图
  if (!is.null(all_plots[[best_model_id]])) {
    ggsave(
      filename = "data_proc/best_model_path_plot.pdf",
      plot = all_plots[[best_model_id]],
      width = 12, height = 4
    )
    cat("\n✓ 最佳模型图已保存\n")
  }
}

# ============================================================================
# 9. 保存所有成功模型的图形 ----
# ============================================================================

valid_plots <- all_plots[!sapply(all_plots, is.null)]

if (length(valid_plots) > 0) {
  pdf("data_proc/all_model_path_plots.pdf", width = 12, height = 4)
  for (p in valid_plots) {
    print(p)
  }
  dev.off()
  
  cat("\n✓ 所有模型路径系数图已保存到: data_proc/all_model_path_plots.pdf\n")
}

# ============================================================================
# 10. 最终总结 ----
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("分析完成！\n")
cat(strrep("=", 80), "\n")
cat("统计信息:\n")
cat(sprintf("  - 测试模型总数: %d\n", nrow(all_model_combinations)))
cat(sprintf("  - 成功收敛: %d (%.1f%%)\n", n_converged, n_converged/nrow(all_model_combinations)*100))
cat(sprintf("  - 未收敛/失败: %d (%.1f%%)\n\n", n_failed, n_failed/nrow(all_model_combinations)*100))

cat("输出文件:\n")
cat("  - 汇总表(完整): data_proc/sem_model_comparison_full.csv\n")
cat("  - 汇总表(最佳): data_proc/sem_model_comparison_best.csv\n")
cat("  - 最佳模型图:   data_proc/best_model_path_plot.pdf\n")
cat("  - 所有模型图:   data_proc/all_model_path_plots.pdf\n")
cat(strrep("=", 80), "\n")

# 打印前5个最佳模型（不含公式）
if (nrow(summary_table_sorted) > 0) {
  cat("\n前5个最佳模型:\n")
  print(summary_table_sorted %>% 
          select(model_id, cfi, tli, rmsea, n_att, n_pbc, n_sn) %>% 
          head(5))
  
  cat("\n提示：模型公式字符串已保存在CSV文件的model_formula列中\n")
}

# ============================================================================
# 11. 查看特定模型公式的辅助函数 ----
# ============================================================================

view_model_formula <- function(model_id) {
  cat("\n", strrep("=", 80), "\n")
  cat("模型", model_id, "的公式:\n")
  cat(strrep("=", 80), "\n\n")
  cat(all_results[[model_id]]$model_string, "\n\n")
}

cat("\n使用 view_model_formula(模型编号) 可以查看特定模型的公式\n")
cat("例如: view_model_formula(1)\n")
