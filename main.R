# Statement ----
# 上海市垃圾分类处理。

# Preparation ----
library(tidyverse)
library(patchwork)
library(ggplot2)
library(corrplot)
library(showtext)
showtext::showtext_auto()

# Data ----
#数据清理-统一列名
get_colname <- function(x){ # x为路径字符串
  df <- readxl::read_excel(x)
  df_col <- colnames(df)
  return (df_col) #返回列名
}

# 23年数据
col_23 <- get_colname("./data_raw/SHWS2023.xlsx") #取出23年列名
class(col_23)
col_name23 <- data.frame(org_name = c(col_23),new_name = NA) #添加空列

# TD: 所有类似代码都用管道符改写。
namelist23 <- read_csv("./data_proc/namelist23.csv")
ws_23 <- readxl::read_excel("./data_raw/SHWS2023.xlsx") %>% 
  rename_with(~c(namelist23$new_name))
#查看变量分布
lapply(ws_23[, c(
  "transac_secondhand",	
  "freq_online_secondhand",	
  "sens_of_oblg", 
  "volun_expr",	
  "sort_self",	
  "put_self")],
  table, useNA = "ifany")

# 22年数据
col_22 <- get_colname("./data_raw/SHWS2022.xlsx") 
col_name22 = data.frame(org_name = c(col_22),new_name = NA) #添加空列

ws_22 <- readxl::read_excel("./data_raw/SHWS2022.xlsx")
namelist22 <- read_csv("./data_proc/namelist22.csv")
ws_22 <- rename_with(ws_22, ~c(namelist22$new_name)) #改变列名

# 21年数据
col_21 <- get_colname("./data_raw/SHWS2021.xlsx") 
col_name21 = data.frame(org_name = c(col_21),new_name = NA) #添加空列

ws_21 <- readxl::read_excel("./data_raw/SHWS2021.xlsx")
namelist21 <- read_csv("./data_proc/namelist21.csv")
ws_21 <- rename_with(ws_21, ~c(namelist21$new_name)) #改变列名

# 20年数据
col_20 <- get_colname("./data_raw/SHWS2020.xlsx") 
col_name20 = data.frame(org_name = c(col_20),new_name = NA) #添加空列

ws_20 <- readxl::read_excel("./data_raw/SHWS2020.xlsx")
namelist20 <- read_csv("./data_proc/namelist20.csv")
ws_20 <- rename_with(ws_20, ~c(namelist20$new_name)) #改变列名

#19年数据
col_19 <- get_colname("./data_raw/SHWS2019.xlsx") 
col_name19 = data.frame(org_name = c(col_19),new_name = NA) #添加空列

ws_19 <- readxl::read_excel("./data_raw/SHWS2019.xlsx")
namelist19 <- read_csv("./data_proc/namelist19.csv")
ws_19 <- rename_with(ws_19, ~c(namelist19$new_name)) #改变列名

#替换-3（跳过）和0（未填答）-2（空答案）为空值NA
ws_23[ws_23==-3|ws_23==0|ws_23==-2] <- NA
ws_22[ws_22==-3|ws_22==0|ws_22==-2] <- NA
ws_21[ws_21==-3|ws_21==0|ws_21==-2] <- NA
ws_20[ws_20==-3|ws_20==0|ws_20==-2] <- NA
ws_19[ws_19==-3|ws_19==0|ws_19==-2] <- NA


#增加年份变量
ws_23$year <- 2023
ws_22$year <- 2022
ws_21$year <- 2021
ws_20$year <- 2020
ws_19$year <- 2019

#合并数据
#求列名交集
share_colname <- intersect(colnames(ws_19), colnames(ws_20))
share_colname <- intersect(share_colname,colnames(ws_21))
share_colname <- intersect(share_colname,colnames(ws_22))
share_colname <- intersect(share_colname,colnames(ws_23))
share_colname

ws_all <- rbind(select(ws_19, share_colname),select(ws_20, share_colname),
                select(ws_21, share_colname),select(ws_22, share_colname),
                select(ws_23, share_colname))

#将年份转换为因子变量
ws_all$year <- as.factor(ws_all$year)

#逆转ws_all中逆向的五分变量，改变后1=完全不同意，5=完全同意
lst_5to1 <- c(
  "satis_way_of_commu","satis_way_of_sh","wil_of_engage",
  "seper_recyc","scal_no_my_busi", "scal_pr_atten","scal_know_sort",
  "scal_sign_sort", "scal_tidy_sort","scal_near_sort", "scal_kind_trouble", 
  "scal_t_trouble", "scal_green_acc", "scal_guilty", "scal_law", "scal_fine",
  "scal_nb_WS","scal_family_WS", "scal_reward_gracc", "scal_reward"
) 
for (i in lst_5to1) {
  ws_all[i] <- 6 - ws_all[i]
}
ws_all

#逆转atti_WS（原本1=非常支持，5=非常反对，6=说不清；改后 不支持1-5支持，说不清存为NA
#新变量用新名字，以保留处理痕迹。
ws_all$atti_WS_rev <- ifelse(ws_all$atti_WS %in% 1:5,
                             6 - ws_all$atti_WS,
                             NA)

table(ws_all$atti_WS, ws_all$atti_WS_rev, useNA = "ifany")


#列出五年共同变量
nl_list <- list(namelist19, namelist20, namelist21, namelist22, namelist23) 
#nl=namelist
#按19年
common_nl <- Reduce(function(x, y) inner_join(x, y, by = names(x)[3]), nl_list)
head(common_nl)
common_nl <- common_nl[,2:3]
dim(common_nl)
head(common_nl)


# 创建所有数据框的列表（不包括 namelist23）
df_list <- list(namelist19, namelist20, namelist21, namelist22)

# 提取所有数据框中存在的 new_name
common_new_names <- Reduce(
  function(x, y) union(x, y), lapply(df_list, function(df) df$new_name)
)

# 筛选 namelist23 中独有的 new_name
unique_namelist23 <- namelist23 %>%
  filter(!new_name %in% common_new_names) %>%
  select(new_name, org_name)  # 只保留这两列

# 查看结果
print(unique_namelist23)

# ----23年数据进一步处理
#逆转ws_23中逆向的五分变量，改变后1=完全不同意，5=完全同意
lst_5to1 <- c(
  "satis_way_of_commu","satis_way_of_sh","wil_of_engage",
  "seper_recyc","scal_no_my_busi", "scal_pr_atten","scal_know_sort",
  "scal_sign_sort", "scal_tidy_sort","scal_near_sort", "scal_kind_trouble", 
  "scal_t_trouble", "scal_green_acc", "scal_guilty", "scal_law", "scal_fine",
  "scal_nb_WS","scal_family_WS", "scal_reward_gracc", "scal_reward"
) 
for (i in lst_5to1) {
  ws_23[i] <- 6 - ws_23[i]
}
ws_23

#继续逆转逆向的五分变量，改变后1=完全不同意，5=完全同意/符合
ws_23 <- ws_23 |>
  dplyr::mutate(
    use_bag = 6 - use_bag,
    energy_sens = 6 - energy_sens,
    save_res = 6 - save_res
  )
#查看频率分布
prop.table(table(ws_23$use_bag))
prop.table(table(ws_23$energy_sens))
prop.table(table(ws_23$save_res))

#改写部分是否变量为10
ws_23$transac_secondhand <- ifelse(ws_23$transac_secondhand == 2, 0, ws_23$transac_secondhand)
ws_23$volun_expr <- ifelse(ws_23$volun_expr == 2, 0, ws_23$volun_expr)
##查看
table(ws_23$transac_secondhand)
table(ws_23$volun_expr)

##改写多选题目的单个选项变量为10
ws_23$sort_self <- ifelse(is.na(ws_23$sort_self), 0, ws_23$sort_self)
ws_23$put_self <- ifelse(is.na(ws_23$put_self), 0, ws_23$put_self)

table(ws_23$sort_self)
table(ws_23$put_self)

#逆转atti_WS（原本1=非常支持，5=非常反对，6=说不清；改后 不支持1-5支持，说不清存为NA
#新变量用新名字_rev，以保留处理痕迹。
ws_23$atti_WS_rev <- ifelse(ws_23$atti_WS %in% 1:5,
                             6 - ws_23$atti_WS,
                             NA)
#检查修改前后的分布
table(ws_23$atti_WS, ws_23$atti_WS_rev, useNA = "ifany") 

## 查看部分23年变量分布（23独有+人口数）
lapply(ws_23[, c(
  "transac_secondhand",	
  "freq_online_secondhand",	
  "sens_of_oblg", 
  "volun_expr",	
  "sort_self",	
  "put_self",
  "num_family")],
  table, useNA = "ifany")

##23家庭人数柱状图
library(ggplot2)

ggplot(ws_23, aes(x = factor(num_family))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "家庭人数", y = "频数", title = "家庭人数的分布") +
  theme_minimal()


# Description ----
## Basic demo info ----
# 各年份人口基本属性饼图矩阵

# 定义变量
demo_vars <- c("gender", "edu", "mon_income_fam", "fam_regis")
demo_labels <- c(
  gender = "性别", edu = "教育程度", occu = "职业",
  mon_income_fam = "家庭月收入", fam_regis = "户口"
)

# 类别标签
category_labels <- list(
  gender = c("1" = "男", "2" = "女"),
  edu = c("1" = "小学", "2" = "初中", "3" = "高中", "4" = "大专", 
          "5" = "本科", "6" = "硕士", "7" = "博士"),
  mon_income_fam = c("1" = "<3K", "2" = "3-5K", "3" = "5-8K",
                     "4" = "8-12K", "5" = "12-20K", "6" = "20-30K", "7" = ">30K"),
  fam_regis = c("1" = "上海", "2" = "非上海")
)

# 获取年份
years <- sort(unique(ws_all$year))

# 设置输出（增加分辨率和尺寸）
png(filename = "demo_pie_charts.png", 
    width = 2000, height = 1500, res = 200)  # 增加宽度和高度

# 设置紧凑布局
par(mfrow = c(length(demo_vars), length(years)), 
    mar = c(0.2, 0.2, 2, 0.2),  # 大幅减小边距：下、左、上、右
    oma = c(1, 1, 2.5, 0.5))    # 减小外边距：下、左、上、右

# 颜色方案
get_colors <- function(var, n) {
  switch(var,
         gender = c("#3498DB", "#E74C3C")[1:n],
         edu = c("#E8F5E9", "#C8E6C9", "#A5D6A7", "#81C784", 
                 "#66BB6A", "#4CAF50", "#43A047")[1:n],
         mon_income_fam = c("#FFF3E0", "#FFE0B2", "#FFCC80", "#FFB74D",
                            "#FFA726", "#FF9800", "#FB8C00")[1:n],
         rainbow(n)
  )
}

# 绘制饼图
for (i in 1:length(demo_vars)) {
  var <- demo_vars[i]
  
  for (j in 1:length(years)) {
    year <- years[j]
    data <- ws_all[ws_all$year == year & !is.na(ws_all[[var]]), var]
    
    if (length(data) > 0) {
      freq <- table(data)
      pct <- prop.table(freq) * 100
      
      # 应用标签
      if (var %in% names(category_labels)) {
        lab_map <- category_labels[[var]]
        names(freq) <- ifelse(names(freq) %in% names(lab_map),
                              lab_map[names(freq)], names(freq))
      }
      
      # 绘图（增大radius，让饼图充满空间）
      pie(freq, 
          labels = sprintf("%s\n%.1f%%", names(freq), pct),
          col = get_colors(var, length(freq)),
          main = sprintf("%d年 %s (N=%d)", year, demo_labels[var], sum(freq)),
          cex.main = 0.85,    # 标题字体略小
          cex = 0.65,         # 标签字体略小
          border = "white",
          radius = 1.0)       # 增大半径，让饼图更大
    } else {
      plot.new()
      text(0.5, 0.5, "无数据", cex = 1, col = "gray50")
      title(main = sprintf("%d年 %s", year, demo_labels[var]), cex.main = 0.85)
    }
  }
}

# 总标题
mtext("各年份人口基本属性分布", outer = TRUE, side = 3, 
      cex = 1.4, font = 2, line = 0.8)

# 关闭设备
dev.off()

# 恢复默认设置
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

## Occu and resi area ----
# ============================================================================
# 职业和居住区域各年份条形图矩阵
# ============================================================================

# 定义变量
bar_vars <- c("occu", "resi_area")
bar_labels <- c(
  occu = "职业",
  resi_area = "居住区域"
)

# 类别标签（可根据实际情况调整）
bar_category_labels <- list(
  occu = c(
    "1" = "机关事业", "2" = "企业", "3" = "个体", 
    "4" = "学生", "5" = "退休", "6" = "无业", "7" = "其他"
  ),
  resi_area = c(
    "1" = "浦东", "2" = "黄浦", "3" = "静安", "4" = "徐汇",
    "5" = "长宁", "6" = "普陀", "7" = "虹口", "8" = "杨浦",
    "9" = "宝山", "10" = "闵行", "11" = "嘉定", "12" = "松江",
    "13" = "青浦", "14" = "奉贤", "15" = "金山", "16" = "崇明"
  )
)

# 获取年份
years <- sort(unique(ws_all$year))

# 设置输出
png(filename = "demo_bar_charts.png", 
    width = 2000, height = 800, res = 150)

# 设置布局（2行×5列）
par(mfrow = c(length(bar_vars), length(years)), 
    mar = c(3, 3, 2.5, 0.5),
    oma = c(2, 2, 2.5, 0.5))

# 颜色方案
get_bar_colors <- function(var, n) {
  switch(var,
         occu = colorRampPalette(c("#E3F2FD", "#1976D2"))(n),
         resi_area = colorRampPalette(c("#FFF3E0", "#E65100"))(n),
         rainbow(n)
  )
}

# 绘制条形图
for (i in 1:length(bar_vars)) {
  var <- bar_vars[i]
  
  for (j in 1:length(years)) {
    year <- years[j]
    data <- ws_all[ws_all$year == year & !is.na(ws_all[[var]]), var]
    
    if (length(data) > 0) {
      freq <- table(data)
      
      # 应用标签
      if (var %in% names(bar_category_labels)) {
        lab_map <- bar_category_labels[[var]]
        names(freq) <- ifelse(names(freq) %in% names(lab_map),
                              lab_map[names(freq)], names(freq))
      }
      
      # 绘制条形图
      bp <- barplot(freq,
                    col = get_bar_colors(var, length(freq)),
                    border = "white",
                    las = 2,  # 标签垂直
                    cex.names = 0.6,
                    ylim = c(0, max(freq) * 1.15),
                    main = sprintf("%d年 %s (N=%d)", year, bar_labels[var], sum(freq)),
                    cex.main = 0.85,
                    ylab = "",
                    xlab = "")
      
      # 在条形上添加数值
      text(bp, freq, labels = freq, pos = 3, cex = 0.6, col = "black")
      
      # 添加网格线
      grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")
      
    } else {
      plot.new()
      text(0.5, 0.5, "无数据", cex = 1.2, col = "gray50")
      title(main = sprintf("%d年 %s", year, bar_labels[var]), cex.main = 0.85)
    }
  }
}

# 总标题
mtext("各年份职业与居住区域分布", outer = TRUE, side = 3, 
      cex = 1.4, font = 2, line = 0.8)

# 添加Y轴标签
mtext("频数", outer = TRUE, side = 2, cex = 1, line = 0.5)

# 关闭设备
dev.off()

# 恢复默认设置
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# Willingness and act ----
# 定义变量标签映射
var_labels <- c(
  # 意愿
  wil_of_engage = "参与意愿",
  wil_share_info = "分享信息意愿",
  
  # 行动
  seper_recyc = "分类回收行为",
  freq_share_info = "分享信息频率",
  
  # 其他
  satis_way_of_commu = "社区方式满意度",
  satis_way_of_sh = "上海方式满意度",
  know_method_sort = "了解分类方法",
  know_WSprcs = "了解处理流程",
  sense_of_role = "角色认知/作用感",
  import_share_info = "信息分享重要性",
  interest_WS = "垃圾分类兴趣",
  benefit_WS = "分类受益感知",
  loss_WS = "分类受损感知"
)

# 绘图代码
tar_col <- c(
  "wil_of_engage", "wil_share_info", 
  "seper_recyc", "freq_share_info", 
  "satis_way_of_commu","satis_way_of_sh",
  "know_method_sort","know_WSprcs","sense_of_role",
  "import_share_info", "interest_WS", 
  "benefit_WS","loss_WS"
)

# 如果回答是1-5的李克特量表，使用发散型配色更有意义
ws_all %>% 
  select("year", tar_col) %>% 
  pivot_longer(
    cols = all_of(tar_col), 
    names_to = "question", 
    values_to = "reply"
  ) %>% 
  group_by(year, question, reply) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(question = factor(question, levels = tar_col)) %>% 
  ggplot() + 
  geom_col(aes(year, n, fill = as_factor(reply)), position = "fill") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +  # 红黄蓝发散
  theme_bw() +
  facet_wrap(. ~ question, labeller = labeller(question = var_labels)) +
  labs(fill = "回答值",
       x = "年份",
       y = "比例") +
  theme(
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Correlation ----
# 定义简短的中文标签
var_labels_short <- c(
  wil_of_engage = "参与意愿",
  wil_share_info = "分享意愿",
  seper_recyc = "分类行为",
  freq_share_info = "分享频率",
  satis_way_of_commu = "社区满意",
  satis_way_of_sh = "上海满意",
  know_method_sort = "分类知识",
  know_WSprcs = "流程了解",
  sense_of_role = "作用感知",
  import_share_info = "信息重要性",
  interest_WS = "分类兴趣",
  benefit_WS = "受益感知",
  loss_WS = "受损感知"
)

# 计算相关系数和p值
ws_all_sub_cor <- ws_all %>% 
  select(tar_col) %>% 
  cor(method = "spearman", use = "pairwise.complete.obs")

# 修改行列名为简短中文标签
rownames(ws_all_sub_cor) <- var_labels_short[tar_col]
colnames(ws_all_sub_cor) <- var_labels_short[tar_col]

ws_all_sub_p <- cor.mtest(select(ws_all, tar_col), method = "spearman")

# 自定义颜色：绿色（负相关）-> 白色（0）-> 橙色（正相关）
col_custom <- colorRampPalette(c(
  "#2E7D32",    # 深绿
  "#66BB6A",    # 中绿
  "#E8F5E9",    # 浅绿
  "#FFE0B2",    # 浅橙
  "#FF9800",    # 中橙
  "#E65100"     # 深橙
))(200)

# 绘制相关性图
corrplot(
  ws_all_sub_cor, 
  method = "circle",           # 使用圆圈
  type = "upper",              # 只显示上三角
  col = col_custom,            # 自定义颜色
  p.mat = ws_all_sub_p$p,      # p值矩阵
  sig.level = 0.05,            # 显著性水平
  insig = "blank",             # 不显著的直接留白（不显示）
  tl.col = "black",            # 标签颜色
  tl.srt = 45,                 # 标签旋转45度
  tl.cex = 0.9,                # 标签大小
  addCoef.col = "black",       # 添加相关系数数值（仅显著的）
  number.cex = 0.6,            # 数值大小
  number.digits = 2,           # 保留2位小数
  diag = FALSE,                # 不显示对角线
  cl.pos = "r",                # 图例位置（右侧）
  cl.cex = 0.8                 # 图例大小
)


# 参加志愿活动与否对态度、行为的影响。
ggplot(
  ws_23_sub %>% filter(!is.na(volun_val)), 
  aes(as_factor(volun_val), wil_act_val)
) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.05) + 
  facet_grid(volun ~ wil_act) + 
  theme_bw() + 
  labs(x = "Volunteer", y = "Will or Action") 

# SEM ----
# ============================================================================
# 基于计划行为理论(TPB)的结构方程模型分析
# 请确保已安装以下R包：lavaan, semPlot, tidyverse, psych
# 安装命令: install.packages(c("lavaan", "semPlot", "tidyverse", "psych"))
# ============================================================================

# 加载必要的包
library(lavaan)
library(semPlot)
library(tidyverse)
library(psych)

cat("计划行为理论(TPB)结构方程模型分析\n")

# ============================================================================
# 1. 数据准备
# ============================================================================

cat("【步骤1】数据准备\n")
cat("--------------------------------------------------------------------------------\n")


cat(sprintf("原始数据维度: %d × %d\n", nrow(ws_all), ncol(ws_all)))

# 选择TPB模型所需变量
tpb_vars <- c(
  # 态度 (Attitude)
  "interest_WS",        # 垃圾分类兴趣
  "benefit_WS",         # 受益感知
  "loss_WS",            # 受损感知
  
  # 主观规范 (Subjective Norm)
  "import_share_info",  # 信息重要性
  "sense_of_role",      # 作用感知
  
  # 知觉行为控制 (Perceived Behavioral Control)
  "know_method_sort",   # 分类知识
  "know_WSprcs",        # 流程了解
  "satis_way_of_commu", # 社区满意度
  
  # 行为意愿 (Intention)
  "wil_of_engage",      # 参与意愿
  "wil_share_info",     # 分享意愿
  
  # 行为 (Behavior)
  "seper_recyc",        # 分类行为
  "freq_share_info"     # 分享频率
)

# 提取并清理数据
tpb_data <- ws_all %>%
  select(all_of(tpb_vars)) %>%
  na.omit()

cat(sprintf("清理后数据维度: %d × %d\n", nrow(tpb_data), ncol(tpb_data)))
cat(sprintf("有效样本量: %d\n\n", nrow(tpb_data)))

# 描述性统计
cat("变量描述性统计:\n")
print(summary(tpb_data))
cat("\n")

# ============================================================================
# 2. 信度分析
# ============================================================================

cat("\n【步骤2】信度分析\n")
cat("--------------------------------------------------------------------------------\n")

# 定义各潜变量的观测变量
attitude_items <- c("interest_WS", "benefit_WS", "loss_WS")
subjnorm_items <- c("import_share_info", "sense_of_role")
pbc_items <- c("know_method_sort", "know_WSprcs", "satis_way_of_commu")
intention_items <- c("wil_of_engage", "wil_share_info")
behavior_items <- c("seper_recyc", "freq_share_info")

# 计算Cronbach's Alpha
cat("Cronbach's Alpha:\n")
cat(sprintf("  态度 (Attitude):         %.3f\n", 
            alpha(tpb_data[, attitude_items])$total$raw_alpha))
cat(sprintf("  主观规范 (SubjNorm):     %.3f\n", 
            alpha(tpb_data[, subjnorm_items])$total$raw_alpha))
cat(sprintf("  知觉行为控制 (PBC):      %.3f\n", 
            alpha(tpb_data[, pbc_items])$total$raw_alpha))
cat(sprintf("  行为意愿 (Intention):    %.3f\n", 
            alpha(tpb_data[, intention_items])$total$raw_alpha))
cat(sprintf("  行为 (Behavior):         %.3f\n", 
            alpha(tpb_data[, behavior_items])$total$raw_alpha))
cat("\n判断标准: Alpha > 0.70 可接受, > 0.80 良好\n\n")

# ============================================================================
# 3. 构建测量模型 (CFA)
# ============================================================================

cat("\n【步骤3】验证性因子分析 (CFA)\n")
cat("--------------------------------------------------------------------------------\n")

# 定义测量模型
tpb_measurement <- '
  # 潜变量定义
  Attitude =~ interest_WS + benefit_WS + loss_WS
  SubjNorm =~ import_share_info + sense_of_role
  PBC =~ know_method_sort + know_WSprcs + satis_way_of_commu
  Intention =~ wil_of_engage + wil_share_info
  Behavior =~ seper_recyc + freq_share_info
'

cat("拟合测量模型...\n")
fit_cfa <- cfa(tpb_measurement, 
               data = tpb_data,
               estimator = "MLR",
               std.lv = TRUE)

cat("✓ 测量模型拟合完成\n\n")

# 输出拟合指数
cat("【测量模型拟合指数】\n")
fit_measures <- fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", 
                                       "tli", "rmsea", "srmr"))
print(fit_measures)
cat("\n拟合标准:\n")
cat("  CFI/TLI: > 0.90 可接受, > 0.95 良好\n")
cat("  RMSEA: < 0.08 可接受, < 0.05 良好\n")
cat("  SRMR: < 0.08 可接受\n\n")

# 因子载荷
cat("【因子载荷】\n")
loadings <- standardizedSolution(fit_cfa) %>%
  filter(op == "=~") %>%
  select(潜变量 = lhs, 观测变量 = rhs, 标准化载荷 = est.std)
print(loadings)
cat("\n")

# ============================================================================
# 4. 构建结构模型 (SEM)
# ============================================================================

cat("\n【步骤4】结构方程模型 (SEM)\n")
cat("--------------------------------------------------------------------------------\n")

# 定义完整的结构方程模型
tpb_model <- '
  # 测量模型
  Attitude =~ interest_WS + benefit_WS + loss_WS
  SubjNorm =~ import_share_info + sense_of_role
  PBC =~ know_method_sort + know_WSprcs + satis_way_of_commu
  Intention =~ wil_of_engage + wil_share_info
  Behavior =~ seper_recyc + freq_share_info
  
  # 结构模型（TPB理论路径）
  Intention ~ Attitude + SubjNorm + PBC
  Behavior ~ Intention + PBC
'

cat("拟合结构模型...\n")
fit_sem <- sem(tpb_model, 
               data = tpb_data,
               estimator = "MLR",
               std.lv = TRUE)

cat("✓ 结构模型拟合完成\n\n")

# 模型摘要
cat("【完整模型摘要】\n")
summary(fit_sem, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
cat("\n")

# ============================================================================
# 5. 提取关键结果
# ============================================================================

cat("\n【步骤5】关键结果提取\n")
cat("--------------------------------------------------------------------------------\n")

# 拟合指数
cat("【拟合指数】\n")
fit_measures_sem <- fitMeasures(fit_sem, c("chisq", "df", "pvalue", 
                                           "cfi", "tli", "rmsea", 
                                           "rmsea.ci.lower", "rmsea.ci.upper",
                                           "srmr", "aic", "bic"))
print(round(fit_measures_sem, 3))
cat("\n")

# 路径系数
cat("【路径系数】\n")
path_coefs <- standardizedSolution(fit_sem) %>%
  filter(op == "~") %>%
  select(因变量 = lhs, 自变量 = rhs, 
         非标准化系数 = est, 标准化系数 = est.std, 
         标准误 = se, Z值 = z, p值 = pvalue) %>%
  mutate(显著性 = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**",
    pvalue < 0.05 ~ "*",
    TRUE ~ ""
  ))

print(path_coefs)
cat("\n显著性: *** p<0.001, ** p<0.01, * p<0.05\n\n")

# R²值
cat("【解释变异量 R²】\n")
r2_values <- inspect(fit_sem, "r2")
print(r2_values)
cat("\n")

# ============================================================================
# 6. 间接效应分析
# ============================================================================

cat("\n【步骤6】间接效应分析\n")
cat("--------------------------------------------------------------------------------\n")

# 定义间接效应
tpb_model_indirect <- '
  # 测量模型
  Attitude =~ interest_WS + benefit_WS + loss_WS
  SubjNorm =~ import_share_info + sense_of_role
  PBC =~ know_method_sort + know_WSprcs + satis_way_of_commu
  Intention =~ wil_of_engage + wil_share_info
  Behavior =~ seper_recyc + freq_share_info
  
  # 结构模型
  Intention ~ b1*Attitude + b2*SubjNorm + b3*PBC
  Behavior ~ b4*Intention + b5*PBC
  
  # 间接效应
  indirect_att := b1 * b4
  indirect_sn := b2 * b4
  indirect_pbc := b3 * b4
  
  # 总效应
  total_att := b1 * b4
  total_sn := b2 * b4
  total_pbc := b5 + (b3 * b4)
'

fit_indirect <- sem(tpb_model_indirect, 
                    data = tpb_data,
                    estimator = "MLR",
                    std.lv = TRUE)

cat("【间接效应和总效应】\n")
effects <- parameterEstimates(fit_indirect, standardized = TRUE) %>%
  filter(op == ":=") %>%
  select(效应 = lhs, 估计值 = est, 标准化值 = std.all, 
         标准误 = se, Z值 = z, p值 = pvalue)

print(effects)
cat("\n")

# ============================================================================
# 7. 可视化
# ============================================================================

cat("\n【步骤7】模型可视化\n")
cat("--------------------------------------------------------------------------------\n")

# 保存路径图1 - 详细版
png("tpb_model_detailed.png", width = 3000, height = 2400, res = 300)

semPaths(fit_sem,
         what = "std",              # 标准化系数
         layout = "tree2",          # 树状布局
         rotation = 2,              # 旋转
         edge.label.cex = 1.1,      # 路径系数字体大小
         node.label.cex = 1,        # 节点标签大小
         sizeMan = 7,               # 观测变量大小
         sizeLat = 11,              # 潜变量大小
         edge.color = "black",      # 路径颜色
         style = "lisrel",          # LISREL风格
         curve = 2,                 # 曲线程度
         fade = FALSE,
         residuals = FALSE,
         intercepts = FALSE,
         thresholds = FALSE,
         nCharNodes = 0)            # 显示完整标签

title("计划行为理论(TPB)结构方程模型 - 详细版", 
      line = 1, cex.main = 1.5)

dev.off()
cat("✓ 详细路径图已保存: tpb_model_detailed.png\n")

# 保存路径图2 - 简化版（只显示潜变量关系）
png("tpb_model_simple.png", width = 2400, height = 1800, res = 300)

semPaths(fit_sem,
         what = "std",
         layout = "spring",
         whatLabels = "std",
         edge.label.cex = 1.3,
         sizeMan = 0,               # 不显示观测变量
         sizeLat = 15,              # 放大潜变量
         edge.color = "steelblue",
         edge.width = 2,
         style = "ram",
         curve = 1.5,
         fade = FALSE,
         residuals = FALSE,
         intercepts = FALSE,
         nCharNodes = 0)

title("TPB模型 - 潜变量关系", line = 1, cex.main = 1.6)

dev.off()
cat("✓ 简化路径图已保存: tpb_model_simple.png\n")

# 保存路径图3 - 学术出版风格
png("tpb_model_publication.png", width = 3200, height = 2400, res = 300)

semPaths(fit_sem,
         what = "std",
         layout = "tree2",
         rotation = 2,
         edge.label.cex = 1.2,
         node.label.cex = 1,
         sizeMan = 6,
         sizeLat = 10,
         color = list(lat = "lightblue", man = "white"),
         edge.color = "black",
         borders = TRUE,
         bg = "white",
         style = "lisrel",
         curve = 1.8,
         fade = FALSE,
         residuals = FALSE,
         intercepts = FALSE,
         thresholds = FALSE)

title("Theory of Planned Behavior - Structural Equation Model", 
      line = 1, cex.main = 1.4)

dev.off()
cat("✓ 出版风格路径图已保存: tpb_model_publication.png\n\n")

# ============================================================================
# 8. 结果导出
# ============================================================================

cat("\n【步骤8】结果导出\n")
cat("--------------------------------------------------------------------------------\n")

# 导出路径系数为CSV
write.csv(path_coefs, "tpb_path_coefficients.csv", row.names = FALSE)
cat("✓ 路径系数已导出: tpb_path_coefficients.csv\n")

# 导出间接效应为CSV
write.csv(effects, "tpb_indirect_effects.csv", row.names = FALSE)
cat("✓ 间接效应已导出: tpb_indirect_effects.csv\n")

# 保存完整的模型摘要
sink("tpb_model_summary.txt")
cat("================================================================================\n")
cat("计划行为理论(TPB)结构方程模型 - 完整摘要\n")
cat("================================================================================\n\n")
summary(fit_sem, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
sink()
cat("✓ 完整摘要已保存: tpb_model_summary.txt\n")

# 保存R对象
save(fit_cfa, fit_sem, fit_indirect, tpb_data, 
     file = "tpb_model_results.RData")
cat("✓ R对象已保存: tpb_model_results.RData\n\n")

# ============================================================================
# 9. 模型评估总结
# ============================================================================

cat("\n【步骤9】模型评估总结\n")
cat("--------------------------------------------------------------------------------\n")

# 评估模型拟合度
cfi <- fit_measures_sem["cfi"]
tli <- fit_measures_sem["tli"]
rmsea <- fit_measures_sem["rmsea"]
srmr <- fit_measures_sem["srmr"]

cat("拟合度评估:\n")
cat(sprintf("  CFI = %.3f  %s\n", cfi, 
            ifelse(cfi > 0.95, "[优秀]", 
                   ifelse(cfi > 0.90, "[良好]", "[需改进]"))))
cat(sprintf("  TLI = %.3f  %s\n", tli, 
            ifelse(tli > 0.95, "[优秀]", 
                   ifelse(tli > 0.90, "[良好]", "[需改进]"))))
cat(sprintf("  RMSEA = %.3f  %s\n", rmsea, 
            ifelse(rmsea < 0.05, "[优秀]", 
                   ifelse(rmsea < 0.08, "[良好]", "[需改进]"))))
cat(sprintf("  SRMR = %.3f  %s\n", srmr, 
            ifelse(srmr < 0.08, "[良好]", "[需改进]"))))

cat("\n路径分析结果:\n")
sig_paths <- path_coefs %>% filter(p值 < 0.05)
cat(sprintf("  显著路径数: %d / %d\n", nrow(sig_paths), nrow(path_coefs)))

cat("\n解释力度:\n")
cat(sprintf("  Intention的R² = %.3f (解释了%.1f%%的变异)\n", 
            r2_values["Intention"], r2_values["Intention"]*100))
cat(sprintf("  Behavior的R² = %.3f (解释了%.1f%%的变异)\n", 
            r2_values["Behavior"], r2_values["Behavior"]*100))

