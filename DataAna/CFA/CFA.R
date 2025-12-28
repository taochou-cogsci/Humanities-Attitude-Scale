# 如果没安装，请先安装
# install.packages(c("readxl","lavaan","semTools","psych","dplyr","tidyr","performance"))
# install.packages("semPlot")
# 加载
library(readxl)
library(lavaan)
library(semTools)
library(psych)
library(dplyr)
library(tidyr)
library(performance)  # 用于额外拟合指标
library(semPlot)

folder <- "D:/桌面/SBTHW/心理测量/final project/DataAna/CFA"
file   <- file.path(folder, "CFA_DATA_tree.xlsx")

df <- read_excel(file)

# 快速看前几列与维度变量是否存在
head(df)
colnames(df)


factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F5 = c("IHAS32", "IHAS34", "IHAS35", "IHAS37"),
  F2 = c("IHAS12", "IHAS13", "IHAS14", "IHAS15"),
  F3 = c("IHAS18", "IHAS19", "IHAS20"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47"),
  F4 = c("IHAS22", "IHAS25", "IHAS26")
)

factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F5 = c("IHAS32", "IHAS34", "IHAS35", "IHAS37"),
  F2 = c("IHAS12", "IHAS13", "IHAS14", "IHAS15"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47")
)

factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F5 = c("IHAS32", "IHAS34", "IHAS37"),
  F2 = c("IHAS12", "IHAS13", "IHAS14", "IHAS15"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47")
)

factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F2 = c("IHAS12", "IHAS14", "IHAS15"),
  F3 = c("IHAS18", "IHAS19", "IHAS20"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47"),
  F4 = c("IHAS22", "IHAS25", "IHAS26")
)


factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F2 = c("IHAS12", "IHAS13", "IHAS14", "IHAS15"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47")
)

factor_items <- list(
  F1 = c("IHAS1", "IHAS2", "IHAS3", "IHAS5", "IHAS6", "IHAS7", "IHAS8"),
  F5 = c("IHAS32", "IHAS34", "IHAS35", "IHAS37"),
  F2 = c("IHAS12", "IHAS13", "IHAS14", "IHAS15"),
  F3 = c("IHAS18", "IHAS19", "IHAS20"),
  F6 = c("IHAS39", "IHAS40", "IHAS42", "IHAS47")
) # 最后确定要使用的factor_items

factor_items <- list(
  F1 = c("HAS1", "HAS2", "HAS3", "HAS4", "HAS5", "HAS6", "HAS7"),
  F2 = c("HAS8", "HAS9", "HAS10", "HAS11"),
  F3 = c("HAS12", "HAS13", "HAS14", "HAS15"),
  F4 = c("HAS16", "HAS17", "HAS18"),
  F5 = c("HAS19", "HAS20", "HAS21", "HAS22")
) # 用来画正式的树的factor_items

# 构造 model 字符串
model_lines <- sapply(names(factor_items), function(f){
  items <- factor_items[[f]]
  paste0(f, " =~ ", paste(items[items %in% colnames(df)], collapse = " + "))
})

model <- paste(model_lines, collapse = "\n")
cat("CFA model:\n", model, "\n")

# 列出所有用于 CFA 的条目（存在于 df 的）
cfa_items <- unlist(factor_items)
cfa_items <- cfa_items[cfa_items %in% colnames(df)]

# 若为有序 Likert，请将这些列视为 ordered
df[cfa_items] <- lapply(df[cfa_items], function(x) {
  as.ordered(as.numeric(x))
})


# 运行 CFA（WLSMV）
fit_wlsmv <- cfa(model,
                 data = df,
                 ordered = cfa_items,
                 estimator = "WLSMV",
                 missing = "pairwise") 

fit_wlsmv <- cfa(model,
                 data = df)  

semPaths(
  fit_wlsmv,
  what = "std",        # 画标准化载荷（论文必备）
  layout = "tree",
  style = "lisrel",
  residuals = FALSE,   # 一般论文里会关掉误差
  intercepts = FALSE,
  edge.label.cex = 0.9,
  sizeMan = 5,
  sizeLat = 7
)

std_loadings <- standardizedSolution(fit_wlsmv)

# 只看因子载荷（λ）
std_loadings_loadings <- subset(
  std_loadings,
  op == "=~"
)

std_loadings_loadings


inspect(fit_wlsmv, "theta")

lavaan::standardizedsolution(fit_wlsmv)

# 拟合指标（常报告）
fitMeasures(fit_wlsmv, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

# 标准化载荷与因子相关矩阵
summary(fit_wlsmv, standardized = TRUE, fit.measures = TRUE)

# 提取标准化因子载荷矩阵
inspect(fit_wlsmv, "std.lv")      # 标准化（latent variance=1）
# 或者
parameterEstimates(fit_wlsmv, standardized = TRUE) %>% 
  filter(op=="=~") %>% arrange(lhs)

# 最大的前 20 个修改指标（注意：仅作探索与理论检验，不要盲目按 MI 修改模型）
modif <- modificationIndices(fit_wlsmv)
modif %>% arrange(desc(mi)) %>% head(20)

# Cronbach's alpha (每因子)
for(f in names(factor_items)){
  its <- factor_items[[f]]
  its <- its[its %in% colnames(df)]
  if(length(its) >= 2){
    cat("\nFactor:", f, "items:", paste(its, collapse=", "), "\n")
    print(alpha(as.data.frame(df[its]))$total)  # alpha 输出
  }
}

# 基于 CFA 的 reliability (omega / CR)
# semTools::reliability 返回多种信度指标
semTools::reliability(fit_wlsmv)

print(1)


## 特别提醒：算信度时不能转成ordered类型的数据

alpha_results <- lapply(factor_items, function(items) {
  df_sub <- df %>% select(all_of(items))
  pc <- psych::polychoric(df_sub)$rho
  psych::alpha(pc, n.obs = nrow(df_sub))
})

alpha_table <- data.frame(
  Factor = names(alpha_results),
  N_items = sapply(factor_items, length),
  Alpha = sapply(alpha_results, function(x) round(x$total$raw_alpha, 3))
)

alpha_table

psych::alpha(df)



