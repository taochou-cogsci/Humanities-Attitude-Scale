# ========== 载入必要套件 ==========
# install.packages(c("readxl","dplyr","psych","GPArotation","openxlsx"))  # 若未安装请先取消注释运行
library(readxl)       # 用于读取 Excel
library(dplyr)        # 数据选择与操作
library(psych)        # EFA, polychoric, KMO, bartlett, omega 等
library(GPArotation)  # 旋转方法（oblimin, oblique 等）
library(openxlsx)     # 写 Excel（保存结果）

# ========== 1. 读取数据并选择目标列 ==========
path <- "D:/桌面/SBTHW/心理测量/final project/DataAna/EFA_DATA.xlsx" # <- 根据你本机路径修改
dat_all <- read_excel(path)                # 读取整个表格（包含 SubjID 等）
# 选择 SubjID 与 IHAS1-IHAS50 列（假设列名以 IHAS 开头）
dat_items <- dat_all %>%
  select(starts_with("IHAS"))     # 只保留 SubjID 和所有以 IHAS 开头的列
# 如果你确认列名是 IHAS1..IHAS50，也可以用 select(SubjID, IHAS1:IHAS50)

# ========== 2. 准备纯项目数据矩阵（去掉 SubjID） ==========
#item_df <- dat_items %>% select(-SubjID)  # 去掉被试编号，只保留项目分数用于分析
item_df <- dat_items

# ========== 3. 描述性检查（缺失、分布、简单统计） ==========
dim(item_df)                              # 行列数（样本数 × 题目数）
summary(item_df)                          # 每题的描述统计（min, max, mean, NA 等）
sapply(item_df, function(x) mean(is.na(x))) # 每题缺失比例

# ========== 4. 计算多项序列相关矩阵（polychoric） ==========
# polychoric 返回 list，rho 是相关矩阵；适用于有序分类型（Likert）
poly_res <- polychoric(item_df)  
R <- poly_res$rho   # 多项序列相关矩阵（用于后续 EFA）
ev <- eigen(R) # get eigenvalues
ev$values
                      
# ========== 5. KMO 与 Bartlett 检验（评估适合性） ==========
KMO_result <- KMO(R)                      # Kaiser-Meyer-Olkin 检验（采样适合度）
print(KMO_result)                         # 输出 KMO 总体与各题指标
bartlett_result <- cortest.bartlett(R, n = nrow(item_df))  # Bartlett 球形检验
print(bartlett_result)

# ========== 6. 决定因子数量（并行分析 + scree） ==========
fa.parallel(R, n.obs = nrow(item_df), fa = "fa") 
# 上面会画图并输出建议的因子数（基于并行分析与碎石图）
# 读输出选择合适的因子个数（例如下面设 nfactors <- 5）

# ========== 8) 运行 EFA（示例 nfactors = 5，可根据上步调整） ==========
nfactors <- 9   # <- 你依据 fa.parallel 输出调整这里
efa_res <- fa(r = R,
              n.obs = nrow(item_df),
              nfactors = nfactors,
              fm = "pa",          # principal axis 提取法，适合非正态/序数数据
              rotate = "oblimin") # 斜交旋转（允许因子相关）
warnings()
# 打印载荷（只显示绝对值 >= 0.30 的载荷）
print.psych(efa_res, cutoff=0.30)
print(efa_res$loadings, cutoff = 0.30)

