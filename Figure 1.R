BiocManager::install("ggtree")
BiocManager::install("treeio")
# 安装并加载ape包
if (!require("ape")) install.packages("ape")
library(ape)

# 读取NEXUS树文件
tree <- read.tree("Tempest5426iqtreefigtree.newick")  

# 获取所有节点数
Ntip <- length(tree$tip.label)
Nnode <- tree$Nnode
all_nodes <- 1:(Ntip + Nnode)

# 构建父节点查找表
parent_of <- rep(NA, Ntip + Nnode)
for(i in 1:nrow(tree$edge)) {
  parent_of[tree$edge[i,2]] <- tree$edge[i,1]
}

# 对每个叶节点，追溯到根
get_path_to_root <- function(node) {
  path <- node
  while(!is.na(parent_of[node])) {
    node <- parent_of[node]
    path <- c(node, path)
  }
  return(path)
}

# 生成所有叶节点的路径
paths <- lapply(1:Ntip, get_path_to_root)

# 转为数据框
maxlen <- max(sapply(paths, length))
path_strings <- sapply(paths, function(x) paste(x, collapse = ";"))
result <- data.frame(
  LeafNode = 1:Ntip,
  Taxlabel = tree$tip.label,
  PathToRoot = path_strings
)

# 导出为csv
write.csv(result, "leaf_paths_to_root.csv", row.names = FALSE)



# 加载必要的包
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggtree)
library(ape)
library(patchwork)

# 读取Excel文件
tree2 <- read.tree("Tempest5426iqtreefigtree.newick")
data <- read_excel("5426-genotype.xlsx")#读取宏文件

# 计算每个clade的样本数量
clade_counts <- data %>%
  count(clade) %>%
  arrange(desc(n))

# 创建自定义颜色映射
color_palette <- c()

# 为每个主要clade系列定义固定颜色范围
clade_colors <- list(
  I = colorRampPalette(c("#FFCCCB", "#B22222"))(11),       # 红色系列
  III = colorRampPalette(c("#90EE90", "#006400"))(3),      # 绿色系列
  IV = colorRampPalette(c("#FFFF99", "#FFD700"))(4),       # 黄色系列
  V = colorRampPalette(c("#D8BFD8", "#483D8B"))(10),       # 紫色系列
  VII = colorRampPalette(c("#D2B48C", "#8B4513"))(3)       # 棕色系列
)

# 为每个clade分配固定颜色
assign_colors <- function(clade_list, color_gradient) {
  colors <- setNames(color_gradient[1:length(clade_list)], clade_list)
  return(colors)
}

# 处理I系列 (固定红色渐变)
I_clades <- c("I", "I_A", "I_B", "I_C", "I_D", "I_E", "I_F", "I_G", "I_H", "I_J", "I_K")
I_clades <- intersect(I_clades, clade_counts$clade)
if(length(I_clades) > 0) {
  I_colors <- assign_colors(I_clades, clade_colors$I)
  color_palette <- c(color_palette, I_colors)
}

# 处理II (固定蓝色)
if("II" %in% clade_counts$clade) {
  color_palette <- c(color_palette, "II" = "#1E90FF")
}

# 处理III系列 (固定绿色渐变)
III_clades <- c("III", "III_A", "III_B")
III_clades <- intersect(III_clades, clade_counts$clade)
if(length(III_clades) > 0) {
  III_colors <- assign_colors(III_clades, clade_colors$III)
  color_palette <- c(color_palette, III_colors)
}

# 处理IV系列 (固定黄色渐变)
IV_clades <- c("IV", "IV_A", "IV_B", "IV_C")
IV_clades <- intersect(IV_clades, clade_counts$clade)
if(length(IV_clades) > 0) {
  IV_colors <- assign_colors(IV_clades, clade_colors$IV)
  color_palette <- c(color_palette, IV_colors)
}

# 处理V系列 (固定紫色渐变)
V_clades <- c("V", "V_A", "V_B", "V_C", "V_D", "V_E", "V_F", "V_G", "V_H", "V_J")
V_clades <- intersect(V_clades, clade_counts$clade)
if(length(V_clades) > 0) {
  V_colors <- assign_colors(V_clades, clade_colors$V)
  color_palette <- c(color_palette, V_colors)
}

# 处理VII系列 (固定棕色渐变)
VII_clades <- c("VII", "VII_A", "VII_B")
VII_clades <- intersect(VII_clades, clade_counts$clade)
if(length(VII_clades) > 0) {
  VII_colors <- assign_colors(VII_clades, clade_colors$VII)
  color_palette <- c(color_palette, VII_colors)
}

# 处理unassigned (灰色)
if("unassigned" %in% clade_counts$clade) {
  color_palette <- c(color_palette, "unassigned" = "grey50")
}

# 处理其他clade (使用独特颜色)
other_clades <- setdiff(clade_counts$clade, names(color_palette))
if(length(other_clades) > 0) {
  # 为其他clade生成独特颜色
  other_colors <- rainbow(length(other_clades), s = 0.7, v = 0.8)
  names(other_colors) <- other_clades
  color_palette <- c(color_palette, other_colors)
}

# 创建图例顺序（保持降序排列）
legend_order <- c(
  I_clades[order(-clade_counts$n[clade_counts$clade %in% I_clades])],
  "II",
  III_clades[order(-clade_counts$n[clade_counts$clade %in% III_clades])],
  IV_clades[order(-clade_counts$n[clade_counts$clade %in% IV_clades])],
  V_clades[order(-clade_counts$n[clade_counts$clade %in% V_clades])],
  VII_clades[order(-clade_counts$n[clade_counts$clade %in% VII_clades])],
  setdiff(other_clades, "unassigned"),
  "unassigned"
)

# 确保数据中的clade因子水平与图例顺序一致
data$clade <- factor(data$clade, levels = legend_order)

# --------------------------
# 第一部分：创建堆积条形图（显示样本总数）
# --------------------------
region_clade_counts <- data %>%
  group_by(Study.Region, clade) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Study.Region) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()

# 创建显示总数的标签数据
region_totals <- region_clade_counts %>%
  distinct(Study.Region, total)

# 确保堆积条形图中的clade使用相同的因子水平
region_clade_counts$clade <- factor(region_clade_counts$clade, levels = legend_order)

# 第一部分：创建堆积条形图
p1 <- ggplot(region_clade_counts, aes(x = percentage, y = Study.Region, fill = clade)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(data = region_totals, 
            aes(x = 105, y = Study.Region, label = paste0("n=", total)),
            inherit.aes = FALSE, size = 3.5, hjust = 0) +
  labs(x = "Percentage (%)", y = "Study Region", fill = "Clade") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_fill_manual(values = color_palette, breaks = legend_order, drop = FALSE) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# 第二部分：创建系统发育树
temp_tree <- ggtree(tree2, layout = "rectangular", ladderize = TRUE, size = 0.5) %<+% data
max_y_value <- max(temp_tree$data$y) * 1.1 


# 1. 画树并加点
p2 <- ggtree(tree2, layout = "rectangular", ladderize = TRUE, size = 0.5) %<+% data +
  geom_tippoint(aes(color = clade), shape = 16, size = 3) +
  theme_tree2() +
  scale_color_manual(values = color_palette, na.value = "grey", drop = FALSE) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = c(0, 0.68),
    legend.justification = c(0, 0.5),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 1, r = 5, b = 5, l = 5)
  ) +
  labs(color = "Clade")+
  geom_hilight(node=5745, fill="yellow", alpha=0.4, extend = 0.006) +
  geom_hilight(node=7357, fill="gray50", alpha=0.4, extend = 0.005)

# 2. 获取y轴范围并留白
ymin <- min(p2$data$y, na.rm = TRUE)
ymax <- max(p2$data$y, na.rm = TRUE)
y_pad <- (ymax - ymin) * 0.01  

p2 <- p2 + coord_cartesian(ylim = c(ymin - y_pad, ymax + y_pad)) +
  guides(color = guide_legend(override.aes = list(size = 6)))  

print(p2)


# 第三部分：组合图形
combined_plot <- (p2 | p1) + 
  plot_layout(widths = c(1, 1))
# 显示和保存图形
print(combined_plot)
ggsave("combined_plot2.pdf", combined_plot, width = 12, height = 9, dpi = 300)

