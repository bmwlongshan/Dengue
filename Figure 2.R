library(readr)
library(treeio)
library(ggplot2)
library(ggtree)
library(tidytree)
library(patchwork)

tree <- read.beast("MCC")
data <- read_csv("452Data.csv")

# 提取后验概率信息并创建颜色映射
# 假设后验概率存储在树的节点数据中，字段名为"posterior"或"prob"
# 根据实际情况调整字段名
if("posterior" %in% names(tree@data)) {
  tree@data$color_group <- ifelse(as.numeric(tree@data$posterior) >= 0.8, "High", "Low")
} else if("prob" %in% names(tree@data)) {
  tree@data$color_group <- ifelse(as.numeric(tree@data$prob) >= 0.8, "High", "Low")
} else {
  # 如果找不到后验概率字段，设置默认值
  warning("未找到后验概率字段，请检查树文件中的字段名")
  tree@data$color_group <- "Low"
}

# 创建基础树图
p1 <- ggtree(tree, ladderize = TRUE, size = 0.8, mrsd = "2024-01-01", 
             aes(color = color_group)) %<+% data +
  theme_tree2() +
  
  # 设置树枝颜色：后验概率>=0.8为绿色，其他为黑色
  scale_color_manual(
    name = "Posterior Probability",
    values = c("High" = "#2E8B57", "Low" = "black"),
    guide = guide_legend(override.aes = list(size = 3))
  ) +
  
  # 添加叶节点点
  geom_tippoint(aes(fill = Study.Region), shape = 21, size = 4, show.legend = TRUE, color = "black") +
  
  # 设置叶节点填充色
  scale_fill_manual(
    name = "Study Region", 
    values = c(
      "China" = "#FF0000",
      "EasternAsia" = "gray1",
      "Africa" = "lightblue",
      "CentralAmerica" = "gold1",
      "Europe" = "purple4",
      "NorthAmerica" = "tan2",
      "Oceania" = "pink3",
      "SouthAmerica" = "seagreen1",
      "SoutheastAsia" = "peachpuff1",
      "SouthAsia" = "lightcyan3"
    )
  ) +
  
  # 设置主题和图例
  theme(
    legend.position = "right",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.key = element_rect(fill = "white")
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 8, color = "black")),
    color = guide_legend(override.aes = list(size = 3))
  ) +
  
  # 设置坐标轴范围
  coord_cartesian(xlim = c(1900, 2050), ylim = c(0, 460)) +
  
  # 设置x轴刻度
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  
  # 添加垂直线
  geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 0.5) +
  geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 0.5) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.5) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.5) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.5) +
  
  # 添加分支标签
  geom_cladelab(
    node = 574, label = "I", fontsize = 5, align = TRUE, angle = 90,
    offset = 0.001, hjust = 0.5, barcolor = "red", offset.text = 0.004, barsize = 1.5
  ) +
  geom_cladelab(
    node = 456, label = "V", fontsize = 5, align = TRUE, angle = 90,
    offset = 0.01, hjust = 0.5, barcolor = "purple", offset.text = 0.004, barsize = 1.5
  ) +
  geom_cladelab(
    node = 876, label = "IV", fontsize = 5, align = TRUE, angle = 90,
    offset = 0.01, hjust = 0.5, barcolor = "green", offset.text = 0.004, barsize = 1.5
  ) +
  geom_cladelab(
    node = 548, label = "III", fontsize = 5, align = TRUE, angle = 90,
    offset = 0.01, hjust = 0.5, barcolor = "pink", offset.text = 0.004, barsize = 1.5
  ) +
  
  # 添加高亮区域
  geom_hilight(node = 677, fill = "gray50", alpha = 0.4, extend = 0.05) +
  geom_hilight(node = 789, fill = "gray50", alpha = 0.4, extend = 0.05)

# 打印并保存图形
print(p1)
ggsave("p1.pdf", plot = p1, width = 20, height = 10)




# 2023 map - 子树也显示支持率，并应用后验概率着色
tree707 <- tree_subset(tree, node = 707, levels_back=4)  

# 对子树707应用后验概率着色逻辑
if("posterior" %in% names(tree707@data)) {
  tree707@data$color_group <- ifelse(as.numeric(tree707@data$posterior) >= 0.8, "High", "Low")
} else if("prob" %in% names(tree707@data)) {
  tree707@data$color_group <- ifelse(as.numeric(tree707@data$prob) >= 0.8, "High", "Low")
} else {
  tree707@data$color_group <- "Low"
}

p2 <- ggtree(tree707, mrsd = "2024-01-01", aes(color = color_group)) +   
  theme_tree2() +
  
  # 设置子树树枝颜色：后验概率>=0.8为绿色，其他为黑色
  scale_color_manual(
    name = "Posterior Probability",
    values = c("High" = "#2E8B57", "Low" = "black"),
    guide = guide_legend(override.aes = list(size = 2))
  ) +
  
  # 添加节点支持率标签
  geom_nodelab(
    aes(label = ifelse(!is.na(posterior) & as.numeric(as.character(posterior)) >= 0.8, 
                       paste0(round(as.numeric(as.character(posterior))*100, 0), "%"), 
                       "")),
    size = 3.0,
    color = "darkred",
    hjust = 1.3,
    vjust = -0.3
  ) +
  
  coord_cartesian(xlim = c(2010, 2040), ylim = c(0, 19)) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 5)) +
  geom_vline(xintercept = 2015, color = 'black', size = 0.1, alpha = 0.1) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.1) +
  geom_vline(xintercept = 2025, color = 'black', size = 0.1, alpha = 0.1) +
  geom_tiplab()

print(p2)
ggsave("tree707_with_support_colored.pdf", plot = p2, width = 8, height = 6)

# 1902 map - 子树也显示支持率，并应用后验概率着色
tree809 <- tree_subset(tree, node = 809, levels_back=5)  

# 对子树809应用后验概率着色逻辑
if("posterior" %in% names(tree809@data)) {
  tree809@data$color_group <- ifelse(as.numeric(tree809@data$posterior) >= 0.8, "High", "Low")
} else if("prob" %in% names(tree809@data)) {
  tree809@data$color_group <- ifelse(as.numeric(tree809@data$prob) >= 0.8, "High", "Low")
} else {
  tree809@data$color_group <- "Low"
}

p3 <- ggtree(tree809, mrsd = "2024-01-01", aes(color = color_group)) +
  theme_tree2() +
  
  # 设置子树树枝颜色：后验概率>=0.8为绿色，其他为黑色
  scale_color_manual(
    name = "Posterior Probability",
    values = c("High" = "#2E8B57", "Low" = "black"),
    guide = guide_legend(override.aes = list(size = 2))
  ) +
  
  # 添加节点支持率标签
  geom_nodelab(
    aes(label = ifelse(!is.na(posterior) & as.numeric(as.character(posterior)) >= 0.8, 
                       paste0(round(as.numeric(as.character(posterior))*100, 0), "%"), 
                       "")),
    size = 3.0,
    color = "darkred",
    hjust = 1.3,
    vjust = -0.3
  ) +
  
  coord_cartesian(xlim = c(2010, 2040), ylim = c(0, 19)) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 5)) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.1) +
  geom_vline(xintercept = 2025, color = 'black', size = 0.1, alpha = 0.1) +
  geom_tiplab()

print(p3)
ggsave("tree809_with_support_colored.pdf", plot = p3, width = 8, height = 6)

# 可选：将三幅图合并成一幅图（P1在左侧，P2、P3在右侧）
combined_plot <- p1 | (p2 / p3) + 
  plot_layout(widths = c(2, 1))

print(combined_plot)
ggsave("combined_trees_with_support_colored.pdf", plot = combined_plot, width = 25, height = 10)
