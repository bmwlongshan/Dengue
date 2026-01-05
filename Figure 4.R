# 安装并加载必要包
if(!require(circlize)) install.packages("circlize")
if(!require(stringr)) install.packages("stringr")
if(!require(dplyr)) install.packages("dplyr")
library(circlize)
library(stringr)
library(dplyr)

# 打开PDF设备
pdf("markov_jumps_chord.pdf", width=10, height=10)

# 1. 读取并提取跳跃数据
lines <- readLines("Combinejumps")
jump_line <- lines[grepl("\\{\\{", lines)]
jump_data_str <- str_extract(jump_line, "\\{\\{.*\\}\\}")
jump_data_str <- substr(jump_data_str, 3, nchar(jump_data_str)-2)
jump_list <- strsplit(jump_data_str, "\\},\\{")[[1]]
jump_df <- do.call(rbind, lapply(jump_list, function(x) {
  x <- gsub("\\{|\\}", "", x)
  parts <- strsplit(x, ",")[[1]]
  data.frame(
    value = as.numeric(parts[2]),
    from = parts[3],
    to = parts[4],
    stringsAsFactors = FALSE
  )
}))
jump_df <- as.data.frame(jump_df)
jump_df$value <- as.numeric(jump_df$value)

# 2. 汇总每对起点-终点的总跳跃数
jump_summary <- jump_df %>%
  group_by(from, to) %>%
  summarise(total = sum(value)) %>%
  ungroup()

# 3. 固定每个地理位置的颜色
all_regions <- unique(c(jump_summary$from, jump_summary$to))
region_colors <- c(
  "Oceania" = "#E41A1C", # 红
  "SoutheastAsia" = "#377EB8", # 蓝
  "SouthAmerica" = "#4DAF4A", # 绿
  "NorthAmerica" = "#984EA3", # 紫
  "Taiwan" = "#FF7F00", # 橙
  "Guangdong" = "#FFFF33", # 黄
  "Fujian" = "#A65628", # 棕
  "Yunnan" = "#F781BF", # 粉
  "Shanghai" = "#999999", # 灰
  "Hebei" = "#1B9E77", # 深绿
  "Hainan" = "#D95F02", # 深橙
  "Henan" = "#7570B3", # 深蓝紫
  "Hubei" = "#E7298A", # 洋红
  "Hunan" = "#66A61E", # 草绿
  "Jiangsu" = "#E6AB02", # 土黄
  "Sichuan" = "#A6761D", # 深棕
  "Zhejiang" = "#666666", # 深灰
  "Europe" = "#8DD3C7", # 浅青
  "Africa" = "#BEBADA", # 浅紫
  "SouthAsia" = "#FB8072", # 浅红
  "EastAsia" = "#80B1D3", # 浅蓝
  "Ningxia" = "#FDB462"  # 浅橙
)
region_colors <- region_colors[all_regions]
region_colors[is.na(region_colors)] <- "#CCCCCC"
names(region_colors) <- all_regions

# 4. 绘制弦图
chordDiagram(jump_summary, 
             grid.col = region_colors,
             transparency = 0.3, 
             annotationTrack = "grid", 
             preAllocateTracks = 1)
title("Markov Jumps 地理流向弦图")

# 5. 所有标签都竖直显示
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sector.name = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(
    mean(xlim), ylim[2] + 0.2,
    sector.name,
    sector.index = sector.name,
    facing = "downward",  
    niceFacing = TRUE,
    adj = c(0.5, 0.5),
    cex = 1.1,
    col = "black"
  )
}, bg.border = NA)

circos.clear()

dev.off()
