library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geomtextpath)

# 读取数据
location <- read_csv("452Location.csv")
chain <- read_csv("Ningxia_tips_transmission_chains.csv")

# 去除可能的空格
location <- location %>%
  mutate(Location = trimws(Location))
chain <- chain %>%
  mutate(from = trimws(from), to = trimws(to))

# 合并经纬度
chain <- chain %>%
  left_join(location %>% rename(from_lat = latitude, from_long = longitude), by = c("from" = "Location")) %>%
  left_join(location %>% rename(to_lat = latitude, to_long = longitude), by = c("to" = "Location"))

# 只保留涉及的地点
involved_locations <- unique(c(chain$from, chain$to))
location_involved <- location %>% filter(Location %in% involved_locations)
location_involved <- location_involved %>%
  mutate(
    nudge_x = case_when(
      Location == "Ningxia" ~ 1,
      Location == "SoutheastAsia" ~ 0,
      Location == "Guangdong" ~ 1,
      Location == "Yunnan" ~ 0.1,
      TRUE ~ 0
    ),
    nudge_y = case_when(
      Location == "Ningxia" ~ 0.5,
      Location == "SoutheastAsia" ~ -1,
      Location == "Guangdong" ~ 0,
      Location == "Yunnan" ~ 0,
      TRUE ~ 0
    )
  )

# 生成标签
chain <- chain %>%
  mutate(
    label = paste0(
      as.integer(event_year), " [", 
      as.integer(HPD_low), " - ", 
      as.integer(HPD_high), "]"
    )
  )

# 标记SoutheastAsia到Ningxia
chain <- chain %>%
  mutate(
    is_sea2nx = (from == "SoutheastAsia" & to == "Ningxia")
  )

# 获取地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")
southeast_asia_countries <- c(
  "Brunei", "Cambodia", "East Timor", "Indonesia", "Laos", "Malaysia",
  "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"
)
sea_map <- world %>% filter(admin %in% southeast_asia_countries)
other_map <- world %>% filter(!(admin %in% c("China", southeast_asia_countries)))

# 读取中国GeoJSON
china_standard <- st_read("China.json", quiet = TRUE)

# 选择宁夏
ningxia_border <- china_standard %>% filter(name == "宁夏回族自治区")

# 绘图
p2 <- ggplot() +
  geom_sf(data = other_map, fill = "#e0ecf4", color = "gray80", size = 0.2) +
  geom_sf(data = sea_map, fill = "#fff7bc", color = "gray40", size = 0.3) +
  geom_sf(data = china_standard, fill = "#fbb4c4", color = "gray80", size = 0.2) +
  geom_sf(data = ningxia_border, fill = NA, color = "#e41a1c", size = 1.2) +
  geom_point(
    data = location_involved,
    aes(x = longitude, y = latitude),
    color = "#238b45", fill = "#a1d99b", shape = 21, size = 3, stroke = 1
  ) +
  geom_text_repel(
    data = location_involved,
    aes(x = longitude, y = latitude, label = Location),
    nudge_x = location_involved$nudge_x,
    nudge_y = location_involved$nudge_y,
    size = 5,
    fontface = "bold",
    color = "#222222",
    box.padding = 1.5,
    point.padding = 1.5,
    max.overlaps = 30,
    segment.curvature = -0.2,
    segment.size = 0.5,
    min.segment.length = 0
  ) +
  geom_curve(
    data = chain %>% filter(!is_sea2nx),
    aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat),
    curvature = 0.15,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = "black", linewidth = 1
  ) +
  geom_textcurve(
    data = chain %>% filter(!is_sea2nx),
    aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat, label = label),
    curvature = 0.15,
    color = "black",
    vjust = 1.2,
    size = 3.5,
    fontface = "bold",
    linewidth = 0,
    straight = TRUE,
    remove_long = TRUE
  ) +
  geom_curve(
    data = chain %>% filter(is_sea2nx),
    aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat),
    curvature = -0.32,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = "purple", linewidth = 1
  ) +
  geom_textcurve(
    data = chain %>% filter(is_sea2nx),
    aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat, label = label),
    curvature = -0.32,
    color = "purple",
    vjust = -0.5,
    size = 3.8,
    fontface = "bold",
    linewidth = 0,
    straight = TRUE,
    remove_long = TRUE
  ) +
  coord_sf(xlim = c(60, 140), ylim = c(5, 55), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#f7fbff", color = NA),
    plot.background = element_rect(fill = "#f7fbff", color = NA),
    panel.grid = element_line(color = "gray90"),
    axis.text = element_text(color = "#222222"),
    plot.title = element_text(face = "bold", size = 18, color = "#08519c")
  ) +
  labs(x = "Longitude", y = "Latitude")

print(p2)
ggsave("Ningxia_Transmission_Chain2.pdf", plot = p2, width = 10, height = 8, units = "in")

