library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggprism)

# 读取 Excel 数据
excel_file <- "E:/LRQin/001methane/Data/03emissionupdt_range.xlsx"  
data <- read_excel(excel_file, sheet = 'zong')

# 假设数据包含5列：time, group, value, min, max
data_long <- data %>%
  gather(key = "Variable", value = "Value", -time, -group, -min, -max, -type) %>%
  mutate(group = factor(group, levels = c("Our work", "Wetcharts", "LPJ-wsl", "ORCHIDEE-MICT","McNorton 2022")))
data_long
# 定义颜色
line_colors <- c("Our work" = "#767171", "Wetcharts" = "#4b7db3", "ORCHIDEE-MICT" = "#573d9b", "LPJ-wsl" = "#ae627d","McNorton 2022" = "#e9967a")
fill_colors <- c("Our work" = "#767171", "Wetcharts" = "#4b7db3", "ORCHIDEE-MICT" = "#573d9b", "LPJ-wsl" = "#ae627d","McNorton 2022" = "#ffffff")
line_types <- c("Our work" = "solid", "Wetcharts" = "dashed", "ORCHIDEE-MICT" = "dashed", "LPJ-wsl" = "dashed","McNorton 2022" = "dashed")
# 确保日期列是 Date 类型
data_long$time <- as.Date(data_long$time)
# 创建一个新的变量，合并 group 和 linetype，作为唯一的图例分组依据
data_long$group_linetype <- interaction(data_long$group, data_long$type)
data_long
#调用 windows 的字体格式，本示例分别以 Arial 和 Times New Roman 为例进行演示
windowsFonts(Arial = windowsFont('Arial'))


# 绘制每组的折线图及阴影区，每组图表在同一张图上
p <- ggplot(data_long, aes(x = time, y = Value, color = group, group = group, linetype = group)) +
  geom_line(na.rm = TRUE,, size = 0.8) +  # 绘制折线图
  geom_ribbon(aes(ymin = min, ymax = max, fill = group), alpha = 0.2, color = NA) +  # 绘制阴影区
  scale_y_continuous() +
  scale_x_date(
    breaks = "1 year",  # 设置主刻度为每年一次
    minor_breaks = seq(as.Date("2019-01-01"), as.Date("2022-12-01"), by = "2 months"),  # 设置次刻度为每两个月一次
    guide = "prism_minor",
    date_labels = "%Y"  # 设置日期标签格式为“年”
  ) +
  scale_color_manual(values = line_colors) +  # 指定折线颜色
  scale_fill_manual(values = fill_colors) +  # 指定阴影颜色
  scale_linetype_manual(values = line_types) +
  theme_prism(border = TRUE, base_rect_size = 0.4) +  # 使用 ggprism 主题
  coord_cartesian(clip = "off") +  # 允许图形超出坐标系的范围，不进行裁剪
  guides(colour = guide_legend(position = "inside"), linetype = guide_legend(position = "inside")) +  # 设置图例的位置为内部
  theme(
    legend.position.inside = c(0.88, 0.86),
    legend.title = element_blank(),
    text = element_text(family = "Arial", face = "plain", size = 16),
    axis.title.y = element_text(margin = margin(r = 21.5)),
    axis.title.x = element_blank(),
    axis.text.y = element_text(margin = margin(r = 15)),
    axis.ticks.y = element_line(linewidth = 0.4),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.ticks.x = element_line(linewidth = 0.4),
    axis.ticks.length.x = unit(0.15, "cm"),  # 设置x轴刻度线朝外
    legend.key.width = unit(0.8, "cm"),  # 调整图例宽度
    legend.key.height = unit(0.4, "cm"),  # 调整图例项的高度
    legend.text = element_text(margin = margin(r = 1))  # 减小标签右侧间距
  )

# 打印图形
print(p)

# 导出为 PNG 图片
ggsave(
  filename = "emissionmodel0312.tif",
  plot = p,
  width = 21.5,
  height = 11,
  dpi = 600,
  units = "cm"
)

#使用 gg.gap 截断坐标轴
#在 gg.gap() 中指定 ggplot2 的作图对象，并添加参数截断坐标轴
#参数中，segments 用于截断坐标轴，rel_heights 缩放截图，tick_width 用于重新指定显示的刻度轴标签，ylim 指定刻度轴范围
# library(gg.gap)
# 
# #截成两段
# gg.gap(plot = p, segments = c(15, 65), rel_heights = c(0.7, 0, 0.2), tick_width = c(5, 10), ylim = c(0, 90))

