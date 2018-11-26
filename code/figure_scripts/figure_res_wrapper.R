library(tidyr)
library(RCurl)

img_dpi <- as.numeric(readline(prompt="Enter img dpi: "))
img_dpi = ifelse(is.na(img_dpi), 100, img_dpi)

default_fig_path <- readline(prompt="Enter default fig path: ")
default_fig_path = ifelse(length(default_fig_path)==1 & exists('fig_path'), fig_path, default_fig_path)

out_device <- readline(prompt="Enter image filetype: ")
out_device = ifelse(length(out_device)==1, 'jpeg', out_device)

#Color blind friendly
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_bw())
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=cbbPalette) + scale_color_manual(values=cbbPalette)+theme(legend.position="bottom")

high_res = ifelse(img_dpi>100, T, F)

fig_path = ifelse(high_res, paste0(default_fig_path, "/high_res/"), default_fig_path)
