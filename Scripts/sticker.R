
library(hexSticker)
library("tidyverse")
library("cowplot")
library("plotly")
library("gganimate")
library("RColorBrewer")
library("transformr")
library("vroom")
library(colorRamps)
library(colorspace)

casos <- vroom("../central_covid/dados_processados/integridade_SIVEP/dados_srag_br.csv")
obs <- read_csv("../central_covid//dados_processados/integridade_SIVEP/dados_obsrag_br.csv")

casos<-casos %>%
  filter(dt_sin_pri >=  "2020-01-01")
obs<-obs %>%
  filter(dt_evoluca >=  "2020-01-01")
last_data_casos<-max(casos$data)
last_data_obs<-max(obs$data)
# Setting locale to Brasil
Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

## Gif casos ##
nowc <-
  ggplot(data = casos, aes(x = dt_sin_pri, y = n, col = as.factor(data))) +
  geom_line(size = .3)+
  # geom_line(aes(x=dt_sin_pri, y=-n, col = as.factor(data)))+
  # scale_color_viridis_d(direction = 1,
  #                       aesthetics = "colour",
  #                       option = "cividis") +
  theme_void()+
  theme(legend.position = "none")+
  theme_transparent()+
  # scale_color_gradientn(colors = colorRampPalette(c("#f4f7fb","#004f9f")))
  scale_colour_discrete_sequential(palette = "Blues 3", rev = T)
nowc

sticker(nowc, package="nowcaster", 
        h_color="#004F9F", p_color = "#004F9F", h_fill="white", h_size = .3,
        s_x=.95, s_y=1.2, s_width=2, s_height=1.5, dpi = 300, 
        p_x = .65, p_size=15,
        filename="man/figures/nowcaster.png")

## Reverse colors logo

nowc_rev <-
  ggplot(data = casos, aes(x = dt_sin_pri, y = n, col = as.factor(data))) +
  geom_line(size = .3)+
  # geom_line(aes(x=dt_sin_pri, y=-n, col = as.factor(data)))+
  # scale_color_viridis_d(direction = 1,
  #                       aesthetics = "colour",
  #                       option = "cividis") +
  theme_void()+
  theme(legend.position = "none")+
  theme_transparent()+
  # scale_color_gradientn(colors = colorRampPalette(c("#f4f7fb","#004f9f")))
  scale_colour_discrete_sequential(palette = "Blues 3", rev = F)
nowc_rev

sticker(nowc_rev, package="nowcaster", 
        h_fill="#004F9F", p_color = "white", h_color="white", h_size = .3,
        s_x=.95, s_y=1.2, s_width=2, s_height=1.5, dpi = 300, 
        p_x = .65, p_size=15,
        filename="man/figures/nowcaster_rev.png")
