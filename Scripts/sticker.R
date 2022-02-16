
library("tidyverse")
library("cowplot")
library("plotly")
library("gganimate")
library("RColorBrewer")
library("transformr")
library("vroom")

casos <- vroom("../../dados_processados/integridade_SIVEP/dados_srag_br.csv")
obs <- read_csv("../../dados_processados/integridade_SIVEP/dados_obsrag_br.csv")

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
  geom_line()+
  scale_color_viridis_d(direction = -1,
                        aesthetics = "colour",
                        option = "turbo") +
  theme_void()+
  theme(legend.position = "none")+
  theme_transparent()
nowc


library(hexSticker)

sticker(nowc, package="nowcaster", 
        h_color="#f9690e", p_color = "#f9690e", h_fill="white", h_size = .3,
        p_size=18, s_x=.95, s_y=1.2, s_width=2, s_height=1.5,
        filename="man/figures/nowcaster.png")
