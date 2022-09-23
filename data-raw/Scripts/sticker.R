
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
library(scico)

casos <- vroom("https://raw.githubusercontent.com/covid19br/central_covid/master/dados_processados/integridade_SIVEP/dados_srag_br.csv")
obs <- vroom("https://raw.githubusercontent.com/covid19br/central_covid/master/dados_processados/integridade_SIVEP/dados_obsrag_br.csv")

casos<-casos %>%
  filter(dt_sin_pri >=  "2020-01-01")
obs<-obs %>%
  filter(dt_evoluca >=  "2020-01-01")
last_data_casos<-max(casos$data)
last_data_obs<-max(obs$data)
# Setting locale to Brasil
Sys.setlocale(category = "LC_TIME", locale = "pt_BR.UTF-8")

## Gif casos ##
nowc <-casos |>
  filter(dt_sin_pri >= as.Date('2020-02-26') ) |>
  ggplot(aes(x = dt_sin_pri, y = n, col = as.factor(data))) +
  geom_line(size = .3)+
  # geom_line(aes(x=dt_sin_pri, y=-n, col = as.factor(data)))+
  # scale_color_viridis_d(direction = 1,
  #                       aesthetics = "colour",
  #                       option = "cividis") +
  theme_void()+
  theme(legend.position = "none")+
  theme_transparent()+
  # scale_color_gradientn(colors = colorRampPalette(c("#f4f7fb","#004f9f")))
  # scale_colour_discrete_sequential(palette = "Blues 3", rev = T)+
  scale_color_scico_d(palette = "devon", direction = -1)
  # geom_hexagon()+
  # geom_url(url = 'covid19br.github.io/nowcaster',
  #          size = 4.5, color = 'navyblue')
nowc

ggsave(filename = 'man/figures/srag_curve.png',
       plot = nowc,
       width = 11,
       height = 9,
       dpi = 200)

s<-sticker(nowc, package="Nowcaster",
        h_color="#004F9F", p_color = "#004F9F", h_fill="white", h_size = .3,
        s_x=1.0, s_y=1.2, s_width=1.5, s_height=1.5, dpi = 300,
        p_x = .65, p_size=15,
        url = "covid19br.github.io/nowcaster",
        u_size = 4.5, u_color = "#004F9F",
        filename="man/figures/nowcaster.png")
s
## Reverse colors logo

nowc_rev <-casos |>
  filter(dt_sin_pri >= as.Date('2020-02-26') ) |>
  ggplot(aes(x = dt_sin_pri, y = n, col = as.factor(data))) +
  geom_line(size = .3)+
  # geom_line(aes(x=dt_sin_pri, y=-n, col = as.factor(data)))+
  # scale_color_viridis_d(direction = 1,
  #                       aesthetics = "colour",
  #                       option = "cividis") +
  theme_void()+
  theme(legend.position = "none")+
  theme_transparent()+
  # scale_color_gradientn(colors = colorRampPalette(c("#f4f7fb","#004f9f")))
  # scale_colour_discrete_sequential(palette = "Blues 3", rev = F)
  scale_color_scico_d(palette = "devon")
nowc_rev

ggsave(filename = 'man/figures/srag_curve_rev.png',
       plot = nowc_rev,
       width = 11,
       height = 9,
       dpi = 200)

s_rev<-sticker(nowc_rev, package="Nowcaster",
        h_fill="#004F9F", p_color = "white", h_color="white", h_size = .9,
        s_x=1.0, s_y=1.2, s_width=1.5, s_height=1.5, dpi = 300,
        p_x = .65, p_size=15,
        url = "covid19br.github.io/nowcaster",
        u_size = 4.5, u_color = "white",
        filename="man/figures/nowcaster_rev.png")
s_rev
