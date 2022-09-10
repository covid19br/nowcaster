library(nowcaster)
library(ggplot2)
library(dplyr)
library(geomtextpath)

dados_by_week <- sragBH |>
  rename(date_onset = DT_SIN_PRI) |> 
  filter(year(date_onset) < 2021) |> 
  group_by(date_onset) |> 
  summarise(n=n())

dados_by_week |> 
  # filter(month(date_onset) < 8) |> 
  ggplot()+
  geom_line(data = dados_by_week, 
            aes(date_onset, 
                y = n, 
                col = 'SARI'), show.legend = FALSE)+
  # geom_vline(xintercept = as.Date('2020-02-26'))+
  geom_textvline(xintercept = as.Date('2020-02-26'), 
            label = 'First Covid-19 cases', 
            angle = 90,
            hjust = .9)+
  # geom_vline(xintercept = as.Date('2020-03-17'), 
  #            aes(col = 'First death by SARI by of Covid-19'))+
  geom_textvline(xintercept = as.Date('2020-03-17'), 
            label = 'First confirmed Death by SARI', 
            angle = 90, 
            hjust = .9)+
  theme_bw()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c('grey50', 'black'), 
                     name = '')+
  scale_x_date(date_breaks = '2 months', 
               date_labels = '%V/%y', 
               name = 'Date [in Weeks]')+
  labs(x = '', 
       y = 'Nº SARI Hospitalizations',
       title = 'Belo Horizonte')
