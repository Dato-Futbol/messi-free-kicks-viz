---
title: "R para visualizar datos deportivos espaciales"
author: "Ismael Gómez (futbolytics.cl / datofutbol.cl)"
date: "29 de octubre de 2020"
output: html_document
---

<br>

Acá encontrarás el **código** utilizado para este **webinar** desarrollado para DataLatam.<br>
<br>
<p>
  <img width="50%" height="50%" src="img/fondo.jpg">
</p>
<br>
Además puedes revisar los siguientes **recursos** adicionales sobre la charla:

1. [Video](https://www.youtube.com/watch?v=KFCj4TL7hio)
2. [Slides](https://www.datofutbol.cl/slides-datalatam-2020)

<br> 

### Ejemplo desarrollado: "Ubicación de los tiros libres de Messi en La Liga"

<br>

#### 1) Obtención de datos:
<br>
Datos abiertos de **Statsbomb**<br>
[1. Github](https://github.com/statsbomb/open-data)<br/ >
[2. Sitio web (detalles + user agreement)](https://www.statsbomb.com/resource-centre)
<br>
<p>
  <img width="30%" height="30%" src="img/statsbomb.jpg">
</p>
<br>
<br>

##### Carga y proceso de datos con paquete [{**StatsBombR**}](https://github.com/statsbomb/StatsBombR)

```{r eval = FALSE}
devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

competitions <- FreeCompetitions() 
liga_spain <- competitions %>% filter(competition_id == 11)
range(liga_spain$season_name)

games <- FreeMatches(liga_spain)
glimpse(games)
unique(paste0(games$home_team.home_team_name, "-", 
              games$away_team.away_team_name))
barcelona_games <- games %>% 
                   filter("Barcelona" %in% c(home_team.home_team_name, 
                                             away_team.away_team_name))

barcelona_events <- StatsBombFreeEvents(MatchesDF = barcelona_games)
library(readr)
#write_rds(barcelona_events, "barcelona_events.rds")
#barcelona_events <- read_rds("barcelona_events.rds")
messi_events <- barcelona_events %>% 
                filter(str_detect(player.name, "Messi"))

glimpse(messi_events)
table(messi_events$type.name)

messi_shots <- messi_events %>% 
               filter(type.name == "Shot")

messi_shots_cleaned <- allclean(messi_shots)
```

<br>
<br>

#### Paso 2A: Exploración

Paquetes: {dplyr} {ggplot2} {[**DataExplorer**](http://boxuancui.github.io/DataExplorer/)}

```{r eval = FALSE}
data <- messi_shots_cleaned %>% 
        left_join(liga_spain %>% select(season_id, season_name)) %>%
        select(match_id, period, timestamp, type.name, possession_team.name, play_pattern.name,
               player.name, position.name, shot.type.name, location.x, location.y,
               shot.body_part.name, shot.technique.name, shot.statsbomb_xg, 
               shot.outcome.name, shot.end_location.x, shot.end_location.y, season_name)

devtools::install_github("boxuancui/DataExplorer")
library(DataExplorer)
plot_bar(data %>% select(shot.type.name, shot.outcome.name), ncol = 2)
plot_density(data %>% select(location.x, location.y), ncol = 1)

```

<p align="center">
  <img width="60%" height="60%" src="img/shots_explore1.png">
</p>
<p align="center">
  <img width="40%" height="40%" src="img/shots_explore2.png">
</p>
<br>
<br>

#### Paso 2B: Procesar coordenadas

```{r eval = FALSE}
data <- data %>%
        mutate_at(vars(contains('.x')), funs( . / 120 * 110)) %>% 
        mutate_at(vars(contains('.y')), funs( (80 - .) / 80 * 73))

data_free_kick <- data %>%
        filter(shot.type.name == "Free Kick")
```
<br>
<br>

#### Paso 3A: Cancha como imagen de fondo

```{r eval = FALSE}
library(jpeg)
img <- jpeg::readJPEG("img/football-pitch1.jpg")

library(grid)
p <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

library(ggplot2)
g <- ggplot() +
        annotation_custom(p, xmin = -0.5, xmax = 110.5, 
                             ymin = -0.5, ymax = 73.5) +
        scale_x_continuous(limits = c(-0.5, 110.5), expand = c(0,0)) +
        scale_y_continuous(limits = c(-0.5, 73.5), expand = c(0,0))
        
g + geom_point(data = data_free_kick, 
               mapping = aes(x = location.x, y = location.y), 
               pch = 21, alpha = 0.2, fill = "red", size = 2)

# library(png)
# img2 <- png::readPNG("image2_path")
# p2 <- rasterGrob(img2, width = unit(1, "npc"), height = unit(1, "npc"))

```

<p align="center">
  <img width="70%" height="70%" src="img/messi_free_kicks.png">
</p>
<br>
<br>

#### Paso 3B: Dibujar la cancha con ggplot2 y ggforce

```{r eval = FALSE}
library(ggforce)
# función para dibujar la mitad ofensiva de la cancha invertida
get_pitch <- function(gp, pitch_fill = "white", pitch_col = "lightgrey"){
        contorno_df <- data.frame(x = c(55, 55, 110, 110), 
                                  xend = c(55, 110, 110, 55),
                                  y = c(0, 73, 73, 0),
                                  yend = c(73, 73, 0, 0))
        
        areas_df <- data.frame(x = c(110, 93, 93, 110, 104, 104), 
                               xend = c(93, 93, 110, 104, 104, 110), 
                               y = c(16, 16, 57, 27, 27, 46),
                               yend = c(16, 57, 57, 27, 46, 46))
        gp +
                theme_void() +
                theme(panel.background = element_rect(fill = pitch_fill),
                      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")) +
                # rectángulos
                geom_segment(data = contorno_df,
                             aes(x = x, xend = xend, y = y, yend = yend), 
                             col = pitch_col) +
                geom_segment(data = areas_df,
                             aes(x = x, xend = xend, y = y, yend = yend), 
                             col = pitch_col) +
                geom_rect(aes(xmin = 110, xmax = 111.5, ymin = 33, ymax = 40), 
                          fill = "transparent", col = pitch_col) +
                # puntos
                geom_point(aes(x = 110/2, y = 73/2), col = pitch_col) +
                geom_point(aes(x = 98, y = 73/2), col = pitch_col) +
                # semi círculos
                geom_arc(aes(x0 = 98, y0 = 73/2, r = 9.15, start = -34*pi/180, 
                             end = -146*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 110/2, y0 = 73/2, r = 9.15, start = 0, 
                             end = 180*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 110, y0 = 73, r = 1, start = 270*pi/180, 
                             end = 360*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 110, y0 = 0, r = 1, start = 180*pi/180, 
                             end = 270*pi/180), col = pitch_col) +
                coord_flip() +
                scale_y_reverse()
}

get_pitch(gp = ggplot(), 
          pitch_fill = "white", pitch_col = "black") +
        geom_point(data = data_free_kick, 
           mapping = aes(x = location.x, y = location.y), 
           alpha = 0.3, col = "darkblue", size = 2)
```
<p align="center">
  <img width="70%" height="70%" src="img/messi_free_kicks2.png">
</p>
<br>
<br>

#### Paso 4: Mapeo de datos

Output temporal
```{r eval = FALSE}    
data_free_kick <- data_free_kick %>%
        mutate(is_goal = ifelse(shot.outcome.name == "Goal", "GOAL", "NO GOAL"))
get_pitch(gp = ggplot(data_free_kick), pitch_fill = "white", pitch_col = "black") + 
        geom_point(mapping = aes(x = location.x, y = location.y, size = shot.statsbomb_xg, fill = is_goal), pch = 21, alpha = 0.3, col = "black", stroke = 1.05) +
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position = "top")
```
<p align="center">
  <img width="70%" height="70%" src="img/messi_temporal.png">
</p>
<br>
<br>

#### Paso 5: Detalles finales
```{r eval = FALSE}   
n_games <- data %>% distinct(match_id) %>% nrow()
messi_stats <- data_free_kick %>%
            summarise(Games = n_games, Shots = n(), xG_Sum = round(sum(shot.statsbomb_xg), 2),
                      Goals = sum(ifelse(is_goal == "GOAL", 1, 0)),
                      xG_dif = paste(ifelse(Goals > xG_Sum, "+", ""), round(Goals - xG_Sum, 2)),
                      xG_per_shot = round(xG_Sum / Shots, 2), Shots_per_goal = round(Shots/Goals, 2)) %>% 
            t() %>% as.data.frame()
library(gridExtra)
tt <- ttheme_minimal(base_size = 8, core = list(bg_params = list(fill = NA, col = NA), 
                                                fg_params = list(fontface = 3L, col = "white")),
                     rowhead = list(fg_params = list(col = "white", fontface = 3L)))
library(ggtext)
g <-  ggplot(data_free_kick) + 
        geom_density2d_filled(mapping = aes(x = location.x, y = location.y, fill = ..level..),
                               contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
        scale_fill_brewer(palette = "YlOrRd", direction = -1) +
        geom_point(data = data_free_kick %>% filter(shot.outcome.name == "Goal"),
                   mapping = aes(x = location.x, y = location.y, size = shot.statsbomb_xg), fill = "blue",
                   pch = 21, alpha = 0.5, col = "white", stroke = 1)
get_pitch(gp = g, pitch_fill = "black") +
        guides(fill = "none", size = guide_legend(title = "xG")) +
        theme(legend.position = "top",
              plot.title = element_text(size = 16, hjust = 0.5),
              plot.subtitle = element_markdown(size = 12, hjust = 0.5)) +
        annotation_custom(tableGrob(messi_stats, cols = NULL, theme = tt,
                                    rows = c("Partidos", "Tiros libres", "suma xG", "Goles", 
                                             "xG dif.", "xG por shot", "Tiros por gol")), 
                                    xmin = 65, xmax = 70, ymin = -8, ymax = -8) +
        labs(title = "Tiros libres de Messi en La Liga 2006/2007 - 2019/2020",
             subtitle = "Mapa de calor indica densidad de tiros. Puntos indican <span style='color:blue'>**Goles**</span>.<br/ >",
             caption = "Autor: @DatoFutbol_cl | Data: Statsbomb")

```

<p align="center">
  <img width="80%" height="80%" src="img/messi_free_kicks3B.png">
</p>
