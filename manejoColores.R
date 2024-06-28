
getwd()
  
rm(list = ls())  


# Paquetería --------------------------------------------------------------

library(tidyverse)
library(haven)


# Levantado de la base ----------------------------------------------------


base <- read_dta("Latinobarometro_2023_Esp_Stata_v1_0.dta")

base %>% glimpse()

unique(base$idenpa)


# Filtrado ----------------------------------------------------------------

base_graph <- base[base$idenpa %in% c(170, 484), ]

base_graph1 <- base_graph %>% 
  filter(idenpa %in% c(170,484))


# Calculo porcentajes -----------------------------------------------------


base_graph <- base_graph %>% 
  group_by(idenpa, P7ST) %>% 
  tally() %>% 
  filter(P7ST > 0) %>% 
  ungroup() %>% 
  group_by(idenpa) %>% 
  mutate(porcentaje = n/sum(n))


# Variable etiqueta respuestas --------------------------------------------

base_graph <- base_graph %>% 
  mutate(respuesta = case_when(               
    P7ST == 1~ "Mucho mejor",
    P7ST == 2~ "Un poco mejor",
    P7ST == 3~ "Igual",
    P7ST == 4~ "Un poco peor",
    P7ST == 5~ "Mucho peor"))

# Orden especifico de la variable

base_graph$respuesta <- factor(base_graph$respuesta, 
                               levels = c("Mucho mejor",
                                          "Un poco mejor",
                                          "Igual",
                                          "Un poco peor",
                                          "Mucho peor"),
                               labels = c("Mucho mejor",
                                          "Un poco mejor",
                                          "Igual",
                                          "Un poco peor",
                                          "Mucho peor"))




# Detalles adicionales para empezar a graficar ----------------------------


# Creacion de variable país


base_graph$pais <- ifelse(test = base_graph$idenpa == 170, yes = "Colombia", no = "México")


base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = respuesta)) +
  geom_col()



# creacion de categoria de agregación -------------------------------------

base_graph$Categoria <- factor(case_when(
  base_graph$respuesta == "Mucho mejor" | base_graph$respuesta == "Un poco mejor" ~ "Mejor",
  base_graph$respuesta == "Igual" ~ "Igual",
  base_graph$respuesta == "Mucho peor" | base_graph$respuesta == "Un poco peor" ~ "Peor"),
  levels = c("Mejor", "Igual", "Peor"),
  labels = c("Mejor", "Igual", "Peor"))



# crear etiquetas internas ------------------------------------------------



base_graph$etiquetas <- str_c(base_graph$respuesta, 
                              round(base_graph$porcentaje*100, 1),
                               "%",
                              sep = " ")




# Gráfico -----------------------------------------------------------------

base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje)) +
  geom_col()

base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = respuesta)) +
  geom_col()

base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = Categoria)) +
  geom_col()

base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = Categoria)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen","darkblue","darkred"))

base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = Categoria, alpha = respuesta)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen","darkblue","darkred"))


alpha_max <- 1
alpha_min <- 0.8

alpha_vals <- c(seq(alpha_max, alpha_min, length.out = 2),
                alpha_min,
                seq(alpha_min, alpha_max, length.out = 2))



base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = Categoria, alpha = respuesta)) +
  geom_col() +
  scale_fill_manual(values = c("darkgreen","darkblue","darkred")) +
  scale_alpha_manual(values = alpha_vals)



# Etiquetas en barras -----------------------------------------------------


grafico <- base_graph %>% 
  ggplot(mapping = aes(x = pais, y = porcentaje, fill = Categoria, alpha = respuesta)) +
  geom_col(show.legend = F,
           color = "white") +
  scale_fill_manual(values = c("darkgreen","darkblue","darkred")) +
  scale_alpha_manual(values = alpha_vals) +
  geom_text(mapping = aes(label = etiquetas),
            position = position_stack(vjust = 0.7),
            col = "white",
            size = 3,
            fontface = "bold",
            show.legend = F) +
  labs(title = "Proporción de respuesta a\n¿Como cree que va a estar la economía en los proximos 12 meses?",
       subtitle = "Comparación entre Colombia y México",
       x = NULL,
       y = "Porcentaje",
       caption = "Fuente: elaboración propia en base Latinobarometro") +
  scale_y_continuous(n.breaks = 10) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, 12),
        plot.subtitle = element_text(hjust = 0.5, 10),
        plot.caption = element_text(hjust = 0, 8))



ggsave(filename = "grafico1.png",
       plot = grafico,
       dpi = 500,
       width = 7.61,
       height = 5.41
         )





















       
