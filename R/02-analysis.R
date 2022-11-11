
# Productivity index ------------------------------------------------------

PPGs <- read_xlsx("./Data_clean/PPGs.xlsx",sheet = 1)

media.geral <- mean(PPGs$media)

prod <- ggplot(data = PPGs, 
               aes(media, 
                   universidade, 
                   xmax= maximo, 
                   xmin= minimo)) +
  geom_vline(xintercept = 0.0, linetype="dashed") +
  geom_vline(xintercept = media.geral, linetype="dashed", colour = "red") +
  geom_errorbarh(alpha=0.8, color="black", height = 0.2) +
  geom_point(data = PPGs, aes(size = N, fill = status), shape = 22) +
  geom_point(data = PPGs, aes(x = mediana)) +
  scale_fill_manual(breaks = c("Excellence program", "Non-excellence program"),
                    values = c("#FDE725FF", "#21908CFF")) +
  theme_bw() +
  xlab("Índice de Produtividade") +
  ylab(NULL) +
  theme(text = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, colour = "black"))

ggsave(filename = "./Figure/indice_prod.tiff", 
       plot = prod, 
       units = "cm",
       width = 25,
       height = 20,
       dpi = 300)
