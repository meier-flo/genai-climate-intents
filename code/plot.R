require(tidyverse)


tab_intents <- read_csv("intent_labels.csv")

tab_intents%>%mutate(name = paste(code,label),
                     bar_label = paste0(round(share*100,1),"% (",n,")"))%>%
ggplot(aes(x = share, y = fct_rev(fct_inorder(name)))) +
  geom_col(width = 0.7) +
  # long bars carry their label inside (white), short bars to the right
  geom_text(aes(label = bar_label),hjust=-0.2,size = 10 / .pt) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "white"), guide = "none") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1),
                     breaks = seq(0, 0.75, 0.25), expand = expansion(mult = c(0, 0))) +
  labs(x ="Share of conversations",y = "") +
  theme_minimal(base_size = 15)+
  theme(axis.text = element_text(colour = "black"))

