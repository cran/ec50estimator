## ----message=FALSE, warning=FALSE---------------------------------------------
library(ec50estimator)
library(drc)
library(ggplot2)
library(dplyr)
library(magrittr)
library(cowplot)
library(ggridges)

## -----------------------------------------------------------------------------
data(multi_isolate)

## -----------------------------------------------------------------------------
multi_isolate %>% 
  ggplot(aes(dose, growth, color = growth))+
  geom_jitter(width = 0.1)+
  facet_grid(field~fungicida)+
  scale_x_log10()+
  scale_color_viridis_c(option = "B",direction = 1)+
  theme_light()

## -----------------------------------------------------------------------------

df_ec50 = estimate_EC50(growth~dose,
                        data =multi_isolate,
                        isolate_col = "isolate", 
                        strata_col =  c("field","fungicida"),
                        interval = "delta",
                        fct = drc::LL.3())
head(df_ec50)


## ----fig.height=10, fig.width=6-----------------------------------------------
as.data.frame(df_ec50) %>% 
  mutate(ID = as.numeric(ID)) %>% 
  ggplot(aes(ID, Estimate, color = field))+
  geom_point(size = 2)+
  geom_errorbar(aes(ymin=Lower,ymax = Upper, color = field), width=0)+
  facet_wrap(~fungicida, scales = "free_x", ncol = 2)+
  scale_y_log10()+
  scale_x_continuous(breaks = 1:50)+
  scale_color_manual(values = c("darkred", "steelblue"))+
  labs(x = "Isolates", y = "EC50")+
  theme_minimal_vgrid(font_size = 10)+
  coord_flip()+
  theme(axis.text.x = element_text(size=10),
        legend.position = "bottom")

## -----------------------------------------------------------------------------
as.data.frame(df_ec50) %>% 
  ggplot(aes(Estimate, field, fill = stat(x)))+
  geom_density_ridges_gradient(alpha = 0.3)+
  scale_x_log10()+
  scale_fill_viridis_c(option = "C")+
  facet_wrap(~fungicida, nrow = 2)+
  theme_minimal_hgrid()+
  labs(x = "EC50", y = "Field")+
  theme(legend.position = "none")

