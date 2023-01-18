#MJBERENS
#January 17 2023

#load all packages
source("code/0-packages.R")

#Load in the site field data
CoBRA2022_field_data <- read.csv("raw/CoBRA2022_water_inventory.csv")

WLD_field_chem <- subset(CoBRA2022_field_data, Area == "WLD")
TB_field_chem <- subset(CoBRA2022_field_data, Area == "TB")


#Field chemistry function for visualization
field_chem_viz <- function(X) {X %>%
    pivot_longer(c("pH":"temp_degC"), values_to = "value", names_to = "parameter") %>%
    ggplot() +
    geom_point(aes(x=value, y=depth_cm, color=plotID)) +
    geom_path(aes(x=value, y=depth_cm, color=plotID)) +
    facet_wrap(~parameter, ncol=4,
               scales = "free_x",
               labeller = as_labeller(c(ORP_mV="ORP (mV)",
                                        pH = "pH",
                                        SpC_mScm = "Conductivity (mS/cm)",
                                        temp_degC = "Temperature (C)"))) +
    scale_y_reverse() +
    labs(y = "Depth below surface (cm)", x = NULL) +
    theme_er1() +
    scale_color_manual(values = c("#8D0D22", "#FF1600", "#F27F7E", "#EBB3B2"))
}

field_chem_viz(WLD_field_chem)

field_chem_viz(TB_field_chem)
