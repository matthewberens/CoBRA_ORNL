#MJBERENS
#January 17 2023

#Load libraries and set directory
library(tidyverse)
setwd("~/Documents/CoBRA_redox_porechem/CoBRA2022")

#General plot formatting
theme_mb1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 10, lineheight = 1.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 8, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=8, face="bold"), #facet labels
          strip.text.y = element_text(size=8, face="bold", angle = 270) #facet labels
    )
}



#List of sites, samples, and field chemistry data
sites2022 <- read.csv("CoBRA2022_Site_List.csv")
CoBRA_2022_Field_Data <- read.csv("data/CoBRA2022_Field_Data.csv") 
CoBRA2022_SubB_Anions <- read.csv("data/CoBRA2022_SubB_Anions.csv") 


#Separate sample metadata
metadata_CoBRA2022 <- CoBRA_2022_Field_Data %>%
  dplyr::select(sampleID:depth_cm)

#Combine all data
master_chemistry_CoBRA2022 <- 
  left_join(CoBRA_2022_Field_Data, CoBRA2022_SubB_Anions, by = "sampleID")

####################################

WLD_chemistry <- 
  master_chemistry_CoBRA2022 %>%
  subset(area == "WLD") %>%
  dplyr::select(sampleID, plotID, depth_cm:sulfate_ppm) %>%
  dplyr::select(-comments)

TB_chemistry <- 
  master_chemistry_CoBRA2022 %>%
  subset(area == "TB") %>%
  dplyr::select(sampleID, plotID, depth_cm:sulfate_ppm) %>%
  dplyr::select(-comments)

ATC_chemistry <- 
  master_chemistry_CoBRA2022 %>%
  subset(area == "ATC") %>%
  dplyr::select(sampleID, plotID, depth_cm:sulfate_ppm) %>%
  dplyr::select(-comments)

##############################################

#Functions for data visualization
field_chem_viz <- function(X) {X %>%
    pivot_longer(c("pH":"temp_degC"), values_to = "value", names_to = "parameter") %>%
    ggplot() +
    geom_point(aes(x=value, y=depth_cm, color=plotID)) +
    geom_path(aes(x=value, y=depth_cm, color=plotID)) +
    facet_wrap(~parameter, ncol=4,
               scales = "free_x",
               labeller = as_labeller(c(ORP_mV="ORP (mV)",
                                        pH = "pH",
                                        SC_mScm = "Conductivity (mS/cm)",
                                        temp_degC = "Temperature (C)"))) +
    scale_y_reverse() +
    labs(y = "Depth below surface (cm)", x = NULL) +
    theme_mb1() +
    scale_color_manual(values = c("#8D0D22", "#FF1600", "#F27F7E", "#EBB3B2"))
}
anions_viz <- function(X) {X %>%
    pivot_longer(c("chloride_ppm":"sulfate_ppm"), values_to = "value", names_to = "parameter") %>%
    ggplot() +
    geom_point(aes(x=value, y=depth_cm, color=plotID)) +
    geom_path(aes(x=value, y=depth_cm, color=plotID)) +
    facet_wrap(~parameter, 
               ncol=2,
               scales = "free_x",
               labeller = as_labeller(c(sulfate_ppm="Sulfate (mg/L)",
                                        chloride_ppm = "Chloride (mg/L)"))) +
    scale_y_reverse() +
    labs(y = "Depth below surface (cm)", x = NULL) +
    theme_mb1() +
    scale_color_manual(values = c("#8D0D22", "#FF1600", "#F27F7E", "#EBB3B2"))
}

################################

chemfig_WLD_2022 <- field_chem_viz(WLD_chemistry)
chemfig_TB_2022 <- field_chem_viz(TB_chemistry)
chemfig_ATC_2022 <- field_chem_viz(ATC_chemistry)

ggsave("figures/chemfig_WLD_2022.png", plot = chemfig_WLD_2022, height = 4, width = 6)
ggsave("figures/chemfig_TB_2022.png", plot = chemfig_TB_2022, height = 4, width = 6)
ggsave("figures/chemfig_ATC_2022.png", plot = chemfig_ATC_2022, height = 4, width = 6)

anionsfig_WLD_2022 <- anions_viz(WLD_chemistry)
anionsfig_TB_2022 <- anions_viz(TB_chemistry)
anionsfig_ATC_2022 <- anions_viz(ATC_chemistry)

ggsave("figures/anionsfig_WLD_2022.png", plot = anionsfig_WLD_2022, height = 4, width = 6)
ggsave("figures/anionsfig_TB_2022.png", plot = anionsfig_TB_2022, height = 4, width = 6)
ggsave("figures/anionsfig_ATC_2022.png", plot = anionsfig_ATC_2022, height = 4, width = 6)
