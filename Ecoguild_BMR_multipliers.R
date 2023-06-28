
# Prepare workspace ####

rm(list = ls(all = TRUE))

library(tidyverse) # for nice coding
library(ggalluvial) # for geom_alluvium()
library(ggpubr) # for ggarrange()

my_cols <- c("#6699CC", "#88CCEE", "#117733", "#AA4499", "#CC6677", "#332288", "#DDCC77",
                      "#44AA99", "#999933", "#882255")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create dataframe of marine bird families & ecological guilds ####

data <- tibble(Family = c("Alcidae", "Anatidae",
                          "Diomedeidae", "Fregatidae",
                          "Gaviidae", "Hydrobatidae",
                          "Laridae", "Oceanitidae",
                          "Pelecanidae", "Phaethontidae",
                          "Phalacrocoracidae", "Podicipediformes",
                          "Procellariidae A", "Procellariidae B",
                          "Procellariidae C", "Spheniscidae",
                          "Stercorariidae", "Sulidae"),
               Functional_group = c("Flap; wing-propelled", "Flap; foot-propelled",
                                    "Flap-glide; surface", "Glide; surface & no rest on water",
                                    "Flap; foot-propelled", "Flap-glide; surface",
                                    "Flap; surface", "Flap-glide; surface",
                                    "Flap-glide; surface", "Flap; surface",
                                    "Flap; foot-propelled & no rest on water", "Flap; foot-propelled",
                                    "Flap-glide; surface", "Flap; wing-propelled",
                                    "Flap-glide; wing-propelled", "Flightless; wing-propelled",
                                    "Flap-glide; surface", "Flap-glide; plunging"),
               Data = c("Yes", "Yes",
                        "Yes", "Yes",
                        "Yes", "Yes",
                        "Yes", "Yes",
                        "Yes", "Yes",
                        "Yes", "Yes",
                        "Yes", "Yes",
                        "No", "Yes",
                        "Yes", "Yes"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create alluvium plot ####

A <- ggplot(data, aes(axis1 = Family, axis2 = Functional_group, axis3 = Data)) +
  geom_alluvium(aes(fill = Functional_group),
                aes.bind="flows", width = 1/12) +
  scale_fill_manual(values = my_cols) +
  # geom_stratum(colour = "grey20", width = 0.3) +
  geom_text(aes(label = after_stat(stratum)), stat = "stratum", size = 3, colour = "grey20",
            hjust = 0, nudge_x = -0.04) +
  scale_x_discrete(limits = c("Family", "Ecological guild", "Are data available?"),
                   expand = c(.05, .05)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw() + theme(legend.position = "none",
                     panel.grid = element_blank(), panel.border = element_blank(),
                     axis.text.y = element_blank(), axis.ticks = element_blank(),
                     axis.text.x = element_text(colour = "black"))
A

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create dataframe of marine bird families & BMR multipliers ####

data2 <- tibble(Family = as.factor(c("Alcidae", "Alcidae", "Alcidae", "Alcidae", "Alcidae",
                           "Anatidae", "Anatidae", "Anatidae", "Anatidae", "Anatidae",
                           "Laridae", "Laridae", "Laridae", "Laridae",
                           "Fregatidae", "Fregatidae",
                           "Phalacrocoracidae", "Phalacrocoracidae", "Phalacrocoracidae",
                           "Diomedeidae", "Diomedeidae", "Diomedeidae",
                           "Stercorariidae", "Stercorariidae", "Stercorariidae",
                           "Spheniscidae", "Spheniscidae", "Spheniscidae", "Spheniscidae",
                           "Sulidae", "Sulidae", "Sulidae", "Sulidae")),
                Multiplier = c(31, 6.5, 2.2, 2.2, 6.3,
                               1.4, 2.1, 5.1, 12.5, 3.5,
                               1.9, 1.1, 2.8, 6.8,
                               3.8, 2.7,
                               20.5, 11.6, 1.7,
                               2.2, 2, 0.8,
                               1.5, 1.9, 4.4,
                               1.7, 3.9, 9.9, 6.6,
                               5.3, 2.1, 3.8, 5.3),
                Activity = c("Flight", "Foraging", "On land", "On water\n (rest)", "On water\n (active)",
                             "On water\n (rest)", "On water\n (active)", "Foraging", "Flight", "Swimming",
                             "Foraging", "On land", "On water\n (rest/active)", "Flight",
                             "Flight", "On land",
                             "Flight", "Foraging", "On land",
                             "Flight", "On water\n (rest/active)", "On land",
                             "On land", "On water\n (rest/active)", "Flight",
                             "On land", "On water\n (rest)", "Swimming", "Foraging",
                             "Flight", "On land", "On water\n (rest)", "Foraging"))

data2$Family <- fct_relevel(data2$Family, "Alcidae",
            "Sulidae",
            "Laridae",
            "Fregatidae",
            "Phalacrocoracidae",
            "Diomedeidae",
            "Stercorariidae",
            "Spheniscidae",
            "Anatidae")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create stacked bar plot ####

B <- ggplot(data2, aes(x = factor(Family, level = c("Sulidae",  "Diomedeidae",
                                                    "Stercorariidae", "Anatidae",
                                                    "Phalacrocoracidae", 
                                                    "Laridae",
                                                    "Alcidae",
                                                    "Spheniscidae","Fregatidae")),
                       y = Multiplier, fill = Activity, label = Multiplier)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values=my_cols) +
  geom_text(position = "fill", size = 3, vjust = 1.5) +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(),
                     legend.box.spacing = unit(-8, "pt"),
                     axis.title.x = element_text(vjust = 5),
                     axis.text.x = element_text(angle = 20, hjust = 0.8, vjust = 1)) +
  scale_x_discrete(labels = c("Sulidae\n(Flap-glide; plunging)",
                              "Diomedeidae\n(Flap-glide; surface)",
                              "Stercorariidae\n(Flap-glide; surface)",
                              "Anatidae\n(Flap; foot-propelled)",
                              "Phalacrocoracidae\n(Flap; foot-propelled\n& no rest on water)",
                              "Laridae\n(Flap; surface)",
                              "Alcidae\n(Flap; wing-propelled)",
                              "Spheniscidae\n(Flightless; wing-propelled)",
                              "Fregatidae\n(Glide; surface\n& no rest on water")) +
  ylab("BMR multipliers (shown in proportion)") +
  xlab("Family\n(Ecological guild)") +
  guides(fill = guide_legend(nrow = 1))
B

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prep to save together ####

ggarrange(NULL, A, NULL, B, nrow = 4, heights = c(0.04, 1, 0.01, 0.7))
