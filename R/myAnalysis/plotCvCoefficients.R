library('grid')
levels(pdta[, 'variable']) <- c("Intercept", 
  "Parl. or single party", "Parl. and single party",
  "Parl. and mult. parties", "Physical integrity violations",
  "gled_lgdppc", "gled_lpop", "geddes_personalYes", 
  "geddes_monarchYes", "geddes_partyYes", 
  "log.archigos_pastleaderfail", "powthy_pastattempts",
  "log.prio_conflict_intra", "prio_conflict_inter",
  "log.banks_genstrike", "log.banks_riot", 
  "log.banks_antigovdem", "tlin", "tsqu", 
  "cooptation_discreteNoLegLeast1Par.flip_ciri_phys", 
  "cooptation_discreteLeg1Par.flip_ciri_phys",
  "cooptation_discretelegMore1Par.flip_ciri_phys"
)
p <- ggplot(
  data = subset(
     pdta, variable %in% c(
      "Parl. or single party",
      "Parl. and single party",
      "Parl. and mult. parties",
      "Physical integrity violations"
     )
   ),
  aes(x = orderObs, y = coefficient)
) +
geom_ribbon(
  aes(
    ymin = coefficient-qnorm(.975)*se,
    ymax = coefficient+qnorm(.975)*se
  ),
  linetype = 'solid', alpha = 0.5, size = .1, fill = '#e0e0e0', colour = '#e0e0e0'
) +
geom_point(size = .6, colour = '#ff7f00') +
geom_text(
  aes(label = cname, colour = factor(tag)), 
  angle = 0, size = 2

) +
scale_colour_manual(values = c('transparent', 'black')) +
guides(colour = 'none') +
labs(y = 'Coefficient estimate') +
theme_minimal(base_size = .75*12) +
theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = unit(c(0, 1, 0, 0)+.1, units = 'lines') 
) +
facet_wrap(~variable, scales = 'free_y')
ggsave(
  plot = p,
  file.path(pathOut, 'cvCountryCoef.pdf'), 
  width = 3, height = 3/1.618, family = 'serif'
)
## ---  finishing -----------------------------------------------
detach(package:grid)
rm(p)
