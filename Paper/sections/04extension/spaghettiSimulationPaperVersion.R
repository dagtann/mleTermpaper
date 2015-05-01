## This file generates a spaghetti plot evaluating the simulation
## It generates a pdf.
## --------------------------------------------------------------
tmp <- within(tmp,                    ## generate speaking labels
  lab <- factor(cooptation, levels = 0:3, 
    labels = c(
      'Neither', 
      'Either-or', 'Parl. and\nsingle party', 
      'Parl. and\nmult. parties'
    )
  )
)
tmp.stoch <- within(tmp.stoch,        ## generate speaking labels
  lab <- factor(cooptation, levels = 0:3, 
    labels = c(
      'Neither', 
      'Either-or', 'Parl. and\nsingle party', 
      'Parl. and\nmult. parties'
    )
  )
)
tmp.agg <- within(tmp.agg,            ## generate speaking labels
  lab <- factor(cooptation, levels = 0:3, 
    labels = c(
      'Neither', 
      'Either-or', 'Parl. and\nsingle party', 
      'Parl. and\nmult. parties'
    )
  )
)
tmp.stoch2 <- aggregate(
  value ~ flip_ciri_phys + cooptation, data = tmp.stoch, 
  FUN = function(x) { cbind(min(x), max(x))}
)
tmp.stoch2 <- within(tmp.stoch2,            ## generate speaking labels
  lab <- factor(cooptation, levels = 0:3, 
    labels = c(
      'Neither', 
      'Either-or', 'Parl. and\nsingle party', 
      'Parl. and\nmult. parties'
    )
  )
)
head(tmp.stoch2)
## --- proceed to plotting --------------------------------------
library('grid')
p <- ggplot(
  data = tmp, 
  aes(x = flip_ciri_phys, y = value)
) +
geom_ribbon(
  data = tmp.stoch2, 
  aes(
    x = flip_ciri_phys, 
    y = 5, ymin = value[, 1], ymax = value[, 2],
    group = NULL
  ),
  linetype = 'solid', alpha = 0.5, size = .1, 
  fill = '#e0e0e0', colour = '#e0e0e0'
) +
geom_line(aes(group = variable), alpha = .1, size = .2) +
geom_line(
  data = tmp.agg, 
  aes(x = flip_ciri_phys, y = value, group = NULL), 
    size = .2, show_guide = FALSE, colour = 'white' #'#d9d9d9'
) +
labs(
  y = expression(plain(ER)[t+1]),
  x = 'Physical integrity violations'
) +
facet_wrap(~lab, nrow = 1) +
theme_minimal(base_size = .6*12, base_family = 'serif') +
theme(
  panel.grid.minor = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0)+.1, units = 'lines')
)

ggsave(plot = p, 
  file = file.path(pathPaper, 'sections', '04extension', 'spaghettiInteraction.pdf'),
  width = 4, height = 4/1.618, dpi = 1200
)
## --- finishing ------------------------------------------------
detach(package:grid)
rm(p, tmp, tmp.stoch, tmp.stoch2, tmp.agg)
## END