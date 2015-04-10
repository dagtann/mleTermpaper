library('moments'); library('grid')
tmp.agg <- aggregate(
  value ~ flip_ciri_phys + cooptation, data = tmp,
  FUN = function(x) { cbind(mean(x), kurtosis(x)-3) }
)
ggplot(
  data = tmp.agg, 
  aes(x = flip_ciri_phys, y = cooptation, fill = value[, 1], alpha = value[, 2])
) +
geom_point(alpha = 1, shape = 0, size = 1, colour = 'grey50') +
geom_tile() +
scale_fill_gradient(limits = c(3, 6), low='#feebe2', high = '#7a0177') +
scale_x_continuous(breaks = 0:8, expand = c(0, 0)) +
scale_y_continuous(expand = c(0, 0), 
  labels = c(
    'Neither Parl.\nnor party', 
    'Parl. or\nsingle party', 'Parl. and\nsingle part', 
    'Parl. and\nmult. parties'
  )
) +
guides(
  fill = guide_legend(label.position = 'right'), 
  alpha = 'none'
  #alpha = guide_legend(label.position = 'right')
) +
labs(
  x = 'Physical integrity violations', y = 'Co-optation',
  fill = expression(E(ER[t+1])),
  alpha = 'Kurtosis'
) +
theme_minimal(base_size = .8*12) +
theme(
  legend.key.size = unit(.5, units = 'lines'),
  legend.position = c(.295, 1.05), 
  legend.direction = 'horizontal',
  legend.text.align = .5,
  legend.box = 'horizontal',
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  panel.grid = element_blank(),
  plot.margin = unit(c(.7, 0, 0, 0)+.1, units = 'lines')
)
ggsave(
  file.path(pathOut, 'heatSimulatedInteraction.pdf'),
  width = 3, height = 3/1.618, dpi = 1200, family = 'serif'
)