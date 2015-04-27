with(                                  ## stated correlation
  subset(org.dta, year >= 1981 & year <= 2004),
  cor(fh_ordinal, flip_ciri_phys, use = 'complete.obs')
)

with(                    ## absolute frequencies co-optation
  subset(org.dta, year >= 1981 & year <= 2004),
  addmargins(table(cooptation_discrete, useNA = 'ifany'))
)
2386-165