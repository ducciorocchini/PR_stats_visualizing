# https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/2041-210X.13477

# First map

library(colorist)
data("fiespa_occ")

# First map with single distributions
m1 <- metrics_pull(fiespa_occ)
p1 <- palette_timecycle(fiespa_occ)
map_multiples(m1, p1, ncol=4, labels=names(fiespa_occ))

# Common map
m1_distill <- metrics_distill(fiespa_occ)
map_single(m1_distill, p1)

