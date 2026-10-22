library(BSol.mapR)
library(sf)
library(tmap)
library(dplyr)
library(bcctheme)

adult_inactivity <- readxl::read_excel(
  "data/deepening-areas.xlsx",
  sheet="Inactivity levels Adults wards"
  )

child_inactivity <- readxl::read_excel(
  "data/deepening-areas.xlsx",
  sheet="Inactivity levels CYP wards"
)


wards <- st_as_sf(Ward) %>%
  filter(Area == "Birmingham") %>%
  left_join(
    adult_inactivity,
    by = join_by("Ward")
  ) %>%
  left_join(
    child_inactivity,
    by = join_by("Ward")
  ) %>%
  mutate(
    `Inactivity Rate (Adults)` = 100 * `Adult inactivity levels`,
    `Inactivity Rate (Children)` = 100 * `Child inactivity levels`
    )

brum_shape <- st_union(wards)

tm_shape(wards) +
  tm_polygons(
    fill = "Inactivity Rate (Adults)",
    fill.scale = tm_scale_continuous(
      label.na = "",
      values = bcc_pal("purple")(10),
      label.format=list(fun=function(x) paste0(formatC(x, digits=1, format="f"), " %"))
    ),
    lwd = 0.5,
    col_alpha = 0.4
  ) +
  tm_shape(brum_shape) +
  tm_borders(
    lwd = 2
  ) +
  tm_layout(
    legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))
  ) + 
  tm_layout(
    legend.position = c(0.08, 0.95),
    #scale = 0.8,
    #legend.height = 15,
    legend.frame.alpha = 0,
    legend.frame.lwd = 0,
    legend.bg.alpha = 0.4,
    inner.margins = 0.08,
    frame = FALSE) +
  tm_compass(
    type = "8star",
    size = 5,
    position = c(0.7, 0.2),
    color.light = "white"
  )

tmap_save(filename = "output/adult-inactivity-v1.png")


tm_shape(wards) +
  tm_polygons(
    fill = "Inactivity Rate (Children)",
    fill.scale = tm_scale_continuous(
      label.na = "",
      values = bcc_pal("purple")(10),
      label.format=list(fun=function(x) paste0(formatC(x, digits=1, format="f"), " %"))
    ),
    lwd = 0.5,
    col_alpha = 0.4
  ) +
  tm_shape(brum_shape) +
  tm_borders(
    lwd = 2
  ) +
  tm_layout(
    legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))
  ) + 
  tm_layout(
    legend.position = c(0.08, 0.95),
    #scale = 0.8,
    #legend.height = 15,
    legend.frame.alpha = 0,
    legend.frame.lwd = 0,
    legend.bg.alpha = 0.4,
    inner.margins = 0.08,
    frame = FALSE) +
  tm_compass(
    type = "8star",
    size = 5,
    position = c(0.7, 0.2),
    color.light = "white"
  )

tmap_save(filename = "output/child-inactivity-v1.png")