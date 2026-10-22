library(BSol.mapR)
library(sf)
library(tmap)
library(dplyr)
library(bcctheme)

deepening_areas <- readxl::read_excel(
  "data/deepening-areas.xlsx",
  sheet="Deepening wards")

pip_areas <- readxl::read_excel(
  "data/deepening-areas.xlsx",
  sheet = "PiP msoa areas"
)

wards <- st_as_sf(Ward) %>%
  filter(Area == "Birmingham") %>%
  left_join(
    deepening_areas,
    by = join_by("Ward")
  )

msoas <- st_as_sf(MSOA21) %>%
  inner_join(
    pip_areas,
    by = join_by("MSOA21")
  ) %>%
  mutate(
    "Label" = "PiP Area"
  )

brum_shape <- st_union(wards)

tm_shape(wards) +
  tm_polygons(
    fill = "Deepening Area",
    fill.scale = tm_scale(
      label.na = "",
      value.na = "white",
      values = c("#84329B","#D00070","#DC582A","#FFAD00","#75BC22","#00A9E0", "#3c3c3b")
      ),
    lwd = 0.5,
    col_alpha = 0.4
    ) +
  tm_shape(brum_shape) +
  tm_borders(
    lwd = 2
  ) +
  tm_shape(msoas) +
  tm_polygons(
    col = "Label",
    lwd = 1.5,
    fill_alpha = 0,
    col.scale = tm_scale_categorical(
      values = "black"
    ),
    col.legend = tm_legend(
      title = ""
    )
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

tmap_save(filename = "output/pip-map.png")