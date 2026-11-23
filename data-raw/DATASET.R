## Pre-render map-data and diagrams to reduce pacakge dependency footprint

if (requireNamespace("DiagrammeR") && requireNamespace("DiagrammeRsvg") &&
    requireNamespace("xml2")) {
  library(DiagrammeR)
  library(DiagrammeRsvg)
  library(xml2)
  grViz(
    "
digraph 'Csquares schematic' {
layout = 'dot';
splines = spline;
node [fontname = Helvetica shape = box style = filled fillcolor =
      white class = 'csquare_nodes'];

csquares   [label = 'csquares' fillcolor = palegreen];
strucstrs  [label = '{<stars>stars::stars|<starscsq>csquares}'
            URL='#using-spatiotemporal-arrays-stars' shape = record];

subgraph cluster_dfc {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
label = 'data.frame csquare';
strucdf    [label = '{<df>data.frame|<dfcsq>csquares}'
            URL='#using-data-frames' shape = record];
strucsf    [label = '{<sf>sf::sf|<sfcsq>csquares}'
            URL='#using-simple-features-sf' shape = record];
}
subgraph cluster_char {
style = filled;
fontname = Helvetica;
fillcolor = lightcyan;
label = 'character csquare';
strucchar  [label = '{<character>character|<charcsq>csquares}'
            URL='#using-characters' shape = record];
vctr       [label = 'vctrs::vctrs_vctr'];
}

vctr -> strucchar:character
strucchar:charcsq  -> csquares;
strucdf:dfcsq      -> csquares;
strucsf:sfcsq      -> csquares;
strucstrs:starscsq -> csquares;
strucdf:df -> strucsf:sf;
strucchar:charcsq -> strucdf:dfcsq;
strucchar:charcsq -> strucsf:sfcsq;
}
") |>
    export_svg() |>
    read_xml() |>
    as.character() |>
    htmltools::HTML() |>
    writeLines(file.path("inst", "diagram.html"))
} else {
  stop("Required libraries not installed. Please install and try again")
}

if (requireNamespace("rnaturalearth") &&
    requireNamespace("rnaturalearthdata")) {
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  rnaturalearth::check_rnaturalearthdata()
  cur_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  coast <- ne_countries(scale = "medium", returnclass = "sf") |>
    st_crop(st_bbox(c(xmin = 2, ymin = 51, xmax = 5, ymax = 53.5),
                    crs = 4326)) |>
    suppressWarnings() |>
    dplyr::select()
  sf_use_s2(cur_s2)
  save(coast, file = file.path("inst", "coast.rdata"), compress = TRUE)
} else {
  stop("Required libraries not installed. Please install and try again")
}
