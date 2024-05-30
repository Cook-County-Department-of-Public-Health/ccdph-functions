### Reference: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2 ###

### Create list of colors ###
ccdph_colors<-c(
  'primary blue' = "#0070ff",
  'white' = "#ffffff",
  'black' = "#000000",
  'primary yellow' = "#ffbf00",
  'light blue' = "#73ccec",
  'secondary blue' = "#485cc7",
  'grey' = "#f6f6f6",
  'red' = "#e73c3e",
  'pink' = "#f65c87",
  'orange' = "#d97652",
  'light pink' = "#f8bed6",
  'secondary yellow' = "#f6dd00",
  'green' = "#6cc24a",
  'dark blue' = "#0c2340"
)

### Function to extract color as hex codes ###
ccdph_cols<-function(...){
  cols<-c(...)
  if(is.null(cols)){return (ccdph_colors)}
  ccdph_colors[cols]
}

### Make palettes ###
ccdph_palettes <- list(
  `primary` = ccdph_cols("primary blue","white","black"),
  `secondary`= ccdph_cols("primary yellow","light blue","secondary blue","grey"),
  `secondary expanded`= ccdph_cols("primary yellow","light blue","secondary blue","grey","red","pink","orange","light pink","secondary yellow","green","dark blue"),
  `warm` = ccdph_cols("primary yellow","red","pink","orange","light pink","secondary yellow"),
  `blues` = ccdph_cols("primary blue","light blue","secondary blue","dark blue","grey"),
  `cool` = ccdph_cols("primary blue","light blue","secondary blue","green","dark blue","grey")
)



### Function to inerpolate palettes ###
ccdph_pal <- function(palette = "secondary", reverse = FALSE, ...) {
  pal <- ccdph_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

## add (number) to this function to interpolate additional colors
## example: ccdph_pal("blues")(10) expands from the 4 included blues to 10



### Function to create 'color' scale for ggplot2 ###
#' @param palette Character name of palette in ccdph_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_ccdph <- function(palette = "secondary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ccdph_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ccdph_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

### Function to create 'fill' scale for ggplot2 ###
#' @param palette Character name of palette in ccdph_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_ccdph <- function(palette = "secondary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ccdph_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ccdph_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

##example: continuous variable
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_ccdph(discrete = FALSE, palette = "cool")

##example: discrete variable
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_ccdph()