#' Labeling for Mosaic plots.
#'
#' @export
#'
#' @description The labels for mosaic plots positioned in the center of each rectangle
#'
#'
#' @inheritParams ggplot2::layer
#' @param divider Divider function. The default divider function is mosaic() which will use spines in alternating directions. The four options for partitioning:
#' \itemize{
#' \item \code{vspine} Vertical spine partition: width constant, height varies.
#' \item \code{hspine}  Horizontal spine partition: height constant, width varies.
#' \item \code{vbar} Vertical bar partition: height constant, width varies.
#' \item \code{hbar}  Horizontal bar partition: width constant, height varies.
#' }
#' @param offset Set the space between the first spine
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
#'
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived)) +
#'   geom_mosaic_label(aes(weight=Freq, x=product(Class), fill=Survived))
#'
#' ggplot(data=titanic, aes(weight=Freq, x=product(Class), fill=Survived)) +
#'   geom_mosaic() +
#'   geom_mosaic_label()
#'
#' ggplot(data=titanic, aes(weight=Freq, x=product(Class, Sex),  fill=Survived)) +
#'   geom_mosaic(divider = c("vspine", "hspine", "hspine")) +
#'   geom_mosaic_label(divider = c("vspine", "hspine", "hspine"), size = 2)
#'
#' ggplot(data=titanic, aes(weight=Freq, x=product(Class), conds=product(Sex),  fill=Survived)) +
#'   geom_mosaic(divider = c("vspine", "hspine", "hspine")) +
#'   geom_mosaic_label(divider = c("vspine", "hspine", "hspine"))
geom_mosaic_label <- function(mapping = NULL, data = NULL, stat = "mosaic",
                               position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                               show.legend = NA, inherit.aes = TRUE, ...)
{
  ll <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMosaicLabel,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = TRUE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )

  ll$compute_aesthetics_og <- ll$compute_aesthetics
  ll$compute_aesthetics <- compute_aesthetics_mosaic
  ll$setup_layer <- setup_layer_mosaic

  ll
}


#' @rdname geom_mosaic_label
#' @format NULL
#' @usage NULL
#' @importFrom grid grobTree
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate select
#' @export
GeomMosaicLabel <- ggplot2::ggproto(
  "GeomMosaicLabel", ggplot2::Geom,
  setup_data = function(data, params) {
    #cat("setup_data in GeomMosaic\n")
    #browser()
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(width = 0.1, linetype = "solid", size=2.7,
                             shape = 19, colour = "black",
                             fill = "grey30", alpha = 1, stroke = 0.1,
                             linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  draw_panel = function(data, panel_scales, coord) {
    #cat("draw_panel in GeomMosaic\n")
    #browser()
    if (all(is.na(data$colour)))
      data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    sub <- subset(data, level==max(data$level))
    text <- sub
    text <- tidyr::nest(text, -label)

    text <-
      dplyr::mutate(
        text,
        coords = purrr::map(data, .f = function(d) {
          data.frame(
            x = (d$xmin + d$xmax)/2,
            y = (d$ymin + d$ymax)/2,
            #size = 2.88,
            angle = 0,
            hjust = 0.5,
            vjust = 0.5,
            alpha = NA,
            family = "",
            fontface = 1,
            lineheight = 1.2,
            dplyr::select(d, -x, -y, size, -alpha)
          )
        })
      )

    text <- tidyr::unnest(text, coords)

    sub$fill <- NA
    sub$colour <- NA
    sub$size <- sub$size/10
    ggplot2:::ggname("geom_mosaic_label", grobTree(
      GeomRect$draw_panel(sub, panel_scales, coord),
      GeomText$draw_panel(text, panel_scales, coord)
    ))
  },

  check_aesthetics = function(x, n) {
    #browser()
    ns <- vapply(x, length, numeric(1))
    good <- ns == 1L | ns == n


    if (all(good)) {
      return()
    }

    stop(
      "Aesthetics must be either length 1 or the same as the data (", n, "): ",
      paste(names(!good), collapse = ", "),
      call. = FALSE
    )
  },

  draw_key = ggplot2::draw_key_rect
)



