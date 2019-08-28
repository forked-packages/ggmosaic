
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

parse_product_formula <- getFromNamespace("parse_product_formula", "productplots")

#' Wrapper for a list
#'
#' @param ... Unquoted variables going into the product plot.
#' @export
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Survived))
product <- function(...) {
  # exprs gives you back exactly what you gave it
  rlang::exprs(...)
}

#' @rdname geom_mosaic
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{xmin}{location of bottom left corner}
#' \item{xmax}{location of bottom right corner}
#' \item{ymin}{location of top left corner}
#' \item{ymax}{location of top right corner}
#' }
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(),
                        show.legend = NA, inherit.aes = TRUE, offset = 0.01, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaic,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}


#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @importFrom tidyr unite_
#' @export
StatMosaic <- ggplot2::ggproto(
  "StatMosaic", ggplot2::Stat,
  #required_aes = c("x"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    #cat("setup_params from StatMosaic\n")
    #browser()
    params
  },

  setup_data = function(data, params) {
    #cat("setup_data from StatMosaic\n")
    #browser()
    data
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, divider, offset) {
    #cat("compute_panel from StatMosaic\n")
    #browser()
    vars <- names(data)[grep("x__", names(data))]
    conds <- names(data)[grep("conds[0-9]+__", names(data))]

    if (length(vars) == 0) formula <- "1"
    else formula <-  paste(vars, collapse="+")

    formula <- paste("weight~", formula)

    if (length(conds) > 0) formula <- paste(formula, paste(conds, collapse="+"), sep="|")

    df <- data
    if (!in_data(df, "weight")) {
      df$weight <- 1
    }

    res <- prodcalc(df, formula=as.formula(formula),
                    divider = divider, cascade=0, scale_max = TRUE,
                    na.rm = na.rm, offset = offset)

    # need to set x variable - I'd rather set the scales here.
    prs <- parse_product_formula(as.formula(formula))
    p <- length(c(prs$marg, prs$cond))
    if (is.function(divider)) divider <- divider(p)

    # the level at which things are labelled could be made a parameter.
    # At the moment the deepest level is being labelled.
    dflist <- list(data=subset(res, level==max(res$level)), formula=as.formula(formula), divider=divider)
    scx <- productplots::scale_x_product(dflist)
    scy <- productplots::scale_y_product(dflist)

    # res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    res <- subset(res, level==max(res$level))

    cols <- c(prs$marg, prs$cond)

    if (length(cols) > 1) {
      df <- res[,cols]
      df <- tidyr::unite_(df, "label", cols, sep="\n")

      res$label <- df$label
    } else res$label <- as.character(res[,cols])

    res$x <- list(scale=scx)
    if (!is.null(scales$y)) {
      # only set the y scale if it is a product scale, otherwise leave it alone
      if ("ScaleContinuousProduct" %in% class(scales$y))
        res$y <- list(scale=scy)
    }

    # merge res with data:
    # is there a fill variable?
    fill_idx <- grep("x__fill", names(data))
    if (length(fill_idx) > 0) {
      fill_res_idx <- grep("x__fill", names(res))
      res$fill <- res[[fill_res_idx]]
    }
    alpha_idx <- grep("x__alpha", names(data))
    if (length(alpha_idx) > 0) {
      alpha_res_idx <- grep("x__alpha", names(res))
      res$alpha <- res[[alpha_res_idx]]
    }

    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    res
  }
)

