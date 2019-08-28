#' Custom function for mosaic layer
#'
#' straight copy from layer-.r in ggplot2, except for the first block
#' to remove the vars mapping from the plot
#' @noRd
compute_aesthetics_mosaic <- function(self, data, plot) {
  # move the x aesthetics out of the mapping
  #browser()
  if (!is.null(plot$mapping$x))
    plot$mapping <- plot$mapping[-grep("x", names(plot$mapping))]

  self$compute_aesthetics_og(data, plot)
}

#' function to setup the vars mapping in mosaics
#' @noRd
setup_layer_mosaic <- function(self, data, plot) {
  #browser()
  if (!is.null(self$mapping$y) | !is.null(plot$mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else self$mapping$y <- structure(1L, class = "productlist")

  # get x mappings from plot or self if x mapping is not in plot
  aes_x <- plot$mapping$x %||% self$mapping$x

  # move the (unquoted) mappings$x to aes_x and then create var_x
  if (!is.null(aes_x)) {
    if (!is.null(self$mapping$x))
      aes_x <- rlang::eval_tidy(self$mapping$x)
    if (!is.null(plot$mapping$x))
      aes_x <- rlang::eval_tidy(plot$mapping$x)

      var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- plot$mapping$fill %||% self$mapping$fill
  var_fill <- ""

  if (!is.null(aes_fill)) {
    if (!is.null(self$mapping$x))
      aes_fill <- rlang::quo_text(self$mapping$fill)
    if (!is.null(plot$mapping$x)) {
      aes_fill <- rlang::quo_text(plot$mapping$fill)
    }

    var_fill <- paste0("x__fill__", aes_fill)

    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {

      self$mapping[[var_fill]] <- plot$mapping$fill %||% self$mapping$fill
    }
  }

  aes_alpha <- plot$mapping$alpha %||% self$mapping$alpha
  var_alpha <- ""

  if (!is.null(aes_alpha)) {
    if (!is.null(self$mapping$alpha))
      aes_alpha <- rlang::quo_text(self$mapping$alpha)
    if (!is.null(plot$mapping$alpha))
      aes_alpha <- rlang::quo_text(plot$mapping$alpha)

    var_alpha <- paste0("x__alpha__", aes_alpha)

    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      self$mapping[[var_alpha]] <- plot$mapping$alpha %||% self$mapping$alpha
    }
  }


  if (!is.null(aes_x)) {
    self$mapping$x <- structure(1L, class = "productlist")

    for (i in seq_along(var_x)) {
      self$mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }


  aes_conds <- plot$mapping$conds %||% self$mapping$conds
  if (!is.null(aes_conds)) {
    if (!is.null(self$mapping$conds))
      aes_conds <- rlang::eval_tidy(self$mapping$conds)
    if (!is.null(plot$mapping$conds))
      aes_conds <- rlang::eval_tidy(plot$mapping$conds)

    self$mapping$conds <- structure(1L, class = "productlist")

    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))

    for (i in seq_along(var_conds)) {
      self$mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }

  data
}
