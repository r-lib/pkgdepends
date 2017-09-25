
box_chars <- function() {
  fancy <- getOption("pkgdepends.fancy.tree") %||% l10n_info()$`UTF-8`
  if (fancy) {
    list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",                   # leaf
      "j" = "\u251C"                    # junction
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",                       # leaf
      "j" = "+"                         # junction
    )
  }
}

remotes_draw_tree <- function(self, private, pkgs) {
  refs <- vcapply(private$remotes, "[[", "ref")
  pkgs <- get_remotes_from_regexps(pkgs, refs)
  if (is.null(private$resolution)) stop("Need to resolve remotes first")
  chars <- box_chars()

  print_tree <- function(self, private, pkg, n = integer(), mx = integer()) {
    level <- length(n) - 1
    prefix <- vcapply(seq_along(n), function(i) {
      if (n[i] < mx[i]) {
        if (i == length(n)) {
          paste0(chars$j, chars$h)
        } else {
          paste0(chars$v, " ")
        }
      } else if (n[i] == mx[i] && i == length(n)) {
        paste0(chars$l, chars$h)
      } else {
        "  "
      }
    })
    cat(prefix, pkg, "\n", sep = "")
    deps <- unique(unlist(
      lapply(private$resolution$packages[[pkg]]$files, "[[", "deps")
    ))
    for (d in seq_along(deps)) {
      print_tree(self, private, deps[[d]], c(n, d), c(mx, length(deps)))
    }
  }

  for (pkg in pkgs) { cat("\n"); print_tree(self, private, pkg) }

  invisible(self)
}

get_remotes_from_regexps <- function(rx, refs) {
  if (is.null(rx)) {
    refs
  } else {
    assert_that(is.character(rx))
    pkgs <- unique(unlist(
      lapply(rx, grep, remotes, value = TRUE)
    ))
  }
}
