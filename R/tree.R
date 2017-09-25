
remotes_draw_tree <- function(self, private, pkgs) {
  refs <- vcapply(private$remotes, "[[", "ref")
  pkgs <- get_remotes_from_regexps(pkgs, refs)
  if (is.null(private$resolution)) stop("Need to resolve remotes first")

  recdeps <- function(pkg) {
    deps <- unique(unlist(
      lapply(private$resolution$packages[[pkg]]$files, "[[", "deps")
    ))
    list(node = pkg, children = lapply(deps, recdeps))
  }

  trees <- lapply(pkgs, recdeps)
  for (t in trees) { cat("\n"); print_tree(t) }

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

print_tree <- function(tree) {
  chars <- box_chars()

  pt <- function(tree, n = integer(), mx = integer()) {
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
    cat(prefix, tree$node, "\n", sep = "")
    for (d in seq_along(tree$children)) {
      pt(tree$children[[d]], c(n, d), c(mx, length(tree$children)))
    }
  }

  pt(tree)
}
