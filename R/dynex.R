
dynex <- function() {
  if (packageVersion("roxygen2") < "6.1.99.9001") {
    stop("pkgdepends needs at least roxygen  7.0.0 (or dev)")
  }
  registerS3method("roclet_output", "roclet_dynex", roclet_output_dynex,
                   envir = asNamespace("roxygen2"))
  registerS3method("roclet_process", "roclet_dynex", roclet_process_dynex,
                   envir = asNamespace("roxygen2"))
  asNamespace("roxygen2")$roclet(c("dynex", "rd"))
}

roclet_process_dynex <- function(x, blocks, env, base_path) {
  blocks <- lapply(blocks, mark_r6_class)
  NextMethod()
}

mark_r6_class <- function(block) {
  if ("R6ClassGenerator" %in% class(block$object$value)) {
    block$tags <- c(
      block$tags,
      list(asNamespace("roxygen2")$roxy_tag("concept", raw = "R6-marker", val = "R6-marker"))
    )
  }
  block
}

roclet_output_dynex <- function(x, results, base_path, ...) {
  results <- lapply_with_names(results, dynamic_examples)
  NextMethod()
}

dynamic_examples <- function(page) {
  page <- add_styles(page)
  concept <- page$get_section("concept")$value
  isr6 <- "R6-marker" %in% concept
  if (isr6) {
    page$add_section(
      asNamespace("roxygen2")$rd_section("concept", setdiff(concept, "R6-marker")),
      overwrite = TRUE
    )
    make_r6_examples_dynamic(page)
  }
  add_dynamic_examples_section(page)
  make_examples_dynamic(page)
  page
}

add_styles <- function(page) {
  if (page$has_section("description")) {
    page$add_section(asNamespace("roxygen2")$rd_section("description", style_man()))
  }
  page
}

style_man <- function(where = NULL) {
  c(
    where,
    paste0(
      '\\if{html}{\\out{',
      '<link rel="stylesheet" type="text/css" href="../doc/assets/extra.css">',
      '<script src="../doc/assets/rd.js"></script>',
      '}}'
    )
  )
}

make_r6_examples_dynamic <- function(page) {
  # We need to walk the Rd and transform the 'Examples' subsections
  # within the "Method" subsections
  rd <- tools::parse_Rd(textConnection(page$get_section("rawRd")$value))
  is_methods_section <- function(x) {
    identical(attr(x, "Rd_tag"), "\\section") && x[[1]] == "Methods"
  }
  methods <- which(vapply(rd, is_methods_section, TRUE))
  if (length(methods) == 0) return(page)

  rd[methods] <- lapply(rd[methods], process_r6_methods)

  value <- deparse_rd(rd)
  page$add_section(asNamespace("roxygen2")$rd_section("rawRd", value), overwrite = TRUE)

  page
}

deparse_rd <- function(rd) {
  ch <- as.character(rd)
  bad <- which(ch %in% c("\\if", "\\href")) + 1L
  del <- bad

  lapply(bad + 1L, function(idx) {
    level <- 1L
    while (idx <= length(ch) && level > 0L) {
      if (ch[idx] == "}") {
        if (level == 1L) del <<- c(del, idx)
        level <- level - 1L
      } else if (ch[idx] == "{") {
        level <- level + 1L
      }
      idx <- idx + 1L
    }
  })

  if (length(del) > 0) ch <- ch[-del]
  paste(ch, collapse = "")
}

process_r6_methods <- function(methods) {
  is_method_section <- function(x) {
    identical(attr(x, "Rd_tag"), "\\subsection") && grepl("^Method ", x[[1]])
  }
  imethods <- vapply(methods[[2]], is_method_section, TRUE)
  if (!any(imethods)) return(methods)

  methods[[2]][imethods] <- lapply(methods[[2]][imethods], process_r6_method)
  methods
}

process_r6_method <- function(method) {
  is_examples <- function(x) {
    identical(attr(x, "Rd_tag"), "\\subsection") && x[[1]] == "Examples"
  }
  ex <- vapply(method[[2]], is_examples, TRUE)
  if (!any(ex)) return(method)

  method[[2]][ex] <- lapply(method[[2]][ex], process_r6_examples)
  method
}

process_r6_examples <- function(ex) {
  pref <- vapply(ex[[2]], attr, "", "Rd_tag") == "\\preformatted"
  if (!any(pref)) return(ex)
  attr(ex[[2]][[which(pref)]], "Rd_tag") <- "\\examplesx"
  if (attr(ex[[2]][[which(pref)]][[1]], "Rd_tag") == "VERB" &&
      ex[[2]][[which(pref)]][[1]] == "# Needs: TRUE\n") {
    ex[[2]][[which(pref)]][[1]] <- NULL
  }
  ex
}

needs_line_rx <- "#+ Needs[^:]*:"

match_needs_line <- function(x) {
  rx <- needs_line_rx
  grepl(paste0("^", rx), x) | grepl(paste0("\n", rx), x)
}

# Take the \examples{} section, and repeat it again, but this time
# put it in a \section{} and also make the examples dynamic

add_dynamic_examples_section <- function(page) {
  if (!page$has_section("examples")) return(page)
  ex <- page$get_section("examples")
  if (!any(match_needs_line(ex$value))) return(page)
  val <- paste0(
    "\\section{Examples}{\n",
    "\\examplesx{",
    ex$value,
    "}",
    "}"
  )

  page$add_section(asNamespace("roxygen2")$rd_section("rawRd", val))
  page
}

# Transform the \examples{} section, to contain proper guards for
# all dynamic examples, and also put the whole thing into \testonly{}

make_examples_dynamic <- function(page) {
  if (!page$has_section("examples")) return(page)
  ex <- page$get_section("examples")
  if (!any(match_needs_line(ex$value))) return(page)
  val <- paste0(
    "\\testonly{\n",
    dynex_add_tests(ex$value),
    "}"
  )
  page$add_section(asNamespace("roxygen2")$rd_section("examples", val), overwrite = TRUE)
  page
}

get_needs_blocks <- function(lines) {
  needs <- which(match_needs_line(lines))
  cid <- cut(
    seq_along(lines),
    breaks = c(if (! 1 %in% needs) 0, needs, length(lines) + 1L),
    right = FALSE,
    include.lowest = TRUE)
  blocks <- tapply(lines, cid, c)
}

dynexx <- function(code) {
  code <- sub("\\s+$", "", sub("^\\s+", "", code))
  code <- gsub("\n\n", "\n", code, fixed = TRUE)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  blocks <- get_needs_blocks(lines)

  drop_trailing_empty_lines <- function(x) {
    empty <- grepl("^\\s*$", x)
    wempty <- which(empty)
    drop <- wempty[wempty > max(which(!empty))]
    if (length(drop) > 0) x <- x[-drop]
    x
  }

  dynex_block <- function(str) {
    str <- drop_trailing_empty_lines(str)
    if (match_needs_line(str[1])) {
      call <- sub(needs_line_rx, "", str[1])
      omit <- tryCatch(!isTRUE(eval(parse(text = call))), error = function(e) TRUE)
      if (omit) {
        str <- paste0("#x ", str)
      } else if (call == "TRUE") {
        str <- str[-1]
      }
    }
    paste(str, collapse = "\n")
  }

  paste0(
    "\\preformatted{",
    paste(vapply(blocks, dynex_block, character(1)), collapse = "\n\n"),
    "}"
  )
}

dynex_add_tests <- function(code) {
  lines <- unlist(strsplit(code, "\n", fixed = TRUE))
  blocks <- get_needs_blocks(lines)
  dynex_block <- function(str) {
    if (match_needs_line(str[1])) {
      call <- str_trim(sub(needs_line_rx, "", str[1]))
      if (call != "TRUE") {
        str <- c(
          paste0("if (", call, ") {"),
          str[-1],
          "}"
        )
      } else {
        str <- str[-1]
      }
    }
    paste(str, collapse = "\n")
  }

  paste(vapply(blocks, dynex_block, character(1)), collapse = "\n\n")
}
