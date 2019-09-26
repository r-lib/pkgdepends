
style_man <- function(where = "@description") {
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
