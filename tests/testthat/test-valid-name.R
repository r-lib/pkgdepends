test_that("is_valid_package_name", {
  expect_snapshot({
    is_valid_package_name("foo")
    is_valid_package_name("pkg")
    is_valid_package_name("f\u00e1f")
    is_valid_package_name("foo-bar")
    is_valid_package_name("x")
    is_valid_package_name("1xyz")
    is_valid_package_name("dotted.")
  })
})
