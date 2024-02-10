# verify_extracted_package: errors if archive DESCRIPTION is not in the root directory

    Code
      run(f2)
    Condition
      Error:
      ! '<tempdir>/<tempfile>/test2.tgz' is not a valid binary, it is missing test2/Meta/package.rds and test2/DESCRIPTION.

# verify_extracted_package: can handle multiple DESCRIPTION files

    Code
      run(f4)
    Condition
      Error:
      ! '<tempdir>/<tempfile>/test4.tgz' is not a valid binary, it is missing test4/DESCRIPTION.

# verify_extracted_package: fails if the binary does not contain package.rds

    Code
      run(f5)
    Condition
      Error:
      ! '<tempdir>/<tempfile>/test5.tgz' is not a valid binary, it is missing test5/Meta/package.rds.

# verify_extracted_package: fails if the DESCRIPTION file is empty

    Code
      run(f6)
    Condition
      Error:
      ! '<tempdir>/<tempfile>/test6.tgz' is not a valid binary, empty DESCRIPTION file at 'test6/DESCRIPTION'.

