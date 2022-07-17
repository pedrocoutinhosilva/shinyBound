# Requirements
The test battery includes unit, snapshot and UI testing. To run all tests make sure to include all packages in the `suggests` section of the DESCRIPTION file:
```
testthat (>= 3.0.0),
rvest,
devtools,
covr
```

Check the `Running tests` section if you would prefer to disable UI tests.

# Running tests
Tests can be run using `devtools`
```
devtools::test()
```

# Test Coverage
Coverage is aimed to be 100% whenever possible. You can check the current coverage report using one of the following:
```
devtools::test_coverage()
covr::code_coverage()
covr::report()
```
