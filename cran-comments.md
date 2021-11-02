## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul Sheridan <paul.sheridan.stats@gmail.com>'

  Possibly misspelled words in DESCRIPTION:
    LTO (17:70, 19:5)
  
  Package has a FOSS license but eventually depends on the following
  package which restricts use:
    isa2

0 errors ✓ | 0 warnings ✓ | 1 note x

## Remarks
* LTO is an acronym, not a misspelled word.

* We call a function from the isa2 package which uses a FOSS license, but do not use any of the isa2 package code directly.

## Downstream dependencies
* There are currently no downstream dependencies for this package.

* This is a new release.


