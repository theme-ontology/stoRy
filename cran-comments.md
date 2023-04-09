This is a patch release fixing some code and urls that broke as a result of changes to the Theme Ontology website.

# R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- winbuilder x86_64-w64-mingw32 (64-bit) (r-devel)
- winbuilder x86_64-w64-mingw32 (64-bit) (r-release)
- winbuilder x86_64-w64-mingw32 (64-bit) (r-oldrelease)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Remarks
This is a resubmission. The following errors have now been fixed:
 
```
  Package CITATION file contains call(s) to old-style personList() or
  as.personList().  Please use c() on person objects instead.
  Package CITATION file contains call(s) to old-style citEntry().  Please
  use bibentry() instead.
```
