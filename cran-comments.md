## Context

`RCLabels` v0.1.7 is a minor update that

- updates the GitHub Actions for 
  continuous integration testing and
  code coverage,
- updates the citation URL to point to Zenodo, and
- adds a code of conduct and contributing guidelines.

No new code or changes to features are included.
See NEWS.md for details.

## Test environments (12 in total) and R CMD check results

* Local macOS X installation 13.5.1 (Ventura), R4.3.2 (2023-10-31)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions:
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (oldrel-1)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.3.0 (2023-04-21 ucrt)               ***********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2023-04-23 r84305 ucrt)       ***********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_oldrelease()`, R version 4.x.x ()                           ***********************
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 2
              - checking for non-standard things in the check directory ... NOTE
                Found the following files/directories:
                ''NULL''
                This notes appears to result from a mal-configuration 
                of this test environment. 
                No other test environment generated this note.
              - checking for detritus in the temp directory ... NOTE
                Found the following files/directories:
                'lastMiKTeXException'
                This notes appears to result from a mal-configuration 
                of this test environment. 
                No other test environment generated this note.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
              - checking HTML version of manual ... NOTE
                Skipping checking HTML validation: no command 'tidy' found. 
                This note received only on rhub.
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1
              - checking HTML version of manual ... NOTE
                Skipping checking HTML validation: no command 'tidy' found. 
                This note received only on rhub.
    
    
## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
