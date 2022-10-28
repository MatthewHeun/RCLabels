## Context

`RCLabels` v0.1.2 is a minor update that solidifies some function names
and fixes several bugs. 
In addition, this version includes a pull request from Hadley Wickham
for changes in `tidyselect`.
See NEWS.md for details.

## Test environments (11 in total) and R CMD check results

* Local macOS X installation 12.6 (Monterey), R4.2.1 (2022-06-23 r81394)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions:
    * macOS-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * windows-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (devel)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (release)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * ubuntu-latest (oldrel-1)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.2.1 (2022-06-23 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2022-10-11 r83083 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2022, R-devel, 64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1 
              - checking for detritus in the temp directory ... NOTE
                Found the following files/directories:
                'lastMiKTeXException'
                This appears to be a mal-configuration 
                of this test environment. 
                No other test environment generated this note.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * PREPERROR: It looks like the virtual machine was unable to spin up.
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 2
              - Skipping checking HTML validation: no command 'tidy' found. 
                This appears to be a mal-configureation of the test environment.
                No other test environments generate this note.
              - Examples with CPU (user + system) or elapsed time > 5s: get_nouns.
                These examples did not generate this note in any other test environment.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

