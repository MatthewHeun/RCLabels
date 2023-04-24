## Context

`RCLabels` v0.1.4 is a minor update that fixes one bug and 
incorporates some backend changes, 
including GitHub actions updated to latest version.
See NEWS.md for details.

## Test environments (11 in total) and R CMD check results

* Local macOS X installation 13.3.1 (Ventura), R4.3.0 (2023-04-21)
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
    * `devtools::check_win_release()`, R version 4.3.0 (2023-04-21 ucrt)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 0
    * `devtools::check_win_devel()`, R Under development (unstable) (2023-04-23 r84305 ucrt)
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
    
    
    
    
    
## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


    
    
    
    
    
    
    
    

* Windows (on win-builder):
* rhub:
    * `devtools::check_rhub()`
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


