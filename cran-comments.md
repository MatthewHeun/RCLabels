## Context

`RCLabels` v0.0.4 is a new submission to CRAN
that assists with manipulating matrix row and column labels.
`RCLabels` is re-factored out of `matsbyname`
so other packages can take advantage of its functions
while preparing matrices.
`RCLabels` includes additional functionality, too,
especially the functions `replace_by_pattern()`
and `match_by_pattern()`, which provide highly customizable
regex pattern matching for row and column labels.
If `RCLabels` is accepted by CRAN, 
I will release a new version of `matsbyname` 
that removes several functions 
that are now in `RCLabels`.


## Test environments (11 in total) and R CMD check results

* local macOS X install 12.0.1 (Monterey), R4.1.2
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: macOS-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: windows-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-latest (devel)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-latest (release)
    * ERRORs: 0
    * WARNINGs: 0
    * NOTEs: 0
* GitHub Actions: ubuntu-latest (oldrel-1)
    * ERRORs: 1 --- The error is caused by use of the new native pipe (|>).
    * WARNINGs: 0
    * NOTEs: 0
* Windows (on win-builder):
    * `devtools::check_win_release()`, R version 4.1.2 (2021-11-01)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 1 --- As expected, the note identifies `RCLabels` as a new submission to CRAN.
    * `devtools::check_win_devel()`, R Under development (unstable) (2021-12-03 r81290)
        * ERRORs: 0
        * WARNINGs: 0
        * NOTEs: 1 --- New submission.
* rhub:
    * `devtools::check_rhub()`
        * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1 --- New submission.
        * Ubuntu Linux 20.04.1 LTS, R-release, GCC
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1 --- New submission.
        * Fedora Linux, R-devel, clang, gfortran
            * ERRORs: 0
            * WARNINGs: 0
            * NOTEs: 1 --- New submission.


## revdepcheck results

Because this is a new submission, there are no reverse dependencies.
 
