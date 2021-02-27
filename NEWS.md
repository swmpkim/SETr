# SETr 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.  
* Correction to `calc_change_cumu()`. Prior version used position to subtract off first pin reading from the rest, but had not first arranged by date - so the wrong reading could be subtracted. This version incorporates arranging so should be correct.  
