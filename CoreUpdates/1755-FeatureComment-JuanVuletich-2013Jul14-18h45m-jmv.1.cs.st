'From Cuis 4.1 of 12 December 2012 [latest update: #1751] on 14 July 2013 at 6:46:28.304201 pm'!

!Feature commentStamp: '<historical>' prior: 0!
A named Feature is something which identifies the primary role of a Package.

  name -- a symbol naming the feature
  version -- an integer which increments at each incompatible change in package interface
  revision -- an integer which increments with each backwards compatible package change
  path -- nil or the (cached)  pathName of a file containing the package with this feature



Try any of:
  (Feature name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (Feature name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (Feature name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"

This should load packages #Compression and #Sound!

