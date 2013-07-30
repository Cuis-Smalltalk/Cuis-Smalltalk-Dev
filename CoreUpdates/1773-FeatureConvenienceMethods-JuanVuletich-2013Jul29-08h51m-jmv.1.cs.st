'From Cuis 4.2 of 25 July 2013 [latest update: #1772] on 29 July 2013 at 8:56:57.524493 am'!

!FeatureRequirement commentStamp: '<historical>' prior: 0!
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion
  path -- nil or the (cached)  pathName of a file containing the package with this feature

Usually use the convenience methods:
	Feature require: 'Sound'
	Feature require: 'Tests' version: 1.0

Try any of:
  (FeatureRequirement name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) require 		"Specific version.revision, or any greater version, up to max"

This should load packages #Compression and #Sound


  (FeatureRequirement name: #'Sound') print
  (FeatureRequirement name: #'Sound' version: 1) print
  (FeatureRequirement name: #'Sound' version: 1 revision: 12) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: nil) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: nil maxVersion: 3) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) print!


!Feature class methodsFor: 'convenience' stamp: 'jmv 7/29/2013 08:56'!
require: featureName
	"
	Feature require: 'StyledTextInstaller'
	Feature require: 'Sound'
	Feature require: 'Tests'
	"
	(FeatureRequirement name: featureName) require! !

!Feature class methodsFor: 'convenience' stamp: 'jmv 7/29/2013 08:56'!
require: featureName version: versionNumber
	"
	Feature require: 'StyledTextInstaller' version: 1.0
	Feature require: 'Sound' version: 1.0
	Feature require: 'Tests' version: 1.0
	"
	(FeatureRequirement name: featureName version: versionNumber) require! !


!Feature class reorganize!
('instance creation' baseSystemFeature name:version:revision:)
('convenience' require: require:version:)
!

