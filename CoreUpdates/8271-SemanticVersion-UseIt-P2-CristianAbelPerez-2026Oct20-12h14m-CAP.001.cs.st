'From Cuis7.9 [latest update: #8270] on 20 October 2026 at 12:22:26 pm'!

!Feature commentStamp: '<historical>' prior: 0!
A package has a ProvidedFeature which specifies its version.  See class FeatureSpec.

  name -- a symbol naming the feature
  version -- the concrete semantic version.!


!FeatureRequirement commentStamp: 'CAP 16/Oct/2026, 7:10:40 pm (UTC)' prior: 0!
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  path -- nil or the (cached)  pathName of a file containing the package with this feature

Usually use the convenience methods:
	Feature require: 'Sound'
	Feature require: 'Tests' version: 1.0

Try any of:
  (FeatureRequirement name: #Sound) require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #Sound version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #Sound version: 1 revision: 0) require.  	"Ask for a specific version.revision"
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: 3) require 		"Specific version.revision, or any greater version, up to max"

This should load packages #Compression and #Sound


  (FeatureRequirement name: #Sound) print
  (FeatureRequirement name: #Sound version: 1) print
  (FeatureRequirement name: #Sound version: 1 revision: 12) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: nil) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: nil maxVersion: 3) print
  (FeatureRequirement name: #Sound minVersion: 1 minRevision: 23 maxVersion: 3) print!


!SystemVersion commentStamp: '<historical>' prior: 0!
SystemVersion current.

Instance variables:
	version. A SemanticVersion: major and minor are the Cuis version, patch is the number of the last update installed, regardless of prefix.
	releaseDate. If nil, not a release.
	updatesPrefix. Valid prefixes for update change sets. nil or '' -> No prefix.
	lastUpdateDate. Date of last update installed.!


!Feature methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 3:18:39 pm (UTC)'!
version

	^ version
! !

!Feature methodsFor: 'accessing' stamp: 'CAP 18/Oct/2026, 12:22:21 am (UTC)'!
version: aVersion

	aVersion hasWildcards ifTrue: [
		^self error: 'A package version can not have wildcards: ', aVersion printString ].
	version := aVersion! !

!Feature methodsFor: 'initialization' stamp: 'CAP 15/Oct/2026, 8:12:54 pm (UTC)'!
name: aName version: aVersion

	name     := aName.
	version  := aVersion.! !


!Feature class methodsFor: 'instance creation' stamp: 'CAP 15/Oct/2026, 8:09:45 pm (UTC)'!
name: aName version: aVersion

	^​ self new name: aName asSymbol version: aVersion.
! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'CAP 20/Oct/2026, 3:18:01 pm (UTC)'!
maxVersion

	^maxVersion! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'CAP 16/Oct/2026, 7:08:37 pm (UTC)'!
minVersion 

	^minVersion ! !

!FeatureRequirement methodsFor: 'initialization' stamp: 'CAP 16/Oct/2026, 7:08:16 pm (UTC)'!
name: featureName minVersion: minVer maxVersion: maxVer

	name := featureName asSymbol.
	minVersion := minVer.
	maxVersion := maxVer.! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'CAP 16/Oct/2026, 7:16:03 pm (UTC)'!
name: aSymbol minVersion: aMinVersion maxVersion: aMaxVersion

	(aMinVersion isKindOf: SemanticVersion) ifFalse: [
		self error: 'Minimum version must be a SemanticVersion.' ].
	(aMaxVersion isKindOf: SemanticVersion) ifFalse: [
		self error: 'Max version must be a SemanticVersion.' ].
	
	^self new name: aSymbol minVersion: aMinVersion maxVersion: aMaxVersion.
! !


!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:24 am (UTC)'!
version

	^version! !

