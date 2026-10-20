'From Cuis7.9 [latest update: #8269] on 20 October 2026 at 11:58:05 am'!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name revision versionNumber version '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minRevision pathName codePackageFile minVersionNumber maxVersionNumber minVersion maxVersion '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor releaseDate updatesPrefix highestUpdate lastUpdateDate version '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!

!Feature methodsFor: 'incrementing' stamp: 'CAP 20/Oct/2026, 2:51:47 pm (UTC)'!
newVersion
	"Increment major version number."

	versionNumber := versionNumber + 1.
	revision := 0.
	version := SemanticVersion major: versionNumber minor: revision.! !

!Feature methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:50:53 pm (UTC)'!
name: theName version: theVersion revision: theRevision

	name := theName.
	versionNumber := theVersion.
	revision := theRevision.
	version := SemanticVersion major: theVersion minor: theRevision.! !


!FeatureRequirement methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:53:57 pm (UTC)'!
name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersionNumber := minVer.
	minRevision := rev.
	maxVersionNumber := maxVer.
	minVersion := SemanticVersion major: minVersionNumber minor: minRevision.
	maxVersion := SemanticVersion major: maxVersionNumber minor: Float infinity patch: Float infinity.! !


!SystemVersion methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:48:02 pm (UTC)'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor := aNumber.
	versionMinor := anotherNumber.
	version := SemanticVersion major: aNumber minor: anotherNumber.! !

!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision versionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion maxVersion minRevision pathName codePackageFile minVersionNumber maxVersionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'version versionMajor versionMinor releaseDate updatesPrefix highestUpdate lastUpdateDate'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Feature allInstancesDo: [ :f |
	f name: f name version: f versionNumber revision: f revision ].
FeatureRequirement allInstancesDo: [ :r |
	r name: r name minVersion: r minVersionNumber minRevision: r minRevision maxVersion: r maxVersionNumber ].
SystemVersion allInstancesDo: [ :v |
	v versionMajor: v versionMajor versionMinor: v versionMinor ].!

