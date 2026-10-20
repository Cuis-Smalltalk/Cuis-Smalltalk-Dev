'From Cuis7.9 [latest update: #8266] on 20 October 2026 at 11:14:44 am'!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision versionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName codePackageFile minVersionNumber maxVersionNumber '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!

!Feature methodsFor: 'incrementing' stamp: 'CAP 20/Oct/2026, 2:11:18 pm (UTC)'!
newVersion
	"Increment major version number."

	version := version + 1.
	versionNumber := version.
	revision := 0! !

!Feature methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:11:22 pm (UTC)'!
name: theName version: theVersion revision: theRevision

	name     := theName.
	version  := theVersion.
	versionNumber := version.
	revision := theRevision! !


!FeatureRequirement methodsFor: 'initialization' stamp: 'CAP 20/Oct/2026, 2:12:02 pm (UTC)'!
name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersion := minVer.
	minVersionNumber := minVersion.
	minRevision := rev.
	maxVersion := maxVer.
	maxVersionNumber := maxVersion.! !

!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision versionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName codePackageFile minVersionNumber maxVersionNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Feature allInstancesDo: [ :f |
	f instVarNamed: 'versionNumber' put: (f instVarNamed: 'version') ].
FeatureRequirement allInstancesDo: [ :r |
	r instVarNamed: 'minVersionNumber' put: (r instVarNamed: 'minVersion') .
	r instVarNamed: 'maxVersionNumber' put: (r instVarNamed: 'maxVersion') ].!

