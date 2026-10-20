'From Cuis7.9 [latest update: #8266] on 20 October 2026 at 10:55:26 am'!
!classDefinition: #SemanticVersion category: #'System-Support'!
Magnitude subclass: #SemanticVersion
	instanceVariableNames: 'major minor patch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!SemanticVersion commentStamp: '<historical>' prior: 0!
Support for semantic versioning of artifacts. See https://semver.org/
Cuis uses instances SemanticVersion for SystemVersion, Feature and FeatureRequirement.
Cuis doesn't enforce actually following the rules for Semantic Versioning for packages, but we encourage their adoption.
Note: Cuis itself doesn't strictly follow semantic versioning, as our release process is based on Dates and not on Features.
-------
Semantic Versioning 2.0.0
Summary

Given a version number MAJOR.MINOR.PATCH, increment the:

    MAJOR version when you make incompatible API changes
    MINOR version when you add functionality in a backward compatible manner
    PATCH version when you make backward compatible bug fixes

Additional labels for pre-release and build metadata are available as extensions to the MAJOR.MINOR.PATCH format.!


!SemanticVersion methodsFor: 'initialization' stamp: 'CAP 17/Oct/2026, 10:16:54 pm (UTC)'!
major: aMajorNumber minor: aMinorNumber patch: aPatchNumber

	major := aMajorNumber.
	minor := aMinorNumber.
	patch := aPatchNumber.
! !

!SemanticVersion methodsFor: 'components' stamp: 'CAP 17/Oct/2026, 10:16:54 pm (UTC)'!
major

	^major! !

!SemanticVersion methodsFor: 'components' stamp: 'CAP 17/Oct/2026, 10:16:54 pm (UTC)'!
minor

	^minor! !

!SemanticVersion methodsFor: 'components' stamp: 'CAP 17/Oct/2026, 10:16:54 pm (UTC)'!
patch

	^patch! !

!SemanticVersion methodsFor: 'converting' stamp: 'CAP 18/Oct/2026, 10:39:46 pm (UTC)'!
anyMinor

	^self class major: major minor: Float infinity patch: Float infinity! !

!SemanticVersion methodsFor: 'converting' stamp: 'CAP 18/Oct/2026, 10:39:46 pm (UTC)'!
anyPatch

	^self class major: major minor: minor patch: Float infinity! !

!SemanticVersion methodsFor: 'converting' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
nextMajor

	^self class major: major + 1.! !

!SemanticVersion methodsFor: 'comparing' stamp: 'CAP 18/Oct/2026, 12:55:30 am (UTC)'!
< another

	self class == another class ifFalse: [ ^another >= self ].
	^major < another major or: [
		major = another major and: [
			minor < another minor or: [
				minor = another minor and: [
					patch < another patch ]]]]! !

!SemanticVersion methodsFor: 'comparing' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
= another

	self class == another class ifFalse: [ ^false ].
	^major = another major and: [ minor = another minor and: [ patch = another patch ] ]! !

!SemanticVersion methodsFor: 'comparing' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
hash

	^self printString hash! !

!SemanticVersion methodsFor: 'printing' stamp: 'CAP 17/Oct/2026, 10:17:34 pm (UTC)'!
printOn: aStream

	aStream
		nextPutAll: (self printStringOfComponent: major);
		nextPut: $.;
		nextPutAll: (self printStringOfComponent: minor);
		nextPut: $.;
		nextPutAll: (self printStringOfComponent: patch)! !

!SemanticVersion methodsFor: 'printing' stamp: 'CAP 18/Oct/2026, 12:34:51 am (UTC)'!
printStringOfComponent: aNumber

	^aNumber = Float infinity
		ifTrue: [ '*' ]
		ifFalse: [ aNumber printString ]! !

!SemanticVersion methodsFor: 'queries' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
hasWildcards
	"Wildcards are only allowed at the end, so one in the patch means there is at least one"

	^patch = Float infinity! !


!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 17/Oct/2026, 11:34:51 pm (UTC)'!
fromString: aString
	"Builds a version from a string like '1.2.3'.
	A trailing * means any value for that component and the ones after it:
		SemanticVersion fromString: '1.2.3'
		SemanticVersion fromString: '1.2.*'
		SemanticVersion fromString: '1.*.*'
		SemanticVersion fromString: '*.*.*'
	A * followed by a number, as in '1.*.3', is an error."

	| components |

	components := aString substringsSeparatedBy: $..
	(self isWellFormed: components) ifFalse: [ ^self error: 'Invalid semantic version: ', aString ].
	
	^self
		major: (self componentFromString: components first)
		minor: (self componentFromString: components second)
		patch: (self componentFromString: components third)! !

!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 18/Oct/2026, 12:46:01 am (UTC)'!
infinity

	^self major: Float infinity minor: Float infinity patch: Float infinity! !

!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 16/Oct/2026, 5:25:31 pm (UTC)'!
major: aMajorNumber 

	^self major: aMajorNumber minor: 0! !

!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 16/Oct/2026, 5:25:38 pm (UTC)'!
major: aMajorNumber minor: aMinorNumber

	^self major: aMajorNumber minor: aMinorNumber patch: 0! !

!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 17/Oct/2026, 10:17:10 pm (UTC)'!
major: aMajorNumber minor: aMinorNumber patch: aPatchNumber

	^self new major: aMajorNumber minor: aMinorNumber patch: aPatchNumber! !

!SemanticVersion class methodsFor: 'instance creation' stamp: 'CAP 18/Oct/2026, 12:59:54 am (UTC)'!
zero

	^self major: 0! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 17/Oct/2026, 11:04:09 pm (UTC)'!
areWildcardsTrailingIn: components

	| wildcardCount |

	wildcardCount := components count: [ :component | self isWildcard: component ].
	^(components last: wildcardCount) allSatisfy: [ :component | self isWildcard: component ]! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 18/Oct/2026, 12:33:22 am (UTC)'!
componentFromString: aComponentString

	^(self isWildcard: aComponentString)
		ifTrue: [ Float infinity ]
		ifFalse: [ aComponentString asNumber ]! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
isNumericIdentifier: aComponentString
	"A number without leading zeroes, as SemVer asks: 0, or a positive digit followed by digits"

	aComponentString isEmpty ifTrue: [ ^false ].
	(aComponentString allSatisfy: #isDigit) ifFalse: [ ^false ].
	^aComponentString size = 1 or: [ aComponentString first ~= $0 ]! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 18/Oct/2026, 12:00:09 am (UTC)'!
isValidComponent: aComponentString

	^(self isWildcard: aComponentString) or: [ self isNumericIdentifier: aComponentString ]! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 18/Oct/2026, 12:29:03 am (UTC)'!
isWellFormed: components

	^components size = 3
		and: [ (components allSatisfy: [ :component | self isValidComponent: component ])
		and: [ self areWildcardsTrailingIn: components ] ]! !

!SemanticVersion class methodsFor: 'private' stamp: 'CAP 17/Oct/2026, 11:04:09 pm (UTC)'!
isWildcard: aComponentString

	^aComponentString = '*'! !

