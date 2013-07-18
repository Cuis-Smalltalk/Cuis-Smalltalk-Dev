'From Cuis 4.1 of 12 December 2012 [latest update: #1744] on 7 July 2013 at 6:27:56.45323 pm'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'version date highestUpdate updates versionMajor versionMinor '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/7/2013 17:53'!
versionRevision
	"Answer an array of version.revision.
	Try to do something not completely unlike 'Semantic Versioning'.
	For example, for Cuis 4.1, updated to #1744, answer
	{ 41 . 1744 }
	SystemVersion current versionRevision
	"
	^{versionMajor * 10 + versionMinor . self highestUpdate}! !

!SystemVersion methodsFor: 'initialize' stamp: 'jmv 7/7/2013 17:57'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor _ aNumber.
	versionMinor _ anotherNumber.
	date _ Date today.
	updates _ OrderedCollection new.
! !


!SystemVersion class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 18:01'!
newVersionMajor: aNumber versionMinor: anotherNumber
	| newVersion |
	newVersion _ self versionMajor: aNumber versionMinor: anotherNumber.
	newVersion
		highestUpdate: self current highestUpdate.
	Current _ newVersion
! !

!SystemVersion class methodsFor: 'instance creation' stamp: 'jmv 7/7/2013 18:00'!
versionMajor: aNumber versionMinor: anotherNumber
	"
	SystemVersion versionMajor: 4 versionMinor: 1
	"
	^self basicNew versionMajor: aNumber versionMinor: anotherNumber! !


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/7/2013 17:55'!
version
	^String streamContents: [ :strm |
		strm nextPutAll: 'Cuis '.
		versionMajor printOn: strm.
		strm nextPut: $. .
		versionMinor printOn: strm ]! !

!methodRemoval: SystemVersion class #newVersion:!
SystemVersion class removeSelector: #newVersion:!
!methodRemoval: SystemVersion #initialize!
SystemVersion removeSelector: #initialize!
!methodRemoval: SystemVersion #majorMinorVersion!
SystemVersion removeSelector: #majorMinorVersion!
!methodRemoval: SystemVersion #version:!
SystemVersion removeSelector: #version:!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor date highestUpdate updates'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SystemVersion current instVarNamed: 'versionMajor' put: 4.
SystemVersion current instVarNamed: 'versionMinor' put: 1.!

