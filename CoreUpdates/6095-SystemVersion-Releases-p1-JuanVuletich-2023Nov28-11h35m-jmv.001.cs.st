'From Cuis 6.0 [latest update: #6094] on 28 November 2023 at 11:36:25 am'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor date highestUpdate '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemVersion methodsFor: 'initialization' stamp: 'jmv 11/28/2023 11:36:02'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor := aNumber.
	versionMinor := anotherNumber.! !

!methodRemoval: SystemVersion #date stamp: 'jmv 11/28/2023 11:35:56'!
SystemVersion removeSelector: #date!
!methodRemoval: SystemVersion #date: stamp: 'jmv 11/28/2023 11:35:58'!
SystemVersion removeSelector: #date:!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor highestUpdate'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
