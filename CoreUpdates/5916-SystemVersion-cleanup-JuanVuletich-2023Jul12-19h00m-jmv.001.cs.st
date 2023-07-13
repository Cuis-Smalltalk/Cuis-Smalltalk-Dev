'From Cuis 6.0 [latest update: #5910] on 12 July 2023 at 7:01:12 pm'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor date highestUpdate updates '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/12/2023 18:01:04'!
highestUpdate
	highestUpdate ifNil: [
		highestUpdate := 0 ].
	^ highestUpdate! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 7/12/2023 18:01:10'!
registerUpdate: changeSetOrPackageName

	changeSetOrPackageName initialIntegerOrNil ifNotNil: [ :number |
		highestUpdate := self highestUpdate max: number ]! !

!SystemVersion methodsFor: 'initialization' stamp: 'jmv 7/12/2023 18:01:14'!
versionMajor: aNumber versionMinor: anotherNumber
	versionMajor := aNumber.
	versionMinor := anotherNumber.
	date := Date today.
! !

!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor date highestUpdate'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
