'From Cuis6.0 [latest update: #6104] on 30 November 2023 at 12:22:47 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 11/29/2023 10:23:51'!
highestUpdate: anInteger lastUpdateDate: aDate
	highestUpdate := anInteger.
	lastUpdateDate := aDate.! !


!SystemVersion class methodsFor: 'instance creation' stamp: 'jmv 11/29/2023 10:24:06'!
newVersionMajor: aNumber versionMinor: anotherNumber
	"
	SystemVersion newVersionMajor: 6 versionMinor: 0.
	"
	| newVersion |
	newVersion := self versionMajor: aNumber versionMinor: anotherNumber.
	newVersion
		highestUpdate: self current highestUpdate
		lastUpdateDate: self current lastUpdateDate.
	Current := newVersion
! !

!methodRemoval: SystemVersion #highestUpdate: stamp: 'jmv 11/30/2023 12:18:17'!
SystemVersion removeSelector: #highestUpdate:!
