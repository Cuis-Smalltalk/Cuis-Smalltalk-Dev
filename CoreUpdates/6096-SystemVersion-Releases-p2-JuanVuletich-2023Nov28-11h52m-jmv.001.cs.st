'From Cuis 6.0 [latest update: #6095] on 28 November 2023 at 11:57:32 am'!
!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor highestUpdate lastUpdateDate releaseDate updatesPrefix '
	classVariableNames: 'Current '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemVersion commentStamp: 'jmv 11/28/2023 11:52:43' prior: 0!
SystemVersion current.

Instance variables:
	versionMajor, versionMinor
	releaseDate. If nil, not a release.
	updatesPrefix. Valid prefixes for update change sets. nil or '' -> No prefix.
	hightestUpdate. Number of last update installed, regardless of prefix.
	latestUpdateDate. Date of last update installed.!


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 11/28/2023 11:56:11'!
registerUpdate: changeSetOrPackageName

	changeSetOrPackageName initialIntegerOrNil ifNotNil: [ :number |
		highestUpdate := self highestUpdate max: number.
		lastUpdateDate := Date today ]! !

!SystemVersion methodsFor: 'services' stamp: 'jmv 11/28/2023 11:55:32'!
updatesPrefixesDo: aBlock
	"So far we only handle one prefix for update change set files.
	This could change some day."

	aBlock value: (updatesPrefix ifNil: '')! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 11/27/2023 10:03:12'!
installNewUpdates
 	
	| updatesFileDirectory |
	updatesFileDirectory := DirectoryEntry smalltalkImageDirectory / 'CoreUpdates'.
	updatesFileDirectory exists ifFalse: [
		updatesFileDirectory := DirectoryEntry smalltalkImageDirectory parent / 			'Cuis-Smalltalk-Dev/CoreUpdates' ].

	updatesFileDirectory exists
		ifFalse: [ ^self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ].
		
	ChangeSet installNewUpdates: updatesFileDirectory! !

!classDefinition: #SystemVersion category: #'System-Support'!
Object subclass: #SystemVersion
	instanceVariableNames: 'versionMajor versionMinor releaseDate updatesPrefix highestUpdate lastUpdateDate'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'System-Support'!
