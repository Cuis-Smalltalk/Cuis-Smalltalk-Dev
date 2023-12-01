'From Cuis6.0 [latest update: #6108] on 30 November 2023 at 4:07:33 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 11/30/2023 16:00:54'!
baseSystemPrefixFor: aChangeSetNumber

	^String streamContents: [ :strm |
		strm
			nextPutAll: SystemVersion current updatesPrefix;
			nextPutAll: (aChangeSetNumber printString padded: #left to: 4 with: $0) ]! !


!SystemVersion methodsFor: 'accessing' stamp: 'jmv 11/30/2023 15:54:01'!
registerUpdate: changeSetOrPackageName

	(changeSetOrPackageName integerOrNilAt: SystemVersion current updatesPrefix size+1)
		ifNotNil: [ :number |
			highestUpdate := self highestUpdate max: number.
			lastUpdateDate := Date today ].! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 11/30/2023 16:01:39'!
baseSystemNameFor: aChangeSetNumber

	^String streamContents: [ :strm |
		strm
			nextPutAll: (self baseSystemPrefixFor: aChangeSetNumber);
			nextPutAll: '-CuisCore-';
"			nextPutAll: Utilities authorName asCamelCase;"
			nextPutAll: 'AuthorName';
"			nextPutAll: '-';
			nextPutAll: Utilities authorInitials asCamelCase;"
			nextPutAll: '-' ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 11/30/2023 16:03:37'!
changeSetForBaseSystem

	| csName numberToUse |
	self notInstallOrTestRun
		ifTrue: [
			numberToUse := self currentBaseCSNumber.
			AllChangeSets
				detect: [ :any | any name beginsWith: (ChangeSet baseSystemPrefixFor: numberToUse) ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName := (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifFalse: [
			csName := 'Affects-BaseSystem--', Installing.
			^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ].! !

!ChangeSet class methodsFor: 'removing' stamp: 'jmv 11/30/2023 16:05:59'!
removeChangeSet: aChangeSet
	"Remove the given changeSet. Caller must assure that it's ok to do this."

	"If the change set to be removed was using the current number for base system changes,
	and it is not empty, start using the following number for the upcoming changes."
	((aChangeSet name beginsWith: (ChangeSet baseSystemPrefixFor: ChangeSet currentBaseCSNumber)) and: [
		aChangeSet isEmpty not ])
			ifTrue: [
				ChangeSet incrementCurrentBaseCSNumber ].

	AllChangeSets remove: aChangeSet ifAbsent: nil.
	aChangeSet wither.! !

