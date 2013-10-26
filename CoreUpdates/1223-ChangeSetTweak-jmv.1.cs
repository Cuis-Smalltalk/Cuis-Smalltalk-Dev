'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 27 March 2012 at 9:05:24 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/26/2012 18:12'!
                     changeSetForPackage: aCodePackage

	| csName |
	aCodePackage ifNil: [
		^self changeSetForBaseSystem ].
	csName _ installing
		ifNil: [ 'ChangesTo-', aCodePackage name ]
		ifNotNil: [
			installing = aCodePackage packageName
				ifTrue: [ 'Installing-', installing ]
				ifFalse: [ 'Modified-', aCodePackage name, '--Installing-', installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !

