'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 3 April 2012 at 10:07:28 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/3/2012 10:07'!
                    changeSetForBaseSystem

	| csName thisNumber |
	Installing

		ifNil: [
			csName _ self baseSystemNameFor: self lastUsedNumber.
			ChangeSorter allChangeSets
				detect: [ :any | any name beginsWith: csName ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					LastUsedNumber _ self lastUsedNumber + 1.
					csName _ (self baseSystemNameFor: self lastUsedNumber),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSorter existingOrNewChangeSetNamed: csName ]]

		ifNotNil: [
			thisNumber _ Installing asInteger.
			thisNumber = (self lastUsedNumber + 1) ifTrue: [
				LastUsedNumber _ thisNumber ].
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName ]! !

