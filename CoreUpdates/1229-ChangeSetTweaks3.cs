'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 9:23:17 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:14'!
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
			thisNumber _ Installing asNumber.
			thisNumber = (self lastUsedNumber + 1) ifTrue: [
				LastUsedNumber _ thisNumber ].
			csName _ 'Affects-BaseSystem--Install-', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName ]! !

