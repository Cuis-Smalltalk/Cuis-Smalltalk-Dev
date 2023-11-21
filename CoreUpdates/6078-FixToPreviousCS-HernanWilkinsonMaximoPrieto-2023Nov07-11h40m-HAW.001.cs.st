'From Cuis 6.0 [latest update: #6068] on 7 November 2023 at 3:27:46 pm'!

!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 14:48:45'!
selectedEntryFormatted

	| selectedEntry |
	
	selectedEntry := self entries at: menuMorph selected.
	^currentRange correspondsToSymbol
		ifTrue: [ '#', selectedEntry ]
		ifFalse: [ selectedEntry separateKeywords ]! !

