'From Cuis 5.0 of 7 November 2016 [latest update: #3697] on 17 April 2019 at 3:05:02 pm'!

!Parser methodsFor: 'error correction' stamp: 'jmv 4/17/2019 15:01:56'!
correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta spots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.
	delta _ userSelection last - aSpots last last.
	spots _ aSpots collect: [ :interval | interval first + delta to: interval last + delta ].
	requestor selectFrom: spots first first to: spots last last.

	correctSelector _ UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

