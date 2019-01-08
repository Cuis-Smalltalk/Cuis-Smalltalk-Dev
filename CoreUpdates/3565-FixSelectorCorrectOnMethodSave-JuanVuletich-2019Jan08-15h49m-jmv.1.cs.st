'From Cuis 5.0 of 7 November 2016 [latest update: #3564] on 8 January 2019 at 4:19:03 pm'!

!Parser methodsFor: 'error correction' stamp: 'jmv 1/8/2019 16:18:53'!
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection correctedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.

	correctSelector _ UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	correctedSpots _ spots with: proposedKeyword keywords collect: [ :interval :toCorrect | interval first to: interval first + toCorrect size-1].
	self substituteSelector: correctSelector keywords wordIntervals: correctedSpots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

