'From Cuis 5.0 of 7 November 2016 [latest update: #3725] on 5 May 2019 at 8:59:01 pm'!

!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 5/5/2019 19:22:41'!
selectionDoItSourceCodeHeaderSizeWithContext: hasContext 
		
	^(String streamContents: [ :stream | self selectionDoItSourceCodeHeaderWithContext: hasContext into: stream ]) size! !

!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 5/5/2019 19:06:58'!
selectionDoItSourceCodeHeaderWithContext: hasContext into: stream

	"I use previousContext and not ThisContext as in the parser to avoid
	name collision. Also, previousContext is more intention revealing - Hernan"
	stream
		nextPutAll: (hasContext ifTrue: [ Scanner doItInSelector, ' previousContext' ] ifFalse: [ Scanner doItSelector ]);
		newLine;
		newLine! !


!Parser methodsFor: 'error correction' stamp: 'jmv 5/5/2019 20:54:13'!
correctSourceDelta

	| userSelectionDelta |
	userSelectionDelta _ requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ].
	encoder selector = Scanner doItSelector ifTrue: [ 
		^ (Scanner selectionDoItSourceCodeHeaderSizeWithContext: false) - userSelectionDelta ].
	encoder selector = Scanner doItInSelector ifTrue: [ 
		^ (Scanner selectionDoItSourceCodeHeaderSizeWithContext: true) - userSelectionDelta ].
	
	^ 0! !


!Parser methodsFor: 'error correction' stamp: 'HAW 5/5/2019 19:42:21'!
correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta adjustedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.

	delta := self correctSourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first - delta to: interval last - delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector _ UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!Parser methodsFor: 'error correction' stamp: 'HAW 5/5/2019 19:38:20'!
correctVariable: proposedVariable interval: aSpot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action delta spot |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	delta _ self correctSourceDelta.
	spot _ aSpot first - delta to: aSpot last - delta.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 5/5/2019 19:34:47'!
selectionDoItSourceCodeIn: evalContext

	^String streamContents: [ :stream |
		Scanner selectionDoItSourceCodeHeaderWithContext: evalContext notNil into: stream.
		stream nextPutAll: self selectionAsStream upToEnd ]! !

