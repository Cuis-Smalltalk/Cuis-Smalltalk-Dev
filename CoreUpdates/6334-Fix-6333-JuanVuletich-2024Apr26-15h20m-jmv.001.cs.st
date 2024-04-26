'From Cuis6.3 [latest update: #6333] on 26 April 2024 at 3:27:49 pm'!

!Parser methodsFor: 'error correction' stamp: 'jmv 4/26/2024 15:26:32'!
sourceDelta

	(encoder selector = Scanner doItSelector or: [ encoder selector = Scanner doItInSelector ]) ifTrue: [
		requestor notNil ifTrue: [
			^requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ]]].
	^ 0! !


!Parser methodsFor: 'error handling' stamp: 'jmv 4/26/2024 15:21:11'!
notify: string at: location
	| adjustedLocation |
	adjustedLocation := location + self sourceDelta.
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [
				^ self fail ]. "failure setting up syntax error"
			SyntaxErrorNotification
				inClass: encoder classEncoding
				category: category
				withCode: 
					(source contents
						copyReplaceFrom: adjustedLocation
						to: adjustedLocation - 1
						with: string , ' ->')
				doitFlag: doitFlag
				errorMessage: string
				location: adjustedLocation]
		ifNotNil: [
			requestor
				notify: string , ' ->'
				at: adjustedLocation
				in: source].
	^self fail! !

!Parser methodsFor: 'error correction' stamp: 'jmv 4/26/2024 15:20:48'!
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

	userSelection := requestor selectionInterval.

	delta := self sourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first + delta to: interval last + delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector := UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~= $:
	   and: [correctSelector last = $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!Parser methodsFor: 'error correction' stamp: 'jmv 4/26/2024 15:21:02'!
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

	userSelection := requestor selectionInterval.
	delta := self sourceDelta.
	spot := aSpot first + delta to: aSpot last + delta.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action := UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value ifNil: [self fail].
	! !

