'From Cuis6.3 [latest update: #6330] on 23 April 2024 at 12:59:35 pm'!

!Parser methodsFor: 'error handling' stamp: 'jmv 4/23/2024 12:58:45'!
notify: string at: location
	"Refer to the comment in Object|notify:."
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [
				^ self fail ]. "failure setting up syntax error"
			SyntaxErrorNotification
				inClass: encoder classEncoding
				category: category
				withCode: 
					(source contents
						copyReplaceFrom: location
						to: location - 1
						with: string , ' ->')
				doitFlag: doitFlag
				errorMessage: string
				location: location]
		ifNotNil: [
			requestor
				notify: string , ' ->'
				at: location
				in: source].
	^self fail! !

!Parser methodsFor: 'error correction' stamp: 'jmv 4/23/2024 12:56:14'!
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection := requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.

	correctSelector := UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~= $:
	   and: [correctSelector last = $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]! !

!Parser methodsFor: 'error correction' stamp: 'jmv 4/23/2024 12:57:27'!
correctVariable: proposedVariable interval: spot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action |
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
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action := UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value ifNil: [self fail].! !

!methodRemoval: Parser #sourceDelta stamp: 'jmv 4/23/2024 12:58:57'!
Parser removeSelector: #sourceDelta!
