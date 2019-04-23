'From Cuis 5.0 of 7 November 2016 [latest update: #3699] on 23 April 2019 at 10:14:09 am'!

!Parser methodsFor: 'error correction' stamp: 'jmv 4/23/2019 10:13:10'!
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
	delta _ userSelection last - aSpot last.
	spot _ aSpot first + delta to: aSpot last + delta.
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

