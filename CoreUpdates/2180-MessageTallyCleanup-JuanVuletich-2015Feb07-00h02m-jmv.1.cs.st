'From Cuis 4.2 of 25 July 2013 [latest update: #2178] on 7 February 2015 at 12:04:46.865406 am'!

!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/7/2015 00:03'!
tallySendsTo: receiver inBlock: aBlock
	"
	MessageTally tallySends: [3.14159 printString]
	"
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."
	| prev tallies startTime totalTime |
	startTime _ Time localMillisecondClock.
	tallies _ MessageTally new class: aBlock receiver class method: aBlock method.
	tallies reportOtherProcesses: true.	"Do NOT filter nodes with nil process"
	prev _ aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					(receiver == nil or: [ current receiver == receiver ])
						ifTrue: [ tallies tally: current by: 1 ]].
				prev _ current]].

	totalTime _ Time localMillisecondClock - startTime / 1000.0.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s
					nextPutAll: 'This simulation took ';
					nextPutAll: totalTime printString;
					nextPutAll: ' seconds.';
					newLine.
				tallies fullPrintExactOn: s ]))
			label: 'Spy Results'
			wrap: false ]! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/7/2015 00:04'!
tallySends: aBlock
	"
	MessageTally tallySends: [3.14159 printString]
	"

	^ self tallySendsTo: nil inBlock: aBlock! !

!methodRemoval: MessageTally class #tallySendsTo:inBlock:showTree:!
MessageTally class removeSelector: #tallySendsTo:inBlock:showTree:!
