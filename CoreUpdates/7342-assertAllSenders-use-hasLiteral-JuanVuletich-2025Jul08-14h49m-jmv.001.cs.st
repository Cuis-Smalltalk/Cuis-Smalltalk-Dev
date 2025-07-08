'From Cuis7.5 [latest update: #7341] on 8 July 2025 at 2:57:54 pm'!

!ChangeSelector class methodsFor: 'pre-conditions' stamp: 'jmv 7/8/2025 14:52:43'!
assertAllSenders: senders send: aSelector

	| invalidSenders |
	
	invalidSenders := senders reject: [ :aSender | aSender hasLiteral: aSelector ].
	invalidSenders notEmpty ifTrue: [ self signalInvalidSenders: invalidSenders of: aSelector ].! !

