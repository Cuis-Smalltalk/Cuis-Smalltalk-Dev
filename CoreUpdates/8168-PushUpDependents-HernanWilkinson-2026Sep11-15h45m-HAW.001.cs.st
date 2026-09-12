'From Cuis7.9 [latest update: #8163] on 12 September 2026 at 11:31:30 am'!

!AssignmentNode methodsFor: 'testing' stamp: 'HAW 11/Sep/2026, 7:41:10 pm (UTC)'!
sendsMessageToSelf: aSelector

	^ value sendsMessageToSelf: aSelector! !


!BraceNode methodsFor: 'testing' stamp: 'HAW 11/Sep/2026, 7:41:10 pm (UTC)'!
sendsMessageToSelf: aSelector

	^ elements anySatisfy: [ :anElement | anElement sendsMessageToSelf: aSelector ]! !


!CascadeNode methodsFor: 'testing' stamp: 'HAW 11/Sep/2026, 7:45:13 pm (UTC)'!
sendsMessageToSelf: aSelector

	^ messages anySatisfy: [ :aMessageNode |
		aMessageNode sendsMessageToSelf: aSelector ]! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 11/Sep/2026, 7:45:13 pm (UTC)'!
isSentToSelfNamed: aSelector

	^ self receiverOrCascadeReceiver isSelfPseudoVariable and: [ self selector isNamed: aSelector ]! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 11/Sep/2026, 7:45:55 pm (UTC)'!
sendsMessageToSelf: aSelector

	^ (self isSentToSelfNamed: aSelector)
		or: [ self receiverOrCascadeReceiver sendsMessageToSelf: aSelector ]
		or: [ self arguments anySatisfy: [ :anArgument | anArgument sendsMessageToSelf: aSelector ] ]! !

