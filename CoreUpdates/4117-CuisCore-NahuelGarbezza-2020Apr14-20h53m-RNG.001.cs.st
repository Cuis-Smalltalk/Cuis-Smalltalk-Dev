'From Cuis 5.0 [latest update: #4113] on 14 April 2020 at 8:54:36 pm'!

!Parser methodsFor: 'expression types' stamp: 'RNG 4/14/2020 20:53:34'!
cascade
	" {; message} => CascadeNode."

	| receiverNode messageNodes |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	receiverNode := parseNode cascadeReceiver.
	messageNodes := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := receiverNode.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			messageNodes addLast: parseNode].
	self createCascadeNodeWith: receiverNode and: messageNodes! !

