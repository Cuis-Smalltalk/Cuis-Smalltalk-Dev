'From Cuis7.1 [latest update: #6575] on 9 August 2024 at 10:47:21 am'!

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 8/9/2024 10:39:09'!
transformToDo: encoder
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange blockRange limitIsAssignedTo |
	block := arguments last.
	"First check for valid arguments"
	(block notNil
	 and: [block isBlockNode
	 and: [block numberOfArguments = 1
	 and: [block firstArgument isVariableReference "As with debugger remote vars"]]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	(limit := arguments at: 1) isVariableReference ifTrue:
		[limitIsAssignedTo := false.
		 block nodesDo:
			[:node|
			(node isAssignmentNode and: [node variable = limit]) ifTrue:
				[limitIsAssignedTo := true]].
		 limitIsAssignedTo ifTrue:
			[^false]].
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := SelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	blockRange := encoder rawSourceRanges at: block ifAbsent: [1 to: 0].
	blockVar := block firstArgument.
	initStmt := AssignmentNode new variable: blockVar value: receiver.
	limit isVariableReference | limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := AssignmentNode new
							variable: limit
							value: arguments first].
	test := MessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: {limit}
				precedence: precedence
				from: encoder
				sourceRange: (myRange first to: blockRange first-1).
	incStmt := AssignmentNode new
				variable: blockVar
				value: (MessageNode new
							receiver: blockVar selector: #+
							arguments: {increment}
							precedence: precedence
							from: encoder
							sourceRange: (myRange last to: (myRange last max: blockRange last)))
				from: encoder
				sourceRange: (myRange last to: (myRange last max: blockRange last)).
	arguments := {limit. increment. block. initStmt. test. incStmt. limitInit}.
	block noteOptimizedIn: self.
	^true! !

