'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 20 December 2018 at 9:43:37 am'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 09:41:03'!
computeMessageEntriesForSelectors: selectors

	selectors isEmpty
		ifTrue: [ self computeMessageEntries: nil ]
		ifFalse: [ | prefixedSelectors |
			prefixedSelectors := selectors select: [ :aSelector | aSelector beginsWith: prefix ].
			entries _ prefixedSelectors asArray sort ]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 12/20/2018 09:38:26'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClassOrEntries |

	id _ allSource copyFrom: range start to: range end.
	receiverClassOrEntries _ range rangeType 
		caseOf: {
			[ #globalVar ] 		-> [ (Smalltalk at: id asSymbol) class ].
			[ #self ] 				-> [ contextClass ].
			[ #super ] 				-> [ contextClass superclass ].
			[ #true ] 				-> [ True ].
			[ #false ] 				-> [ False ].
			[ #nil ] 				-> [ UndefinedObject ].
			[ #character ] 		-> [ id first class ].
			[ #number ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #string ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #symbol ] 			-> [ self classOfLiteral: id in: contextClass ].
			[ #stringSymbol ]  	-> [ self classOfLiteral: id in: contextClass ].
			[ #instVar ] 			-> [ specificModel classOfInstVarNamed: id ].
			[ #methodArg ] 		-> [ specificModel classOfTempVarNamed: id ].
			[ #tempVar ] 			-> [ specificModel classOfTempVarNamed: id ].
			[ #workspaceVar ]		-> [ specificModel classOfWorkspaceVarNamed: id ].
			[ #blockArg ]			-> [ specificModel classOfBlockArgNamed: id ].
			[ #blockTempVar ]		-> [ specificModel classOfBlockTempVarNamed: id ].
			[ #thisContext ] 		-> [ specificModel classOfThisContext ]. 
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #blockEnd ]			-> [ BlockClosure ].
			[ #unary ]				-> [ self returnTypeOfUnaryMessage: id at: range ].
			[ #rightParenthesis ]  -> [ self returnTypeOfEnclosedExpressionAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ self computeMessageEntriesForSelectors: receiverClassOrEntries ]
		ifFalse: [ self computeMessageEntries: receiverClassOrEntries ]! !

