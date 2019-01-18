'From Cuis 5.0 of 7 November 2016 [latest update: #3583] on 18 January 2019 at 12:32:51 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 12:32:36'!
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
			[ #arrayEnd ]			-> [ Array ].
			[ #rightBrace ]		-> [ Array ].
			[ #unary ]				-> [ self returnTypeOfUnaryMessage: id at: range ].
			[ #rightParenthesis ] -> [ self returnTypeOfEnclosedExpressionAt: range ].
			[ #cascadeSeparator]	-> [ self typeOfCascadeReceiverAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ self computeMessageEntriesForSelectors: receiverClassOrEntries ]
		ifFalse: [ self computeMessageEntries: receiverClassOrEntries ]! !

!methodRemoval: SmalltalkCompleter #computeMessageForMetaclass:!
SmalltalkCompleter removeSelector: #computeMessageForMetaclass:!
!methodRemoval: SmalltalkCompleter #computeMessageForMetaclassAddingCategory:!
SmalltalkCompleter removeSelector: #computeMessageForMetaclassAddingCategory:!
!methodRemoval: SmalltalkCompleter #computeMessageForMetaclassWithCategories:!
SmalltalkCompleter removeSelector: #computeMessageForMetaclassWithCategories:!
!methodRemoval: SmalltalkCompleter #withParserSourceMethodNodeDo:ifError:!
SmalltalkCompleter removeSelector: #withParserSourceMethodNodeDo:ifError:!
