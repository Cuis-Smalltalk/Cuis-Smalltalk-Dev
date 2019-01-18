'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 18 January 2019 at 12:58:36 am'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 00:58:19'!
typeOfCascadeReceiverAt: aRange 
	
	| positionBeforeSemiColon |
	
	Smalltalk isLiveTypingInstalled ifFalse: [ ^nil ].
	
	positionBeforeSemiColon := aRange end - 1.
	
	"I have to parse excluding the last semi-colon because if not a syntax error will be signaled 
	because the cascade message is not finished - Hernan"
	^self
		withMethodNodeOf: (parser source first: positionBeforeSemiColon) 
		do: [ :methodNode | self typeOfCascadeReceiverIn: methodNode at: positionBeforeSemiColon ]
		ifError: [ :anError | nil ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 00:14:55'!
withMethodNodeOf: aSourceCode do: doBlock ifError: anErrorBlock

	^ [[ doBlock value: (parser classOrMetaClass methodNodeFor: aSourceCode) ] 
		on: UndeclaredVariableReference 
		do: [  :anUndeclareVariableReference | anUndeclareVariableReference declareTempAndResume ]]
		on: Error
		do: anErrorBlock
		
	! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 00:21:47'!
withMethodNodeOfAllSourceCodeDo: doBlock ifError: anErrorBlock

	^ self withMethodNodeOf: parser source do: doBlock ifError: anErrorBlock 
	! !


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'HAW 1/17/2019 23:26:04'!
shouldOpenMorphWhenNoPrefixFor: currentChar 
	
	^currentChar isAlphaNumeric 
		or: [ currentChar isRightBracket
		or: [ currentChar = $) 
		or: [ currentChar = $; ]]]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/17/2019 16:49:38'!
canComputeMessageEntriesFor: prevRange and: prevPrevRange

	^ prevRange notNil and: [ prevPrevRange isNil or: [ prevPrevRange rangeType ~= #binary ]]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/17/2019 23:33:22'!
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
			[ #rightParenthesis ]  -> [ self returnTypeOfEnclosedExpressionAt: range ].
			[ #cascadeSeparator]	-> [ self typeOfCascadeReceiverAt: range ]. }
		otherwise: [ nil ].
	
	receiverClassOrEntries isCollection 
		ifTrue: [ self computeMessageEntriesForSelectors: receiverClassOrEntries ]
		ifFalse: [ 
"			Descomentar para que suceda solo para cuando el receiver es una clase"
			"(receiverClassOrEntries notNil and: [ receiverClassOrEntries isMeta ])"
			(receiverClassOrEntries notNil)
				ifTrue: [  self computeMessageForMetaclass: receiverClassOrEntries ] 
				ifFalse: [ self computeMessageEntries: receiverClassOrEntries ] ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 00:21:47'!
returnTypeOfEnclosedExpressionAt: aRange 

	Smalltalk isLiveTypingInstalled ifFalse: [ ^nil ].
	
	^self 
		withMethodNodeOfAllSourceCodeDo: [ :methodNode | self returnTypeOfEnclosedExpresionIn: methodNode at: aRange end - 1 ]
		ifError: [ :anError | nil ]! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 1/18/2019 00:45:13'!
returnTypeOfUnaryMessage: aSelector at: aRange 
	
	Smalltalk isLiveTypingInstalled ifFalse: [ ^nil ].
	
	^self
		withMethodNodeOfAllSourceCodeDo: [ :methodNode | self returnTypeOfNodeUnderCursorIn: methodNode at: aRange end]
		ifError: [ :anError | nil ]! !


!SmalltalkCompleter reorganize!
('entries' computeEntries newCursorPosition: selectedEntry)
('entries - private ' add:and:to: add:and:whenFullTo: add:and:whenNotFullTo: canComputeMessageEntriesFor:and: classOfLiteral:in: classOfLocalBindingNamed:in: computeEntriesOfMessageOrIdentifiersFor:using:at:in:and: computeIdentifierEntries computeMessageEntries: computeMessageEntriesFor:at:in:and: computeMessageEntriesForSelectors: computeMessageEntriesWhenSendinMessageFor:using:in:and: computeMessageEntriesWithEmptyPrefixFor:at:in:and: computeMessageForMetaclass: computeMessageForMetaclassAddingCategory: computeMessageForMetaclassWithCategories: insert:and:at:to: parse:in:and: returnTypeOfEnclosedExpressionAt: returnTypeOfUnaryMessage:at: selectedClassOrMetaClassIn: sortByLastUsedTime: textProviderOrModel typeOfCascadeReceiverAt: withMethodNodeOf:do:ifError: withMethodNodeOfAllSourceCodeDo:ifError:)
('testing' opensWithTab)
('*LiveTyping' commonSupertypeOrSelectorsOf:in: returnTypeOfEnclosedExpresionIn:at: returnTypeOfNodeUnderCursorIn:at: typeOfCascadeReceiverIn:at:)
!

