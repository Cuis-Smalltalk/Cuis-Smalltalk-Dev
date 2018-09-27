'From Cuis 5.0 of 7 November 2016 [latest update: #3451] on 23 September 2018 at 10:28:44 pm'!

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 22:24:12'!
classOfBlockArgNamed: aName in: specificModel

	(specificModel isKindOf: Debugger) ifTrue: [ ^ self classOfBlockArgNamed: aName inDebugger: specificModel ].
	
	^ nil! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/23/2018 22:27:23'!
classOfBlockArgNamed: aName inDebugger: aDebugger 
	
	^ aDebugger contextStackIndex ~= 0 ifTrue: [ | selectedContext tempIndex |
		selectedContext := aDebugger selectedContext.
		tempIndex := selectedContext tempNames indexOf: aName ifAbsent: [ ^nil ].
		(selectedContext namedTempAt: tempIndex) class ]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/23/2018 22:23:41'!
computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel
	
	| id receiverClass |
	
	id _ allSource copyFrom: range start to: range end.
	receiverClass _ range rangeType 
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
			[ #instVar ] 			-> [ self classOfInstVarNamed: id in: specificModel ].
			[ #tempVar ] 			-> [ self classOfTempVarNamed: id in: specificModel ].
			[ #workspaceVar ]		-> [ self classOfWorkspaceVarNamed: id in: specificModel ].
			[ #classVar ] 			-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #poolConstant ] 	-> [ self classOfLocalBindingNamed: id in: contextClass ].
			[ #thisContext ] 		-> [ self classOfThisContextIn: specificModel ]. 
			[ #blockArg ]			-> [ self classOfBlockArgNamed: id in: specificModel ].} 
		otherwise: [ nil ].
		
	self computeMessageEntries: receiverClass! !

