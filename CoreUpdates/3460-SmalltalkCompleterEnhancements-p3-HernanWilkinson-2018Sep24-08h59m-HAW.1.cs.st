'From Cuis 5.0 of 7 November 2016 [latest update: #3451] on 24 September 2018 at 3:31:12 pm'!

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:03:26'!
classOfInstVarNamed: aName inDebugger: aDebugger 
	
	^aDebugger contextStackIndex ~= 0 ifTrue: [ (aDebugger receiver instVarNamed: aName) class]! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:04:44'!
classOfInstVarNamed: aName inInspector: anInspector 
	
	^ (anInspector object instVarNamed: aName) class ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:17:20'!
classOfTempVarNamed: aName inContext: context

	| tempIndex |
	
	tempIndex := context tempNames indexOf: aName.
	
	^ tempIndex ~= 0 ifTrue: [(context tempAt: tempIndex) class]! !


!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/24/2018 13:25:49'!
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

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/24/2018 12:48:26'!
parse: sourceToParse in: contextClass and: specificModel
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: sourceToParse.
	parser parse: (specificModel is: #CodeProvider).
	
	^ parser last3Ranges.
! !

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/24/2018 12:58:46'!
selectedClassOrMetaClassIn: specificModel

	(specificModel is: #CodeProvider) ifTrue: [ ^ specificModel selectedClassOrMetaClass ].
	
	"I can not use #selectedClassOrMetaClass becuase it changes with the selection but when compiling to evaluate it assumes object as receiver - Hernan"
	^ (specificModel isKindOf: Inspector) ifTrue: [ specificModel object class ] ! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:02:09'!
classOfBlockArgNamed: aName in: specificModel

	^ (specificModel isKindOf: Debugger) ifTrue: [ self classOfBlockArgNamed: aName inDebugger: specificModel ].
	! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:03:54'!
classOfInstVarNamed: aName in: specificModel

	(specificModel isKindOf: Debugger) ifTrue: [ ^ self classOfInstVarNamed: aName inDebugger: specificModel ]. 
	^ (specificModel isKindOf: Inspector) ifTrue: [ self classOfInstVarNamed: aName inInspector: specificModel ].
	
	! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:16:59'!
classOfTempVarNamed: aName inDebugger: aDebugger
		
	| context |
							
	context := aDebugger selectedContext.
	
	^self classOfTempVarNamed: aName inContext: context! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:29:14'!
classOfWorkspaceVarNamed: id in: specificModel
	
	(specificModel isKindOf: Workspace) ifTrue: [ ^ self classOfWorkspaceVarNamed: id inWorkspace: specificModel ].
	^ (specificModel isKindOf: Inspector) ifTrue: [ self classOfWorkspaceVarNamed: id inInspector: specificModel ].
	! !

!SmalltalkCompleter methodsFor: 'class detection' stamp: 'HAW 9/24/2018 15:19:44'!
classOfWorkspaceVarNamed: aName inInspector: anInspector 

	^ self classOfTempVarNamed: aName inContext: anInspector object

! !

