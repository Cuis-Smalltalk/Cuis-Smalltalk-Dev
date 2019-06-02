'From Cuis 5.0 of 7 November 2016 [latest update: #3777] on 29 May 2019 at 9:14:30 am'!
!classDefinition: #TemporaryVariableDeclarationCounter category: #'Tools-Refactoring'!
ParseNodesDeclaringTemporaryVariableVisitor subclass: #TemporaryVariableDeclarationCounter
	instanceVariableNames: 'count '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #TemporaryVariableDeclarationRemover category: #'Tools-Refactoring'!
ParseNodesDeclaringTemporaryVariableVisitor subclass: #TemporaryVariableDeclarationRemover
	instanceVariableNames: 'methodNode newSourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TemporaryVariableDeclarationCounter methodsFor: 'initialization' stamp: 'EB 5/28/2019 21:00:58'!
initialize

	count _ 0.! !

!TemporaryVariableDeclarationCounter methodsFor: 'visiting' stamp: 'EB 5/28/2019 20:59:44'!
visitBlockNodeDeclaringTemporary: aBlockNode

	count _ count + 1.! !

!TemporaryVariableDeclarationCounter methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:00:06'!
visitMethodNodeDeclaringTemporary: aMethodNode

	count _ count + 1.! !


!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:24:38'!
sourceTextWithoutMethodTemporaryDeclarationLine
	
	| endTempsMark startTempsMark |
	
	startTempsMark _ methodNode sourceText indexOf: $|.
	endTempsMark _ methodNode sourceText indexOf: $| startingAt: startTempsMark + 1.
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark.! !

!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:22:07'!
sourceTextWithoutTemporaryDeclaration

	| temporaryVariablePositions variableDeclarationPosition | 

	temporaryVariablePositions _ methodNode positionsForTemporaryVariable: temporaryVariable ifAbsent: [].
	variableDeclarationPosition _ {temporaryVariablePositions first}.
	^methodNode sourceText copyReplacing: variableDeclarationPosition with: ''! !

!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:41:15'!
sourceTextWithoutTemporaryDeclarationLineFrom: firstIndex to: lastIndex

	^methodNode sourceText copyReplaceFrom: firstIndex to: lastIndex with: ' '.! !

!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:24:38'!
sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode
	
	| sourceTextUpToEndTemps endTempsMark startTempsMark |
	
	endTempsMark _ aBlockNode tempsMark.
	sourceTextUpToEndTemps _ methodNode sourceText copyFrom: 1 to: endTempsMark - 1.
	startTempsMark _ sourceTextUpToEndTemps findLastOccurrenceOfString: '|' startingAt: 1.
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark.
			
			! !

!TemporaryVariableDeclarationRemover methodsFor: 'source text generation' stamp: 'EB 5/28/2019 21:27:58'!
sourceTextWithoutTemporaryFromParseNode: aParseNode
withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock
	
	^aParseNode temporaries size = 1 
		ifTrue: aTemporaryDeclarationLineRemovingBlock value
		ifFalse: [ self sourceTextWithoutTemporaryDeclaration ].! !

!TemporaryVariableDeclarationRemover methodsFor: 'accessing' stamp: 'EB 5/28/2019 21:12:33'!
methodNode: aMethodNode

	methodNode _ aMethodNode.! !

!TemporaryVariableDeclarationRemover methodsFor: 'accessing' stamp: 'EB 5/28/2019 21:18:31'!
newSourceCode

	^newSourceCode ! !

!TemporaryVariableDeclarationRemover methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:17:14'!
visitBlockNodeDeclaringTemporary: aBlockNode

	self 
		visitNodeDeclaringTemporary: aBlockNode 
		withTemporaryDeclarationLineRemover: [ 
			self sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode ]! !

!TemporaryVariableDeclarationRemover methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:17:46'!
visitMethodNodeDeclaringTemporary: aMethodNode
	
	self 
		visitNodeDeclaringTemporary: aMethodNode 
		withTemporaryDeclarationLineRemover: [ self sourceTextWithoutMethodTemporaryDeclarationLine ]! !

!TemporaryVariableDeclarationRemover methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:27:34'!
visitNodeDeclaringTemporary: aParseNode 
withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

	newSourceCode _ self
		sourceTextWithoutTemporaryFromParseNode: aParseNode
		withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock.
		
	methodNode methodClass compile: newSourceCode.! !


!TemporaryVariableDeclarationRemover class methodsFor: 'instance creation' stamp: 'EB 5/28/2019 21:11:31'!
in: aMethodNode for: aTemporaryVariable

	| instance |

	instance _ self for: aTemporaryVariable.
	instance methodNode: aMethodNode.
	^instance! !


!BlockNode methodsFor: 'accessing' stamp: 'EB 5/28/2019 19:43:53'!
temporaries
	"Collection of TempVariableNodes"
	^temporaries ifNil: [#()]! !

!BlockNode methodsFor: 'accessing' stamp: 'EB 5/28/2019 19:43:59'!
temporaries: aCollection
	"Collection of TempVariableNodes"
	temporaries := aCollection! !

!BlockNode methodsFor: 'accessing' stamp: 'EB 5/28/2019 19:52:48'!
tempsMark
	"Index of the end of the temporaries declarations in the containing MethodNode sourceText"
	^tempsMark! !

!BlockNode methodsFor: 'accessing' stamp: 'EB 5/28/2019 19:53:04'!
tempsMark: anInteger
	"Index of the end of the temporaries declarations in the containing MethodNode sourceText"
	tempsMark := anInteger! !


!TemporaryToInstanceVariable methodsFor: 'applying' stamp: 'EB 5/28/2019 21:29:47'!
removeTemporary

	| remover |
	
	remover _ TemporaryVariableDeclarationRemover in: method for: variable. 
	method accept: remover.
	^remover newSourceCode. 
	! !

!methodRemoval: TemporaryToInstanceVariable #sourceCodeWithoutAnyTemporaryDeclarations!
TemporaryToInstanceVariable removeSelector: #sourceCodeWithoutAnyTemporaryDeclarations!
!methodRemoval: TemporaryToInstanceVariable #sourceCodeWithoutTemporaryDeclaration!
TemporaryToInstanceVariable removeSelector: #sourceCodeWithoutTemporaryDeclaration!
!methodRemoval: TemporaryVariableDeclarationCounter class #for:!
TemporaryVariableDeclarationCounter class removeSelector: #for:!
!methodRemoval: TemporaryVariableDeclarationCounter #initializeFor:!
TemporaryVariableDeclarationCounter removeSelector: #initializeFor:!
!methodRemoval: TemporaryVariableDeclarationCounter #visitBlockNode:!
TemporaryVariableDeclarationCounter removeSelector: #visitBlockNode:!
!classDefinition: #TemporaryVariableDeclarationCounter category: #'Tools-Refactoring'!
ParseNodesDeclaringTemporaryVariableVisitor subclass: #TemporaryVariableDeclarationCounter
	instanceVariableNames: 'count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TemporaryVariableDeclarationCounter class reorganize!
('as yet unclassified')
!

