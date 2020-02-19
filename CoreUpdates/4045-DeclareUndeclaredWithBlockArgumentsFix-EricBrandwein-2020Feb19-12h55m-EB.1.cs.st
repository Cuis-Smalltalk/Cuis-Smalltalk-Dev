'From Cuis 5.0 [latest update: #4044] on 19 February 2020 at 12:57:24 pm'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'EB 1/27/2020 19:50:15'!
lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	self lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: [ :index | ^index ].
	^exceptionBlock value.! !


!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 02:36:44'!
createEmptyTempsDeclarationAfter: aDeclarationPosition
	"Return the position of the end of the declaration."
	| offset |
	
	offset := self insertWord: ' | |' at: aDeclarationPosition + 1.
	^aDeclarationPosition + offset! !

!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 03:01:19'!
createEmptyTempsDeclarationIfNecessaryIn: aBlockNode
	"Return the new tempsMark for this BlockNode"
	| blockTempsMark |
	
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	(self hasNoTempDeclarationPipes: aBlockNode) ifTrue: [ 
		blockTempsMark := self createEmptyTempsDeclarationAfter: blockTempsMark ].
	^blockTempsMark! !

!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 02:39:26'!
declareUndeclaredTemps: undeclaredTempNodes inBlock: aDeclaringBlockNode

	| blockTempsMark |
	
	blockTempsMark := self createEmptyTempsDeclarationIfNecessaryIn: aDeclaringBlockNode.
	undeclaredTempNodes do: [ :varName | blockTempsMark := self pasteTemp: varName before: blockTempsMark ]! !

!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 20:18:18'!
hasNoTempDeclarationPipes: aBlockNode
	"Determine if a BlockNode already has the '| |' used to declare temps."
	| blockTempsMark sourceCode hasNoTemps |
	
	sourceCode := requestor text string.
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	hasNoTemps := aBlockNode temporaries isEmpty.
	^hasNoTemps and: [ (self isLastPipeOfEmptyTempsDeclaration: blockTempsMark) not ].! !

!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 20:19:48'!
isLastPipeOfEmptyTempsDeclaration: pipeIndex

	| indexOfPreviousPipe sourceCode |
	
	sourceCode := requestor text string.
	indexOfPreviousPipe := sourceCode lastIndexOf: $| startingAt: pipeIndex - 1 endingAt: 1 ifAbsent: [ ^false ].
	^(sourceCode at: pipeIndex) = $| and: [
		(sourceCode copyFrom: indexOfPreviousPipe + 1 to: pipeIndex - 1) allSatisfy: [ :char | char isSeparator ]]! !


!Parser methodsFor: 'error correction' stamp: 'EB 1/27/2020 02:16:43'!
declareUndeclaredTemps: methodNode
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."
	| undeclared userSelection blocksToVars |
	(undeclared _ encoder undeclaredTemps) isEmpty ifTrue: [ ^ self ].
	userSelection _ requestor selectionInterval.
	blocksToVars _ IdentityDictionary new.
	undeclared do: [ :var |
		(blocksToVars
			at: (var tag == #method
				ifTrue: [ methodNode block ]
				ifFalse: [ methodNode accept: (VariableScopeFinder new ofVariable: var) ])
			ifAbsentPut: [ SortedCollection new ]) add: var name ].
	(blocksToVars removeKey: methodNode block ifAbsent: nil) ifNotNil: [ :rootVars |
		rootVars do: [ :varName |
			self pasteTempAtMethodLevel: varName ]].
	(blocksToVars keys sort: [ :a :b |
		a tempsMark < b tempsMark ]) do: [ :block | | blockUndeclaredVars |
		blockUndeclaredVars := blocksToVars at: block.
		self declareUndeclaredTemps: blockUndeclaredVars inBlock: block ].
	requestor
		selectInvisiblyFrom: userSelection first
		to: userSelection last + requestorOffset.
	ReparseAfterSourceEditing signal! !

!methodRemoval: Parser #pasteTemp:inBlock: stamp: 'EB 2/19/2020 12:56:32'!
Parser removeSelector: #pasteTemp:inBlock:!
!methodRemoval: Parser #createTempDeclarationInBlockWith:before: stamp: 'EB 2/19/2020 12:56:45'!
Parser removeSelector: #createTempDeclarationInBlockWith:before:!
