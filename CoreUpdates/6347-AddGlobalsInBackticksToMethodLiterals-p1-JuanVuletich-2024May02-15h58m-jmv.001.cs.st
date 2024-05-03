'From Cuis6.3 [latest update: #6340] on 2 May 2024 at 4:01:46 pm'!
!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue completeSourceRanges bindingsInBacktickLiterals '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Encoder methodsFor: 'encoding' stamp: 'jmv 5/2/2024 15:45:56'!
noteBactickLiteralBinding: aVariableBinding
	"Register a aVariableBinding.
	These will later be registered into the literals so that tools can easily browse references."
	bindingsInBacktickLiterals add: aVariableBinding! !


!BacktickNode methodsFor: 'initialization' stamp: 'jmv 5/2/2024 15:29:45'!
addLiteralsOfExpressionToParser

	expression nodesDo: [ :aNode |
		aNode isMessageNode ifTrue: [
			parser addToSentInLiterals: aNode selector key ].
		aNode isLiteralNode ifTrue: [
			(aNode literalValue isSymbol and: [ aNode literalValue ~= Scanner doItSelector ])
				ifTrue: [ parser addToSentInLiterals: aNode literalValue ]].
			 (aNode isLiteralVariableNode and: [ 
				aNode isReferencingBehavior or: [ aNode isReferencingGlobal ]])
					ifTrue: [ parser addToSentInLiterals: aNode variableBinding ]].! !


!LiteralVariableNode methodsFor: 'testing' stamp: 'jmv 5/2/2024 15:28:36'!
variableBinding
	^key! !


!Parser methodsFor: 'public access' stamp: 'jmv 5/2/2024 15:46:23'!
parse: sourceStream class: class category: aCategory noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."

	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category := aCategory.
	myStream := sourceStream.
	[
		repeatNeeded := false.
		startPosition := myStream position.
		sourceCode := myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		noPattern ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag := noPattern.
		failBlock := aBlock.
		[ methNode := self method: noPattern context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded := true.
				myStream := requestor text string readStream ].
		repeatNeeded
	] whileTrue: [ encoder := self encoder class new ].

	methNode sourceText: sourceCode.

	"See BackTickNode >> #addLiteralsOfExpressionToParser"
	sentInLiterals do: [ :each |
		each isSymbol ifTrue: [encoder noteOptimizedSelector: each].
		each isVariableBinding ifTrue: [encoder noteBactickLiteralBinding: each]].

	^ methNode.! !

!Parser methodsFor: 'backtick' stamp: 'jmv 5/2/2024 15:23:27'!
addToSentInLiterals: aSymbolOrBinding

	sentInLiterals add: aSymbolOrBinding ! !


!Encoder methodsFor: 'initialization' stamp: 'jmv 5/2/2024 15:44:12'!
initScopeAndLiteralTables

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new.
	bindingsInBacktickLiterals := Set new.! !


!BacktickNode methodsFor: 'initialization' stamp: 'jmv 5/2/2024 15:22:22'!
initializeExpression: anExpression source: aSourceCode parser: aParser range: aRange

	expression := anExpression.
	sourceCode := aSourceCode.
	parser := aParser.
	range := aRange.

	self addLiteralsOfExpressionToParser.! !

!classDefinition: #Encoder category: #'Compiler-Kernel'!
ParseNode subclass: #Encoder
	instanceVariableNames: 'scopeTable nTemps supered requestor class selector literalStream selectorSet litIndSet litSet sourceRanges globalSourceRanges addedSelectorAndMethodClassLiterals optimizedSelectors cue completeSourceRanges bindingsInBacktickLiterals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

