'From Cuis 6.0 [latest update: #5947] on 26 July 2023 at 9:14:29 am'!
!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category optimizeSpecialSends '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments keywordRanges optimizeSpecialSends '
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!Behavior methodsFor: 'methods' stamp: 'HAW 7/26/2023 08:51:25'!
methodNodeFor: aSourceCode noPattern: aBoolean optimizeSpecialSends: doOptimizeSpecialSends

	|  parser methodNode |

	parser := self parserClass new.
	parser optimizeSpecialSends: doOptimizeSpecialSends.

	methodNode := parser parse: aSourceCode class: self noPattern: aBoolean.
	methodNode sourceText: aSourceCode.

	^methodNode
			! !


!CompiledMethod methodsFor: 'ometa2preload' stamp: 'HAW 7/26/2023 09:08:27'!
createMethodNodeOptimizeSpecialSends: doOptimizeSpecialSends
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail."
	^ (doOptimizeSpecialSends
		ifTrue: [ 	(aClass methodNodeFor: source) ]
		ifFalse: [ 	(aClass methodNodeFor: source noPattern: false optimizeSpecialSends: false) ]) 
			ifNil: [ self decompile ].! !


!Parser methodsFor: 'public access' stamp: 'HAW 7/26/2023 08:30:39'!
optimizeSpecialSends: aBoolean

	optimizeSpecialSends := aBoolean! !


!MessageNode methodsFor: 'initialization' stamp: 'HAW 7/26/2023 08:56:53'!
optimizeSpecialSends: aBoolean
	optimizeSpecialSends := aBoolean! !


!Behavior methodsFor: 'methods' stamp: 'HAW 7/26/2023 08:35:12'!
methodNodeFor: aSourceCode noPattern: aBoolean

	^self methodNodeFor: aSourceCode noPattern: aBoolean optimizeSpecialSends: true! !


!CompiledMethod methodsFor: 'ometa2preload' stamp: 'HAW 7/26/2023 08:59:52'!
createMethodNode
	
	^self createMethodNodeOptimizeSpecialSends: true! !


!Parser methodsFor: 'expression types' stamp: 'HAW 7/26/2023 09:04:27'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: [
			start := self startOfNextToken.
			selector := source collectionSpecies writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [
			
			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
					optimizeSpecialSends: optimizeSpecialSends;
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true! !


!MessageNode methodsFor: 'private' stamp: 'HAW 7/26/2023 09:05:26'!
transform: encoder 
	special = 0 ifTrue: [^false].
	optimizeSpecialSends == false ifTrue: [ ^false ].

	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue: 
			[^true]
		ifFalse: 
			[special := 0. ^false]! !


!SourceCodeIntervalPrecondition methodsFor: 'initialization' stamp: 'HAW 7/26/2023 09:04:53'!
initializeFor: anIntervalToExtract of: aMethodUnderValidation

	intervalToExtract := anIntervalToExtract.
	method := aMethodUnderValidation.
	methodNode := method createMethodNodeOptimizeSpecialSends: false.
	sourceCode := method sourceCode.! !

!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category optimizeSpecialSends'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments keywordRanges optimizeSpecialSends'
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
