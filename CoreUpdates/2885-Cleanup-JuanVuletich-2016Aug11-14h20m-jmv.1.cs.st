'From Cuis 4.2 of 25 July 2013 [latest update: #2877] on 11 August 2016 at 2:41:21 pm'!

!PseudoClass methodsFor: 'testing' stamp: 'jmv 7/29/2016 10:49:39'!
lastUnderscoreMeansSubscript
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false! !


!Compiler methodsFor: 'public access' stamp: 'jmv 8/4/2016 15:11:52'!
format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"

	| aNode |
	self from: textOrStream
		class: aClass
		notifying: aRequestor.
	aNode := self formatNoPattern: false ifFail: [^ nil].
	^aNode decompileString! !


!Encoder methodsFor: 'initialize-release' stamp: 'jmv 8/4/2016 15:24:29'!
init: aCue notifying: anObject
	"The use of the variable requestor is a bit confusing here. This is
	*not* the original requestor, which is available through the cue.
	It's the Parser instance that is using the encoder."

	self setCue: aCue.
	requestor := anObject.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	cue getClass variablesAndOffsetsDo:
		[:variable "<String>" :offset "<Integer>" |
					scopeTable
						at: variable
						put: (offset >= 0
								ifTrue: [InstanceVariableNode new
											name: variable index: offset]
								ifFalse: [MaybeContextInstanceVariableNode new
											name: variable index: offset negated])].
	cue context ~~ nil ifTrue:
		[| homeNode |
		 homeNode := self bindTemp: self doItInContextName.
		 "0th temp = aContext passed as arg"
		 cue context tempNames withIndexDo:
			[:variable :index|
			variable ~= self doItInContextName ifTrue:
				[scopeTable
					at: variable
					put: (MessageAsTempNode new
							receiver: homeNode
							selector: #namedTempAt:
							arguments: (Array with: (self encodeLiteral: index))
							precedence: 3
							from: self)]]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32
! !


!BlockNode methodsFor: 'printing' stamp: 'jmv 8/4/2016 15:08:50'!
decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^ String streamContents: [:strm | self printOn: strm indent: 0]
! !


!MessageNode methodsFor: 'initialize-release' stamp: 'jmv 8/4/2016 15:20:22'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !


!ZMessageNode methodsFor: 'initialization' stamp: 'jmv 8/11/2016 14:28:44'!
receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder! !

!methodRemoval: SHTextStyler #formattedAndStyledText!
SHTextStyler removeSelector: #formattedAndStyledText!
!methodRemoval: SHTextStyler #styledTextFor:!
SHTextStyler removeSelector: #styledTextFor:!
!methodRemoval: ZVariableScopeFinder #visitFutureNode:!
ZVariableScopeFinder removeSelector: #visitFutureNode:!
!methodRemoval: ZParseNodeEnumerator #visitFutureNode:!
ZParseNodeEnumerator removeSelector: #visitFutureNode:!
!methodRemoval: ZParseNodeVisitor #visitFutureNode:!
ZParseNodeVisitor removeSelector: #visitFutureNode:!
!methodRemoval: VariableScopeFinder #visitCommentNode:!
VariableScopeFinder removeSelector: #visitCommentNode:!
!methodRemoval: VariableScopeFinder #visitFutureNode:!
VariableScopeFinder removeSelector: #visitFutureNode:!
!methodRemoval: ParseNodeEnumerator #visitCommentNode:!
ParseNodeEnumerator removeSelector: #visitCommentNode:!
!methodRemoval: ParseNodeEnumerator #visitFutureNode:!
ParseNodeEnumerator removeSelector: #visitFutureNode:!
!methodRemoval: ParseNodeVisitor #visitCommentNode:!
ParseNodeVisitor removeSelector: #visitCommentNode:!
!methodRemoval: ParseNodeVisitor #visitFutureNode:!
ParseNodeVisitor removeSelector: #visitFutureNode:!
!methodRemoval: ZMethodNode #asColorizedSmalltalk80Text!
ZMethodNode removeSelector: #asColorizedSmalltalk80Text!
!methodRemoval: ZMethodNode #decompileText!
ZMethodNode removeSelector: #decompileText!
!methodRemoval: ZSelectorNode #emitForEffect:on:!
ZSelectorNode removeSelector: #emitForEffect:on:!
!methodRemoval: ZSelectorNode #sizeForEffect:!
ZSelectorNode removeSelector: #sizeForEffect:!
!methodRemoval: ZParseNode #isFutureNode!
ZParseNode removeSelector: #isFutureNode!
!methodRemoval: MethodNode #asColorizedSmalltalk80Text!
MethodNode removeSelector: #asColorizedSmalltalk80Text!
!methodRemoval: MethodNode #decompileText!
MethodNode removeSelector: #decompileText!
!methodRemoval: MessageAsTempNode #emitStorePop:on:!
MessageAsTempNode removeSelector: #emitStorePop:on:!
!methodRemoval: MessageAsTempNode #sizeForStorePop:!
MessageAsTempNode removeSelector: #sizeForStorePop:!
!methodRemoval: SelectorNode #emitForEffect:on:!
SelectorNode removeSelector: #emitForEffect:on:!
!methodRemoval: SelectorNode #sizeForEffect:!
SelectorNode removeSelector: #sizeForEffect:!
!methodRemoval: BlockNode #decompileText!
BlockNode removeSelector: #decompileText!
!methodRemoval: ParseNode #addCommentToMorph:!
ParseNode removeSelector: #addCommentToMorph:!
!methodRemoval: ParseNode #isFutureNode!
ParseNode removeSelector: #isFutureNode!
!methodRemoval: ZCompiler class #couldEvaluate:!
ZCompiler class removeSelector: #couldEvaluate:!
!methodRemoval: ZCompiler class #format:in:notifying:decorated:!
ZCompiler class removeSelector: #format:in:notifying:decorated:!
!methodRemoval: ZCompiler #compiledMethodFor:in:to:notifying:ifFail:logged:!
ZCompiler removeSelector: #compiledMethodFor:in:to:notifying:ifFail:logged:!
!methodRemoval: ZCompiler #format:in:notifying:decorated:!
ZCompiler removeSelector: #format:in:notifying:decorated:!
!methodRemoval: ZCompiler #parse:in:notifying:!
ZCompiler removeSelector: #parse:in:notifying:!
!methodRemoval: ZCompiler #parserClass:!
ZCompiler removeSelector: #parserClass:!
!methodRemoval: ZCompiler #translate:noPattern:ifFail:parser:!
ZCompiler removeSelector: #translate:noPattern:ifFail:parser:!
!methodRemoval: Compiler class #couldEvaluate:!
Compiler class removeSelector: #couldEvaluate:!
!methodRemoval: Compiler class #evaluate:environment:!
Compiler class removeSelector: #evaluate:environment:!
!methodRemoval: Compiler class #evaluate:environment:logged:!
Compiler class removeSelector: #evaluate:environment:logged:!
!methodRemoval: Compiler class #evaluate:notifying:!
Compiler class removeSelector: #evaluate:notifying:!
!methodRemoval: Compiler class #format:in:notifying:decorated:!
Compiler class removeSelector: #format:in:notifying:decorated:!
!methodRemoval: Compiler #compile:ifFail:!
Compiler removeSelector: #compile:ifFail:!
!methodRemoval: Compiler #compiledMethodFor:in:to:notifying:ifFail:!
Compiler removeSelector: #compiledMethodFor:in:to:notifying:ifFail:!
!methodRemoval: Compiler #compiledMethodFor:in:to:notifying:ifFail:logged:!
Compiler removeSelector: #compiledMethodFor:in:to:notifying:ifFail:logged:!
!methodRemoval: Compiler #evaluate:notifying:!
Compiler removeSelector: #evaluate:notifying:!
!methodRemoval: Compiler #format:in:notifying:decorated:!
Compiler removeSelector: #format:in:notifying:decorated:!
!methodRemoval: Compiler #parse:in:notifying:!
Compiler removeSelector: #parse:in:notifying:!
!methodRemoval: Compiler #parserClass:!
Compiler removeSelector: #parserClass:!
!methodRemoval: Compiler #translate:noPattern:ifFail:parser:!
Compiler removeSelector: #translate:noPattern:ifFail:parser:!
!methodRemoval: ZDecompiler #methodRefersOnlyOnceToTemp:!
ZDecompiler removeSelector: #methodRefersOnlyOnceToTemp:!
!methodRemoval: Decompiler #methodRefersOnlyOnceToTemp:!
Decompiler removeSelector: #methodRefersOnlyOnceToTemp:!

!LeafNode reorganize!
('accessing' key)
('code generation' code emitCodeForEffect:encoder: emitCodeForLoad:forValue:encoder: reserve: sizeCodeForEffect: sizeCodeForLoad:forValue: sizeCodeForValue:)
('initialize-release' key:code: key:index:type: name:key:code:)
('private' code:type:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
!

!classRemoval: #ColoredCodeStream!
Smalltalk removeClassNamed: #ColoredCodeStream!
!classRemoval: #CommentNode!
Smalltalk removeClassNamed: #CommentNode!
!classRemoval: #FieldNode!
Smalltalk removeClassNamed: #FieldNode!
!classRemoval: #FutureNode!
Smalltalk removeClassNamed: #FutureNode!
!classRemoval: #MethodTempsNode!
Smalltalk removeClassNamed: #MethodTempsNode!
!classRemoval: #SpecialLiteralNode!
Smalltalk removeClassNamed: #SpecialLiteralNode!
