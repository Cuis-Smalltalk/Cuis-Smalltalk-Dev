'From Cuis 4.2 of 25 July 2013 [latest update: #2907] on 20 August 2016 at 7:21:22 pm'!

!InstructionStream methodsFor: 'scanning' stamp: 'jmv 8/20/2016 19:04:27'!
skipBackBeforeJump
	"Assuming that the receiver is positioned just after a jump, skip back one or two bytes,
	 depending on the size of the previous jump instruction."
	| scanner client prevPc |
	scanner := InstructionStream on: self method.
	client := ZInstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	scanner pc: prevPc.
	(scanner willJumpIfTrue or: [scanner willJumpIfFalse]) ifFalse:
		[self error: 'Where''s the jump??'].
	self jump: prevPc - pc! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/20/2016 18:59:29'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass ifNil: [
		PrimaryBytecodeSetEncoderClass := ZEncoderForV3PlusClosures].
	SecondaryBytecodeSetEncoderClass ifNil: [
		SecondaryBytecodeSetEncoderClass := ZEncoderForV3PlusClosures].! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/20/2016 19:00:36'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	(aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header >= 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/20/2016 19:00:30'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	(aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header < 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !


!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:55:42'!
clsBlockStartLocator
	^ZBlockStartLocator! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:56:45'!
clsClosureExtractor
	^ZClosureExtractor! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:57:20'!
clsCompiledMethodWithNode
	^ZCompiledMethodWithNode! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:58:12'!
clsCompiler
	^ZCompiler! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:58:31'!
clsDecompiler
	^ZDecompiler! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:00:02'!
clsEncoderForV3
	^ZEncoderForV3! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 18:59:44'!
clsEncoderForV3PlusClosures
	^ZEncoderForV3PlusClosures! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:02:37'!
clsInstVarRefLocator
	^ZInstVarRefLocator! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:03:36'!
clsInstructionClient
	^ZInstructionClient! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:03:17'!
clsInstructionPrinter
	^ZInstructionPrinter! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:19:46'!
clsParseNode
	^ZParseNode! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:06:17'!
clsParseNodeEnumerator
	^ZParseNodeEnumerator! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:07:59'!
clsParser
	^ZParser! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:02:59'!
clsRelativeInstructionPrinter
	^ZRelativeInstructionPrinter! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:08:19'!
clsScanner
	^ZScanner! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:09:35'!
clsSyntaxErrorNotification
	^ZSyntaxErrorNotification! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 8/20/2016 19:09:57'!
clsUndeclaredVariableWarning
	^ZUndeclaredVariableWarning! !

!methodRemoval: SystemDictionary #useOldCompiler!
SystemDictionary removeSelector: #useOldCompiler!
CompiledMethod initialize!
!classRemoval: #AmbiguousSelector!
Smalltalk removeClassNamed: #AmbiguousSelector!
!classRemoval: #AssignmentNode!
Smalltalk removeClassNamed: #AssignmentNode!
!classRemoval: #BlockArgsNode!
Smalltalk removeClassNamed: #BlockArgsNode!
!classRemoval: #BlockLocalTempCounter!
Smalltalk removeClassNamed: #BlockLocalTempCounter!
!classRemoval: #BlockNode!
Smalltalk removeClassNamed: #BlockNode!
!classRemoval: #BlockStartLocator!
Smalltalk removeClassNamed: #BlockStartLocator!
!classRemoval: #BraceNode!
Smalltalk removeClassNamed: #BraceNode!
!classRemoval: #BytecodeEncoder!
Smalltalk removeClassNamed: #BytecodeEncoder!
!classRemoval: #CascadeNode!
Smalltalk removeClassNamed: #CascadeNode!
!classRemoval: #ClosureExtractor!
Smalltalk removeClassNamed: #ClosureExtractor!
!classRemoval: #CompilationCue!
Smalltalk removeClassNamed: #CompilationCue!
!classRemoval: #CompiledMethodWithNode!
Smalltalk removeClassNamed: #CompiledMethodWithNode!
!classRemoval: #Compiler!
Smalltalk removeClassNamed: #Compiler!
!classRemoval: #Decompiler!
Smalltalk removeClassNamed: #Decompiler!
!classRemoval: #DecompilerConstructor!
Smalltalk removeClassNamed: #DecompilerConstructor!
!classRemoval: #DecompilerConstructorForClosures!
Smalltalk removeClassNamed: #DecompilerConstructorForClosures!
!classRemoval: #Encoder!
Smalltalk removeClassNamed: #Encoder!
!classRemoval: #EncoderForV3!
Smalltalk removeClassNamed: #EncoderForV3!
!classRemoval: #EncoderForV3PlusClosures!
Smalltalk removeClassNamed: #EncoderForV3PlusClosures!
!classRemoval: #InstVarRefLocator!
Smalltalk removeClassNamed: #InstVarRefLocator!
!classRemoval: #InstanceVariableNode!
Smalltalk removeClassNamed: #InstanceVariableNode!
!classRemoval: #InstructionClient!
Smalltalk removeClassNamed: #InstructionClient!
!classRemoval: #InstructionPrinter!
Smalltalk removeClassNamed: #InstructionPrinter!
!classRemoval: #KeyWordNode!
Smalltalk removeClassNamed: #KeyWordNode!
!classRemoval: #LeafNode!
Smalltalk removeClassNamed: #LeafNode!
!classRemoval: #LiteralNode!
Smalltalk removeClassNamed: #LiteralNode!
!classRemoval: #LiteralVariableNode!
Smalltalk removeClassNamed: #LiteralVariableNode!
!classRemoval: #MaybeContextInstanceVariableNode!
Smalltalk removeClassNamed: #MaybeContextInstanceVariableNode!
!classRemoval: #MessageAsTempNode!
Smalltalk removeClassNamed: #MessageAsTempNode!
!classRemoval: #MessageNode!
Smalltalk removeClassNamed: #MessageNode!
!classRemoval: #MethodNode!
Smalltalk removeClassNamed: #MethodNode!
!classRemoval: #NewArrayNode!
Smalltalk removeClassNamed: #NewArrayNode!
!classRemoval: #OptimizedBlockLocalTempReadBeforeWrittenVisitor!
Smalltalk removeClassNamed: #OptimizedBlockLocalTempReadBeforeWrittenVisitor!
!classRemoval: #ParseNode!
Smalltalk removeClassNamed: #ParseNode!
!classRemoval: #ParseNodeEnumerator!
Smalltalk removeClassNamed: #ParseNodeEnumerator!
!classRemoval: #ParseNodeVisitor!
Smalltalk removeClassNamed: #ParseNodeVisitor!
!classRemoval: #ParseStack!
Smalltalk removeClassNamed: #ParseStack!
!classRemoval: #Parser!
Smalltalk removeClassNamed: #Parser!
!classRemoval: #ParserNotification!
Smalltalk removeClassNamed: #ParserNotification!
!classRemoval: #RelativeInstructionPrinter!
Smalltalk removeClassNamed: #RelativeInstructionPrinter!
!classRemoval: #RemoteTempVectorNode!
Smalltalk removeClassNamed: #RemoteTempVectorNode!
!classRemoval: #ReparseAfterSourceEditing!
Smalltalk removeClassNamed: #ReparseAfterSourceEditing!
!classRemoval: #ReturnNode!
Smalltalk removeClassNamed: #ReturnNode!
!classRemoval: #Scanner!
Smalltalk removeClassNamed: #Scanner!
!classRemoval: #SelectorNode!
Smalltalk removeClassNamed: #SelectorNode!
!classRemoval: #SpecialSelectorNode!
Smalltalk removeClassNamed: #SpecialSelectorNode!
!classRemoval: #SyntaxErrorNotification!
Smalltalk removeClassNamed: #SyntaxErrorNotification!
!classRemoval: #TempVariableNode!
Smalltalk removeClassNamed: #TempVariableNode!
!classRemoval: #UndeclaredVariable!
Smalltalk removeClassNamed: #UndeclaredVariable!
!classRemoval: #UndeclaredVariableNode!
Smalltalk removeClassNamed: #UndeclaredVariableNode!
!classRemoval: #UndeclaredVariableReference!
Smalltalk removeClassNamed: #UndeclaredVariableReference!
!classRemoval: #UndeclaredVariableWarning!
Smalltalk removeClassNamed: #UndeclaredVariableWarning!
!classRemoval: #UndefinedVariable!
Smalltalk removeClassNamed: #UndefinedVariable!
!classRemoval: #UnknownSelector!
Smalltalk removeClassNamed: #UnknownSelector!
!classRemoval: #UnusedVariable!
Smalltalk removeClassNamed: #UnusedVariable!
!classRemoval: #VariableNode!
Smalltalk removeClassNamed: #VariableNode!
!classRemoval: #VariableScopeFinder!
Smalltalk removeClassNamed: #VariableScopeFinder!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeKey: #UseZCompiler.!

