'From Cuis 4.2 of 25 July 2013 [latest update: #2772] on 25 May 2016 at 3:37:37.214287 pm'!

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:17'!
clsEncoderForV3
	^self useOldCompiler
		ifTrue: [ ZEncoderForV3 ]
		ifFalse: [ EncoderForV3 ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:17'!
clsEncoderForV3PlusClosures
	^self useOldCompiler
		ifTrue: [ ZEncoderForV3PlusClosures ]
		ifFalse: [ EncoderForV3PlusClosures ]! !


!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsBlockStartLocator
	^self useOldCompiler
		ifTrue: [ ZBlockStartLocator ]
		ifFalse: [ BlockStartLocator ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsClosureExtractor
	^self useOldCompiler
		ifTrue: [ ZClosureExtractor ]
		ifFalse: [ ClosureExtractor ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsCompiledMethodWithNode
	^self useOldCompiler
		ifTrue: [ ZCompiledMethodWithNode ]
		ifFalse: [ CompiledMethodWithNode ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsCompiler
	^self useOldCompiler
		ifTrue: [ ZCompiler ]
		ifFalse: [ Compiler ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsDecompiler
	^self useOldCompiler
		ifTrue: [ ZDecompiler ]
		ifFalse: [ Decompiler ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsInstVarRefLocator
	^self useOldCompiler
		ifTrue: [ ZInstVarRefLocator ]
		ifFalse: [ InstVarRefLocator ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsInstructionClient
	^self useOldCompiler
		ifTrue: [ ZInstructionClient ]
		ifFalse: [ InstructionClient ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:09'!
clsInstructionPrinter
	^self useOldCompiler
		ifTrue: [ ZInstructionPrinter ]
		ifFalse: [ InstructionPrinter ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsParseNode
	^self useOldCompiler
		ifTrue: [ ZParseNode ]
		ifFalse: [ ParseNode ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsParseNodeEnumerator
	^self useOldCompiler
		ifTrue: [ ZParseNodeEnumerator ]
		ifFalse: [ ParseNodeEnumerator ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsParser
	^self useOldCompiler
		ifTrue: [ ZParser ]
		ifFalse: [ Parser ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsRelativeInstructionPrinter
	^self useOldCompiler
		ifTrue: [ ZRelativeInstructionPrinter ]
		ifFalse: [ RelativeInstructionPrinter ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsScanner
	^self useOldCompiler
		ifTrue: [ ZScanner ]
		ifFalse: [ Scanner ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsSyntaxErrorNotification
	^self useOldCompiler
		ifTrue: [ ZSyntaxErrorNotification ]
		ifFalse: [ SyntaxErrorNotification ]! !

!SystemDictionary methodsFor: 'full compiler swap' stamp: 'jmv 5/24/2016 23:10'!
clsUndeclaredVariableWarning
	^self useOldCompiler
		ifTrue: [ ZUndeclaredVariableWarning ]
		ifFalse: [ UndeclaredVariableWarning ]! !

