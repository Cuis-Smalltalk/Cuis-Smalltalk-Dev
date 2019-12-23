'From Cuis 5.0 [latest update: #3985] on 23 December 2019 at 7:21:35 am'!
!classDefinition: #CodeNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #CodeNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #BlockNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #BlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode temporaries optimized optimizedMessageNode actualScopeIfOptimized blockExtent remoteTempNode copiedValues closureCreationNode startOfLastStatement tempsMark '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!CodeNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 07:13:13'!
arguments

	self subclassResponsibility ! !

!CodeNode methodsFor: 'accessing' stamp: 'HAW 12/23/2019 07:13:20'!
temporaries

	self subclassResponsibility ! !

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:14:12'!
hasLocallyArgumentOrTemporaryNamed: aVariableName

	"See #hasArgumentOrTemporaryNamed: comment - Hernan"
	^(self hasNodeIn: self temporaries named: aVariableName)
		or: [self hasNodeIn: self arguments named: aVariableName ]! !

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:16:18'!
hasNodeIn: aCollectionOfNodes named: aName

	^aCollectionOfNodes anySatisfy: [ :tempNode | tempNode name = aName ]! !

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 07:15:28'!
isLocalArgumentOrTemporary: aParseNode

	"Looks only in this scope - Hernan"
	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	! !

!methodRemoval: CodeNode #hasArgumentOrTemporaryNamed: stamp: 'HAW 12/23/2019 07:20:12'!
CodeNode removeSelector: #hasArgumentOrTemporaryNamed:!

!CodeNode reorganize!
('accessing' arguments temporaries)
('testing' hasLocallyArgumentOrTemporaryNamed: hasNodeIn:named: isLocalArgumentOrTemporary:)
!

!classDefinition: #BlockNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #BlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode temporaries optimized optimizedMessageNode actualScopeIfOptimized blockExtent remoteTempNode copiedValues closureCreationNode startOfLastStatement tempsMark'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
