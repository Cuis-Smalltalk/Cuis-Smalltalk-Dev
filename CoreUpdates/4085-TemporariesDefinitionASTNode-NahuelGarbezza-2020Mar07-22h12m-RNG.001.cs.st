'From Cuis 5.0 [latest update: #4058] on 7 March 2020 at 10:34:03 pm'!
"Change Set:		4059-CuisCore-AuthorName-2020Mar07-22h12m
Date:			7 March 2020
Author:			Nahuel Garbezza

Adds two new parse nodes representing a temporaries assignment and each individual assignment, add those to each MethodNode and BlockNode (coexisting with the current approach -list of temporaries-)"!

!classDefinition: #BlockNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #BlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode temporaries optimized optimizedMessageNode actualScopeIfOptimized blockExtent remoteTempNode copiedValues closureCreationNode startOfLastStatement tempsMark temporariesDeclaration '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!BlockNode commentStamp: '<historical>' prior: 0!
I represent a bracketed block with 0 or more arguments and 1 or more statements. If I am initialized with no statements, I create one. I have a flag to tell whether my last statement returns a value from the enclosing method. My last three fields remember data needed for code generation. I can emit for value in the usual way, in which case I create a literal method (actually a context remotely copied) to be evaluated by sending it value: at run time. Or I can emit code to be evaluated in line; this only happens at the top level of a method and in conditionals and while-loops, none of which have arguments.

We are in the process of refactoring "temporaries" inst var to a "temporariesDeclaration" inst var which is a parse node that contains more than just the temporaries' nodes, it is the node that represents the declaration itself. Refer to the class comment in MethodNode for more information on how to migrate to "temporariesDeclaration".!

!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges temporariesDeclaration '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!MethodNode commentStamp: '<historical>' prior: 0!
I am the root of the parse tree..

Instance Variables
	arguments: <SequenceableCollection>
	block: <BlockNode>
	encoder: <BytecodeEncoder>
	localsPool: <IdentitySet>
	locationCounter: <Integer>
	precedence: <Integer>
	primitive: <Integer>
	properties: <AdditionalMethodState|nil>
	selectorOrFalse: <Object>
	sourceText: <String|Text>
	temporaries: <SequenceableCollection>
	temporariesDeclaration: <TemporariesDeclarationNode>

arguments
	- the collection of parsed or decompiled method arguments

block
	- the BlockNode holding the method's statements

encoder
	- the object that comprises the copiler's scope table, literal pool and back-end bytecode generator

localsPool
	- a set used to determine the set of copied values for each block in the method

locationCounter
	- an integer used to mark block scopes for the purposes of the closure transformation.  See BlockNode>>#analyseArguments:temporaries:rootNode:

precedence
	- the precedence of the method's selector (see Symbol>>precedence)

primitive
	- if non-zero this is the integer code of the method's primitive

properties
	- the object used to accumulate method properties (a.k.a. pragmas)

selectorOrFalse
	- the method's selector or false if this is a doit

sourceText
	- the source test from which the method was compiled

temporaries
	- the collection of parsed or decompiled method temporaries

temporariesDeclaration
	- an alternative way to represent the temporaries declaration, by using a parse node to represent that; it should eventually replace the need for the "temporaries" instance variable (because the temporaries can be obtained through this object); every read to "temporaries" can be replaced by sending #allDeclaredVariableNodes to this object; right now the Parser initializes both "temporaries" and "temporariesDeclaration" to ease the migration process!

!classDefinition: #TemporariesDeclarationNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #TemporariesDeclarationNode
	instanceVariableNames: 'tempDeclarationNodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #TemporaryDeclarationNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #TemporaryDeclarationNode
	instanceVariableNames: 'variableNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!ParseNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:29:21'!
isTemporariesDeclaration

	^ false! !


!BlockNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:31:36'!
temporariesDeclaration

	^ temporariesDeclaration! !

!BlockNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:22:01'!
temporariesDeclaration: aTemporariesDeclarationNode
	"RNG: after removing all the usages of the temporaries inst var, the last line can be removed"

	temporariesDeclaration := aTemporariesDeclarationNode.
	self temporaries: aTemporariesDeclarationNode allDeclaredVariableNodes! !


!MethodNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:32:32'!
temporariesDeclaration

	^ temporariesDeclaration! !

!MethodNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:20:11'!
temporariesDeclaration: aTemporariesDeclarationNode
	"RNG: after removing all the usages of the temporaries inst var, the last line can be removed"

	temporariesDeclaration := aTemporariesDeclarationNode.
	self temporaries: aTemporariesDeclarationNode allDeclaredVariableNodes! !

!MethodNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:24:48'!
selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporariesDeclaration _ tempsDeclaration.
	temporaries := tempsDeclaration allDeclaredVariableNodes.
	block := blk.
	primitive := prim.
	properties := propDict.! !

!MethodNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:25:18'!
selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	selectorKeywordsRanges := range.

	^ self selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict! !


!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:28:39'!
declaresAnyVariable

	^ tempDeclarationNodes notEmpty! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:28:44'!
declaresVariable: aVariableNode

	^ self allDeclaredVariableNodes
		anySatisfy: [ :variableNode | variableNode isNamed: aVariableNode name ]! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:28:50'!
isTemporariesDeclaration

	^ true! !

!TemporariesDeclarationNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:28:14'!
allDeclaredVariableNodes

	^ tempDeclarationNodes collect: [ :tempDeclaration | tempDeclaration variableNode ]! !

!TemporariesDeclarationNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:33:16'!
temporaryDeclarationNodes

	^ tempDeclarationNodes! !

!TemporariesDeclarationNode methodsFor: 'printing' stamp: 'RNG 3/7/2020 22:27:53'!
printEachTempVarDeclarationOn: aStream

	^ tempDeclarationNodes do: [ :tempDeclarationNode |
		aStream nextPutAll: tempDeclarationNode variableName; space]! !

!TemporariesDeclarationNode methodsFor: 'printing' stamp: 'RNG 3/7/2020 22:28:00'!
printOn: aStream indent: anInteger

	aStream nextPut: $|; space.
	self printEachTempVarDeclarationOn: aStream.
	aStream nextPut: $|.! !

!TemporariesDeclarationNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:27:28'!
initializeWithAll: aCollectionOfTempDeclarationNodes

	tempDeclarationNodes _ aCollectionOfTempDeclarationNodes! !


!TemporariesDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 3/7/2020 22:27:06'!
empty

	^ self withAll: #()! !

!TemporariesDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 3/7/2020 22:27:12'!
withAll: tempDeclarationNodes

	^ self new initializeWithAll: tempDeclarationNodes! !


!TemporaryDeclarationNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:30:38'!
declaresVariable: aVariableNode

	^ variableNode isNamed: aVariableNode name! !

!TemporaryDeclarationNode methodsFor: 'testing' stamp: 'RNG 3/7/2020 22:30:46'!
isTemporariesDeclaration

	^ true! !

!TemporaryDeclarationNode methodsFor: 'printing' stamp: 'RNG 3/7/2020 22:30:25'!
printOn: aStream indent: anInteger

	aStream nextPut: $|; space.
	variableNode printOn: aStream indent: anInteger.
	aStream space; nextPut: $|.! !

!TemporaryDeclarationNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:30:10'!
variableName

	^ self variableNode name! !

!TemporaryDeclarationNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:30:16'!
variableNode

	^ variableNode! !

!TemporaryDeclarationNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:29:58'!
initializeVariableNode: aVariableNode

	variableNode _ aVariableNode! !


!TemporaryDeclarationNode class methodsFor: 'instance creation' stamp: 'RNG 3/7/2020 22:29:44'!
of: aVariableNode

	^ self new initializeVariableNode: aVariableNode! !


!BlockNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:21:23'!
temporaries
	"Collection of TempVariableNodes"
	
	"RNG: implementation can be changed after adopting the use of temporariesDeclaration inst var.
	After that, the implementation for this message can be changed to:
	^ temporariesDeclaration allDeclaredVariableNodes
	
	Or we can analyze the senders and change the way we request the temporaries"
	
	^temporaries ifNil: [#()]! !

!BlockNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:21:49'!
temporaries: aCollectionOfTemporaries
	"Collection of TempVariableNodes"
	"RNG: deprecated, try to use #temporariesDeclaration: instead"
	
	temporaries := aCollectionOfTemporaries! !


!MethodNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:22:33'!
temporaries
	"For transformations etc, not used in compilation"

	"RNG: implementation can be changed after adopting the use of temporariesDeclaration inst var.
	After that, the implementation for this message can be changed to:
	^ temporariesDeclaration allDeclaredVariableNodes
	
	Or we can analyze the senders and change the way we request the temporaries"
	
	^temporaries! !

!MethodNode methodsFor: 'accessing' stamp: 'RNG 3/7/2020 22:22:44'!
temporaries: aCollectionOfTemporaries
	"For transformations etc, not used in compilation"
	"RNG: deprecated in favor of #temporariesDeclaration:"

	temporaries := aCollectionOfTemporaries! !

!MethodNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:24:02'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim 
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	self 
		selector: selOrFalse
		arguments: args
		precedence: p
		temporaries: temps
		block: blk
		encoder: anEncoder 
		primitive: prim 
		properties: AdditionalMethodState new.! !

!MethodNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:24:16'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."
	"RNG: its external use is deprecated, in favor of any of the methods that receives a temporariesDeclaration instead of the collection of temporaries"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.! !

!MethodNode methodsFor: 'initialization' stamp: 'RNG 3/7/2020 22:24:34'!
selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	selectorKeywordsRanges := range.
	
	^self selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict! !

!classDefinition: #BlockNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #BlockNode
	instanceVariableNames: 'arguments statements returns nArgsNode size remoteCopyNode temporaries optimized optimizedMessageNode actualScopeIfOptimized blockExtent remoteTempNode copiedValues closureCreationNode startOfLastStatement tempsMark temporariesDeclaration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MethodNode category: #'Compiler-ParseNodes'!
CodeNode subclass: #MethodNode
	instanceVariableNames: 'selectorOrFalse precedence arguments block primitive encoder temporaries properties sourceText locationCounter localsPool selectorKeywordsRanges temporariesDeclaration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!TemporariesDeclarationNode reorganize!
('testing' declaresAnyVariable declaresVariable: isTemporariesDeclaration)
('accessing' allDeclaredVariableNodes temporaryDeclarationNodes)
('printing' printEachTempVarDeclarationOn: printOn:indent:)
('initialization' initializeWithAll:)
!


!TemporariesDeclarationNode class reorganize!
('instance creation' empty withAll:)
!


!TemporaryDeclarationNode reorganize!
('testing' declaresVariable: isTemporariesDeclaration)
('printing' printOn:indent:)
('accessing' variableName variableNode)
('initialization' initializeVariableNode:)
!


!TemporaryDeclarationNode class reorganize!
('instance creation' of:)
!

