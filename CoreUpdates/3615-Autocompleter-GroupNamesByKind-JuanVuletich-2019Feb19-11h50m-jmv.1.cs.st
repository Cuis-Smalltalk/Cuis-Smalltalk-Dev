'From Cuis 5.0 of 7 November 2016 [latest update: #3612] on 19 February 2019 at 12:26:57 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'jmv 2/19/2019 12:26:00'!
computeIdentifierEntries
	"Use an aux Set to avoid duplicates, but keep the order given."
	| entriesSet lastTitle |
	
	entriesSet _ Set new.
	lastTitle _ nil.
	entries _ Array streamContents: [ :strm |
		parser namesBeginningWith: prefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle _ kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]]! !


!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:09:28'!
isBlockArgName: aString 
	"Answer true if aString is the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:09:18'!
isBlockTempName: aString 
	"Answer true if aString is the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:09:31'!
isIncompleteBlockArgName: aString 
	"Answer true if aString is the start of the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:09:21'!
isIncompleteBlockTempName: aString 
	"Answer true if aString is the start of the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:09:05'!
isIncompleteClassVarName: aString 
	"Answer true if aString is the start of the name of a class variable, false otherwise"

	self classVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:54'!
isIncompleteInstVarName: aString 
	"Answer true if aString is the start of the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:42'!
isIncompleteMethodArgName: aString 
	"Answer true if aString is the start of the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:34'!
isIncompleteMethodTempName: aString 
	"Answer true if aString is the start of the name of a method temporary, false otherwise."

	self methodTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:07:49'!
isIncompletePoolConstantName: aString 
	"Answer true if aString is the start of the name of a pool constant, false otherwise"

	self poolConstantNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:07:38'!
isIncompleteWorkspaceVarName: aString 
	"Answer true if aString is the  start of the name of an workspace variable, false otherwise"

	self workspaceNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:58'!
isInstVarName: aString 
	"Answer true if aString is the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:45'!
isMethodArgName: aString 
	"Answer true if aString is the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 2/19/2019 12:08:28'!
isMethodTempName: aString 
	"Answer true if aString is the name of a method temporary, false otherwise.
    Does not check whether aString is also a block temporary
    or argument"

	self methodTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:23:37'!
blockArgNamesDo: aBlock
	"Iterate over block argument names valid at current blockDepth"

	| title |
	title _ '--- Block Arguments ---'.
	blockDepth to: 1 by: -1 do: [ :level |
		arguments at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:23:47'!
blockTempNamesDo: aBlock
	"Iterate over block temp names valid at current blockDepth"

	| title |
	title _ '--- Block Variables ---'.
	blockDepth to: 1 by: -1 do: [ :level |
		temporaries at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:23:53'!
classVarNamesDo: aBlock

	| title |
	title _ '--- Class Variables ---'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c classPool keysDo: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:24:00'!
instVarNamesDo: aBlock

	| title |
	title _ '--- Instance Variables ---'.
	instanceVariables do: [ :name | aBlock value: name value: title ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:24:05'!
methodArgNamesDo: aBlock
	"Iterate over method argument names"

	| title |
	title _ '--- Method Arguments ---'.
	^arguments at: 0  ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:24:10'!
methodTempNamesDo: aBlock
	"Iterate over method temporary names"

	| title |
	title _ '--- Method Variables ---'.
	^temporaries at: 0 ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:20:50'!
namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name :kindOfIdentifierTitle |
		(name beginsWith: aString) ifTrue: [ aBlock value: name value: kindOfIdentifierTitle ]].
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '--- Classes ---' ] in: Smalltalk classNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '--- Globals ---' ] in: Smalltalk nonClassNames! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:24:43'!
nonGlobalNamesDo: aBlock
	"Evaluate aBlock over all available names, except for globals"

	self
		blockArgNamesDo: aBlock;
		blockTempNamesDo: aBlock;
		methodArgNamesDo: aBlock;
		methodTempNamesDo: aBlock;
		instVarNamesDo: aBlock;
		classVarNamesDo: aBlock;
		poolConstantNamesDo: aBlock;
		workspaceNamesDo: aBlock.
	self
		reservedNames do: [ :name |
			aBlock value: name value: '--- Pseudovariables ---'  ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:24:53'!
poolConstantNamesDo: aBlock

	| title |
	title _ '--- Pool Variables ---'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [ :pool |
					pool bindingsDo: [ :assoc | aBlock value: assoc key value: title ]]]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 12:25:01'!
workspaceNamesDo: aBlock

	| title |
	title _ '--- Workspace Variables ---'.
	workspace ifNotNil: [
		workspace bindingNamesDo: [ :name | aBlock value: name value: title ] ]! !

