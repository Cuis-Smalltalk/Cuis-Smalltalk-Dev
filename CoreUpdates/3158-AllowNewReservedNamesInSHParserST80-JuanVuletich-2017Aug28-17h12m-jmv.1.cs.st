'From Cuis 5.0 of 7 November 2016 [latest update: #3157] on 28 August 2017 at 5:16:24 pm'!

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 8/28/2017 17:06:34'!
reservedNames

	^Theme current pseudoVariables! !


!ClassBuilder methodsFor: 'private' stamp: 'jmv 8/28/2017 16:58:40'!
reservedNames
	"Return a list of names that must not be used for variables"
	^#(#self #super #true #false #nil #thisContext)! !


!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 8/28/2017 16:51:25'!
isIncompleteReservedName: aString 
	"Answer true if aString is the start of a reserved name, false otherwise"

	self reservedNames do: [ :arg | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false! !

!SHParserST80 methodsFor: 'identifier testing' stamp: 'jmv 8/28/2017 16:53:14'!
resolve: aString

	self reservedNames do: [ :symbol | aString = symbol ifTrue: [^symbol]].
	(self isBlockTempName: aString) ifTrue: [^#blockTempVar].
	(self isBlockArgName: aString) ifTrue: [^#blockArg].
	(self isMethodTempName: aString) ifTrue: [^#tempVar].
	(self isMethodArgName: aString) ifTrue: [^#methodArg].
	(self isInstVarName: aString) ifTrue: [^#instVar].
	(self isWorkspaceVarName: aString) ifTrue: [^#workspaceVar].
	Symbol hasInterned: aString ifTrue: [ :symbol |
		(self isClassVarName: symbol) ifTrue: [ ^#classVar ].
		(self isPoolConstantName: symbol) ifTrue: [ ^#poolConstant].
		(self isGlobal: symbol) ifTrue: [^#globalVar]].
	^self resolvePartial: aString! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 8/28/2017 16:52:46'!
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
		reservedNames do: aBlock! !

!SHParserST80 methodsFor: 'testing' stamp: 'jmv 8/28/2017 17:14:20'!
isPartialOrFullIdentifier: aSymbol

	(#(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) 
			statePointsTo:aSymbol) ifTrue: [ ^ true ].
	(self reservedNames statePointsTo: aSymbol) ifTrue: [ ^ true ].
	^ false! !

!methodRemoval: SHParserST80 #isReservedName:!
SHParserST80 removeSelector: #isReservedName:!
!methodRemoval: SHParserST80 #reservedNamesDo:!
SHParserST80 removeSelector: #reservedNamesDo:!
