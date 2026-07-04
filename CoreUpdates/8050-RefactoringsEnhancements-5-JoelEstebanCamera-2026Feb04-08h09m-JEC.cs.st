'From Cuis7.9 [latest update: #8045] on 3 July 2026 at 3:46:00 pm'!
!classDefinition: #RenameNodeVariables category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #RenameNodeVariables
	instanceVariableNames: 'extractMethodProgrammer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodCodeModifier category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodCodeModifier
	instanceVariableNames: 'intervalToExtract extractedNode methodNode newMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReturnCodeModifier category: #'Tools-Refactoring'!
ExtractMethodCodeModifier subclass: #ExtractMethodReturnCodeModifier
	instanceVariableNames: 'apparitionIndex renamedVarsExtractedNode sourceRange'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodTempVarCodeModifier category: #'Tools-Refactoring'!
ExtractMethodCodeModifier subclass: #ExtractMethodTempVarCodeModifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodProgrammer category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodProgrammer
	instanceVariableNames: 'intervalToExtract methodNode newMessage extractedCodeModifiers compiledMethod newMethodIsUsingAssignmentBlock newMethodIsUsingReturningBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodValidNameGenerator category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodValidNameGenerator
	instanceVariableNames: 'methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'categoryOfNewSelector existingMethod newSourceCode newSourceCodeGenerator '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration newCompiledMethod extractMethodProgrammer '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 11/Jan/2026, 2:04:22 am (UTC)'!
visitCascadeNode: aCascadeNode 

	self addEquivalentNodesInCascadeNode: aCascadeNode.

	 ^super visitCascadeNode: aCascadeNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 4/Jan/2026, 11:28:17 pm (UTC)'!
visitReturnNode: aReturnNode

	self addToEquivalentsIfOneStatementEquivalentTo: aReturnNode.

	 ^ super visitReturnNode: aReturnNode ! !


!RenameNodeVariables methodsFor: 'visiting' stamp: 'JEC 4/Jan/2026, 7:46:08 pm (UTC)'!
visitTempVariableNode: aTempVariableNode

	extractMethodProgrammer 
		replaceTemporaryVariableNameIfNeededOf: aTempVariableNode.! !

!RenameNodeVariables methodsFor: 'initialization' stamp: 'JEC 4/Jan/2026, 7:40:37 pm (UTC)'!
initializeUsing: anExtractMethodProgrammer

	extractMethodProgrammer := anExtractMethodProgrammer 

	! !


!RenameNodeVariables class methodsFor: 'apply' stamp: 'JEC 4/Jan/2026, 7:40:49 pm (UTC)'!
using: anExtractMethodProgrammer applyVisitOn: aReturnNode

	"By now, ReturnNode is the only one needed to start"
	^ (self new initializeUsing: anExtractMethodProgrammer)
		visitReturnNode: aReturnNode! !


!CompiledMethod methodsFor: 'accessing' stamp: 'JEC 13/Apr/2026, 4:58:03 pm (UTC)'!
messageFromSelector

	^ Message 
		selector: self selector
		arguments: (self methodNode arguments collect: [ :argument | argument name ])! !


!ExtractMethodCodeModifier methodsFor: 'modify source code' stamp: 'JEC 4/Jan/2026, 10:12:23 pm (UTC)'!
modifyExtractedSourceCode: sourceCode

	self subclassResponsibility! !

!ExtractMethodCodeModifier methodsFor: 'modify source code' stamp: 'JEC 31/Dec/2025, 2:14:41 pm (UTC)'!
shouldModifyExtractedSourceCode

	self subclassResponsibility! !

!ExtractMethodCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:53:20 pm (UTC)'!
is: aSymbol

	^ aSymbol == #ExtractMethodExtractedModifier
		or: [ super is: aSymbol ]! !

!ExtractMethodCodeModifier methodsFor: 'private' stamp: 'JEC 8/Jan/2026, 10:25:05 pm (UTC)'!
completeSourceRangesOfExtractedNode
	
	^ self completeSourceRangesOfExtractedNodeIn: methodNode! !

!ExtractMethodCodeModifier methodsFor: 'private' stamp: 'JEC 8/Jan/2026, 10:25:01 pm (UTC)'!
completeSourceRangesOfExtractedNodeIn: aMethodNode
	
	^ aMethodNode 
		completeSourceRangesOf: extractedNode 
		ifAbsent: [ self shouldNotHappen ]! !

!ExtractMethodCodeModifier methodsFor: 'initialization' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
initializeFor: aTempVarNode from: anIntervalToExtract of: aMethodNode to: aNewMessage

	extractedNode := aTempVarNode.
	intervalToExtract := anIntervalToExtract.
	methodNode := aMethodNode.
	newMessage := aNewMessage.! !

!ExtractMethodCodeModifier methodsFor: 'accessing' stamp: 'JEC 31/Dec/2025, 1:44:17 pm (UTC)'!
firstSourceCodeIndex

	self subclassResponsibility! !


!ExtractMethodCodeModifier class methodsFor: 'instance creation' stamp: 'JEC 31/Dec/2025, 1:42:57 pm (UTC)'!
for: aTempVarNode from: anIntervalToExtract of: aMethodNode to: aNewMessage

	^ self new
		initializeFor: aTempVarNode
		from: anIntervalToExtract 
		of: aMethodNode 
		to: aNewMessage! !


!ExtractMethodReturnCodeModifier methodsFor: 'private' stamp: 'JEC 7/Jan/2026, 11:39:35 pm (UTC)'!
detectParseNodeAt: aPosition satisfies: aConditionBlock ifFoundDo: aFoundBlock ifAbsent: anAbsentBlock

	| parseNodes |

	parseNodes := methodNode 
		parseNodesPathAt: aPosition 
		ifAbsent: [ ^ anAbsentBlock value ].
	
	^ parseNodes 
		detect: aConditionBlock
		ifFound: aFoundBlock
		ifNone: anAbsentBlock! !

!ExtractMethodReturnCodeModifier methodsFor: 'private' stamp: 'JEC 7/Jan/2026, 11:45:29 pm (UTC)'!
existsBlockNodeThatContainsExtractedNode

	^ self 
		detectParseNodeAt: sourceRange first 
		satisfies: [ :parseNode | self isBlockNodeIncludedInTheExtractedInterval: parseNode ]
		ifFoundDo: [ :foundParseNode | true ]
		ifAbsent: [ false ]! !

!ExtractMethodReturnCodeModifier methodsFor: 'private' stamp: 'JEC 7/Jan/2026, 11:42:14 pm (UTC)'!
isBlockNodeIncludedInTheExtractedInterval: parseNode

	^ parseNode key isBlockNode and: [ intervalToExtract includesAllOf: parseNode value ]! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code' stamp: 'JEC 8/Jan/2026, 1:50:52 am (UTC)'!
modifyExtractedSourceCode: aSourceCode

	| returnSourceRanges |

	returnSourceRanges := self returnSourceRangesIn: aSourceCode.
	
	^ aSourceCode 
		copyReplacing: { returnSourceRanges at: self apparitionIndex }
		with: self returnCodeToReplace! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code' stamp: 'JEC 7/Jan/2026, 11:49:54 pm (UTC)'!
shouldModifyExtractedSourceCode

	^ self isNestedReturn! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 8/Jan/2026, 1:52:40 am (UTC)'!
apparitionIndex

	| allNodeSourceRanges |

	allNodeSourceRanges := self completeSourceRangesOfExtractedNode.
	
	allNodeSourceRanges := allNodeSourceRanges 
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ].
		
	^ allNodeSourceRanges indexOf: sourceRange! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 4/Jan/2026, 9:50:59 pm (UTC)'!
extractedNodeToModifySourceCode

	"This is needed because if we extracted TempVars, in the new parsed code they will be 
	LiteralVariableNodes and won't be equivalent.
	Also, the names could be different (when the temp vars were renamed).
	"

	^ (Parser 
		parse: (String streamContents: [ :stream | renamedVarsExtractedNode printOn: stream indent: 0])
		class: methodNode methodClass 
		noPattern: true) block statements first "Must be only the return".! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 4/Jan/2026, 10:10:29 pm (UTC)'!
returnCodeToReplace

	| returningBlockName |
	
	returningBlockName := ExtractMethodValidNameGenerator 
		validReturningBlockVarNameFor: methodNode.

	^ String streamContents: [ :stream |
		stream
			nextPut: $^;
			space;
			nextPutAll: returningBlockName;
			space;
			nextPutAll: 'value:';
			space.
		
		renamedVarsExtractedNode printExpressionOn: stream indent: 0 ]! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 8/Jan/2026, 1:55:00 am (UTC)'!
returnSourceRangesIn: aSourceCode

	| sourceCodeMethodNode extractedNodeSourceCode |

	sourceCodeMethodNode := Parser parse: aSourceCode class: methodNode methodClass noPattern: true.
	
	extractedNodeSourceCode := self extractedNodeToModifySourceCode.
	
	^ (sourceCodeMethodNode 
		completeSourceRangesOf: extractedNodeSourceCode
		ifAbsent: [ self completeSourceRangesOfExtractedNodeIn: sourceCodeMethodNode ])
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ].! !

!ExtractMethodReturnCodeModifier methodsFor: 'initialization' stamp: 'JEC 7/Jan/2026, 11:21:58 pm (UTC)'!
initializeFor: aReturnNode from: anIntervalToExtract of: aMethodNode to: aNewMessage sourceRange: aSourceRange renamedVarsNode: aRenamedVarsParseNode

	super 
		initializeFor: aReturnNode 
		from: anIntervalToExtract 
		of: aMethodNode 
		to: aNewMessage.
	
	sourceRange := aSourceRange.
	renamedVarsExtractedNode := aRenamedVarsParseNode.! !

!ExtractMethodReturnCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 12:09:06 am (UTC)'!
is: aSymbol

	^ aSymbol == #ExtractMethodReturnCodeModifier
		or: [ super is: aSymbol ]! !

!ExtractMethodReturnCodeModifier methodsFor: 'testing' stamp: 'JEC 7/Jan/2026, 11:45:16 pm (UTC)'!
isNestedReturn

	^ self existsBlockNodeThatContainsExtractedNode! !

!ExtractMethodReturnCodeModifier methodsFor: 'testing' stamp: 'JEC 7/Jan/2026, 11:45:51 pm (UTC)'!
isUnnestedReturn
 
	^ self existsBlockNodeThatContainsExtractedNode not! !

!ExtractMethodReturnCodeModifier methodsFor: 'accessing' stamp: 'JEC 8/Jan/2026, 1:52:10 am (UTC)'!
firstSourceCodeIndex

	"Must be the first apparition index in the code and not in the extracted code"

	| sourceRanges |
	
	sourceRanges := self completeSourceRangesOfExtractedNode.
	
	^ sourceRanges min: [ :range | range first ].! !


!ExtractMethodReturnCodeModifier class methodsFor: 'instance creation' stamp: 'JEC 7/Jan/2026, 11:22:14 pm (UTC)'!
for: aReturnNode from: anIntervalToExtract of: aMethodNode to: aNewMessage sourceRange: aSourceRange renamedVarsNode: aRenamedVarsParseNode

	^ self new
		initializeFor: aReturnNode
		from: anIntervalToExtract 
		of: aMethodNode 
		to: aNewMessage
		sourceRange: aSourceRange
		renamedVarsNode: aRenamedVarsParseNode! !


!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 10:26:58 pm (UTC)'!
anyParseNodeOfRangeATemporaryDeclaration: aRange 

	^ methodNode 
		anyParseNodeWithin: aRange 
		satisfy: [ :parseNode | parseNode isTemporaryDeclaration ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
hasExtractedAssignment

	^ methodNode
		anyAssignmentNodeWithin: intervalToExtract
		assignsTo: extractedNode ! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
hasExtractedDeclaration

	^ methodNode
		anyDeclarationNodeWithin: intervalToExtract
		declaresVariable: extractedNode! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
hasExtractedTheCompleteVariableContext

	| variableContextSourceRange |
	
	variableContextSourceRange := (methodNode nodeAndSourceRangesThatDeclares: extractedNode) value.

	^ intervalToExtract includesAllOf: variableContextSourceRange first "Should be only one".! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 12:09:30 am (UTC)'!
is: aSymbol

	^ aSymbol == #ExtractMethodTempVarCodeModifier
		or: [ super is: aSymbol ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 11:09:11 pm (UTC)'!
isAssignedToAPartiallyCascadeCode

	self completeTempVarAssignmentsDo: [ :parseNode :sourceRanges |
		(parseNode isAssigningACascadeNode and: [ self isNodePartiallyExtracted: parseNode ])
			ifTrue: [ ^ true ] ].
	
	^ false! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
isDeclaredBy: aTemporaryDeclarationNode 

	^ aTemporaryDeclarationNode declaresVariable: extractedNode! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 4/Jan/2026, 7:52:40 pm (UTC)'!
isTempVarEquivalentTo: aTempVariableNode 

	^ extractedNode equivalentTo: aTempVariableNode! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 8:48:47 pm (UTC)'!
isTheOnlyDeclaredVariableIn: aSourceCode within: intervalToRemove 

	| declarationFirstPosition declarationLastPosition declarationCode varNameIndex |
	
	(1 to: intervalToRemove first) reverseDo: [ :index |
		((aSourceCode at: index) = $| and: [ declarationFirstPosition isNil ])
			ifTrue: [ declarationFirstPosition := index ] ].
	declarationLastPosition := aSourceCode findString: '|' startingAt: intervalToRemove last.
	declarationCode := (aSourceCode copyFrom: declarationFirstPosition to: declarationLastPosition) withBlanksTrimmed.
	varNameIndex := declarationCode findString: self realVariableName.
	
	declarationCode := (declarationCode 
		copyReplacing: { 
			1 to: 1 . 
			varNameIndex to: varNameIndex + self realVariableName size . 
			declarationCode size to: declarationCode size } 
		with: '') withBlanksTrimmed.
	
	^ declarationCode isEmpty! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
isUsedAsAnArgument

	^ newMessage hasArgument: extractedNode name! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 11:05:47 pm (UTC)'!
isUsedOutsideExtractedInterval

	| sourceRanges |

	sourceRanges := self completeSourceRangesOfExtractedNode.
	
	^ (sourceRanges anySatisfy: [ :aRange | 
		(intervalToExtract rangeIncludes: aRange first) not
			and: [ (self anyParseNodeOfRangeATemporaryDeclaration: aRange) not ] ])
	
		or: [ self isAssignedToAPartiallyCascadeCode ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 7:17:42 pm (UTC)'!
shouldBeAssigned

	^ self hasExtractedTheCompleteVariableContext not
		and: [ self hasExtractedAssignment 
		and: [ self isUsedOutsideExtractedInterval ] ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 21/Dec/2025, 3:18:55 pm (UTC)'!
shouldBeDeclared

	"If the declaration is not extracted but there is an assignment, it should be declared.
	It does not matter whether the variable is used as an argument or not, because
	if it is, we must declare a variable with a different name."

	^ self hasExtractedDeclaration not and: [ self hasExtractedAssignment ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 12:38:40 am (UTC)'!
shouldBeReturned

	^ self shouldBeDeclared! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 11:10:09 pm (UTC)'!
shouldDeclarationBeRemoved

	^ self isUsedAsAnArgument not
		and: [ self isUsedOutsideExtractedInterval not
		and: [ self hasExtractedTheCompleteVariableContext not
		and: [ self hasExtractedDeclaration not ] ] ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'testing' stamp: 'JEC 21/Dec/2025, 3:03:30 pm (UTC)'!
shouldRenameVariable

	^ self isUsedAsAnArgument and: [ self shouldBeReturned ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'accessing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
firstSourceCodeIndex

	"Must be the first apparition index in the code and not in the extracted code"

	| sourceRangesOfTemporary |
	
	sourceRangesOfTemporary := methodNode 
		positionsForTemporaryVariable: extractedNode name 
		ifAbsent: [ self shouldNotHappen ].
	
	^ sourceRangesOfTemporary min: [ :range | range first ].! !

!ExtractMethodTempVarCodeModifier methodsFor: 'accessing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
methodOrBlockNodeDeclaringTemporary

	^ (methodNode nodeAndSourceRangesThatDeclares: extractedNode) key! !

!ExtractMethodTempVarCodeModifier methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:27:55 pm (UTC)'!
newVariableName

	^ ExtractMethodValidNameGenerator 
		for: methodNode 
		validRenameVariableOf: self realVariableName! !

!ExtractMethodTempVarCodeModifier methodsFor: 'accessing' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
realVariableName

	^ extractedNode name! !

!ExtractMethodTempVarCodeModifier methodsFor: 'accessing' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
variableName

	^ self shouldRenameVariable
		ifTrue: [ self newVariableName ]
		ifFalse: [ extractedNode name ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code' stamp: 'JEC 4/Jan/2026, 10:12:01 pm (UTC)'!
modifyExtractedSourceCode: sourceCode

	 ^ self renameVariableIn: sourceCode! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code' stamp: 'JEC 7/Jan/2026, 11:50:07 pm (UTC)'!
shouldModifyExtractedSourceCode

	^ self shouldRenameVariable! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
assignVariableUsing: aBlock

	aBlock value: extractedNode name! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
printOn: aStream

	super printOn: aStream.
	aStream 
		nextPut: $(;
		nextPut: $';
		nextPutAll: extractedNode name;
		nextPut: $';
		nextPut: $)! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 27/Dec/2025, 9:46:41 pm (UTC)'!
writeAssignationOn: aStream
	
	self writeRealNameOn: aStream.
	aStream 
		space;
		nextPutAll: ':=';
		space.
	self writeNewNameOn: aStream.! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 27/Dec/2025, 6:36:58 pm (UTC)'!
writeNameOn: aStream
	
	aStream nextPutAll: self variableName! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 27/Dec/2025, 9:45:21 pm (UTC)'!
writeNewNameDeclarationOn: aStream
	
	aStream nextPut: $:.
	self writeNewNameOn: aStream.! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 27/Dec/2025, 9:31:35 pm (UTC)'!
writeNewNameOn: aStream
	
	aStream nextPutAll: self newVariableName! !

!ExtractMethodTempVarCodeModifier methodsFor: 'printing' stamp: 'JEC 27/Dec/2025, 9:43:23 pm (UTC)'!
writeRealNameOn: aStream
	
	aStream nextPutAll: self realVariableName! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove - private' stamp: 'JEC 28/Dec/2025, 9:11:49 pm (UTC)'!
intervalToRemove: node

	^ (methodNode rangeForNode: node ifAbsent: [ self shouldNotHappen ]) asSourceCodeInterval! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove - private' stamp: 'JEC 8/Jan/2026, 11:07:27 pm (UTC)'!
intervalToRemove: temporariesDeclarationNode withLastDeclaredVariableIn: aSourceCode 

	| intervalToRemove |

	intervalToRemove := self intervalToRemove: temporariesDeclarationNode.
	intervalToRemove := (intervalToRemove first to: (aSourceCode findString: '|' startingAt: intervalToRemove second)) 
		asSourceCodeInterval expandUntilNextNonBlankCharacterOn: aSourceCode.
	^ intervalToRemove! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove - private' stamp: 'JEC 28/Dec/2025, 9:19:18 pm (UTC)'!
intervalToRemove: temporariesDeclarationNodes withMultipleDeclarationsIn: aSourceCode 

	^ temporariesDeclarationNodes 
		detect: [ :aTemporaryDeclarationNode | self isDeclaredBy: aTemporaryDeclarationNode  ]
		ifFound: [ :aTemporaryDeclarationNode | 
			(self intervalToRemove: aTemporaryDeclarationNode) expandUntilNextNonBlankCharacterOn: aSourceCode  ]
		ifNone: [ self shouldNotHappen ]
	! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove - private' stamp: 'JEC 28/Dec/2025, 9:16:38 pm (UTC)'!
intervalToRemove: aNode withOneDeclarationIn: aSourceCode 

	^ (self intervalToRemove: aNode)	
		expandUntilStartOfNextStatementOn: aSourceCode! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove - private' stamp: 'JEC 28/Dec/2025, 9:34:41 pm (UTC)'!
intervalToRemoveDeclarationIn: aSourceCode

	| methodOrBlockNodeDeclaringTemporary intervalToRemove temporariesDeclarationNodes temporariesDeclarationNode |

	methodOrBlockNodeDeclaringTemporary := self methodOrBlockNodeDeclaringTemporary.
	temporariesDeclarationNode := methodOrBlockNodeDeclaringTemporary temporariesDeclaration.
	temporariesDeclarationNodes := temporariesDeclarationNode temporaryDeclarationNodes.
	
	intervalToRemove := temporariesDeclarationNodes size = 1
		ifTrue: [ self intervalToRemove: temporariesDeclarationNode withOneDeclarationIn: aSourceCode ]
		ifFalse: [ self intervalToRemove: temporariesDeclarationNodes withMultipleDeclarationsIn: aSourceCode ].
	
	"We are changing aSourceCode and not the methodNode’s sourceText, so it may happen 
	that we are removing the temporaries and this is the last one. In that case, the interval is change
	to remove the entire declaration"
	(temporariesDeclarationNodes size > 1 
		and: [ self isTheOnlyDeclaredVariableIn: aSourceCode within: intervalToRemove ])
		ifTrue: [ intervalToRemove := 
			self intervalToRemove: temporariesDeclarationNode withLastDeclaredVariableIn: aSourceCode ].
	
	^ intervalToRemove! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 31/Dec/2025, 1:44:03 pm (UTC)'!
firstTempVarAssignmentSourceRangeIn: sourceCodeMethodNode 
	
	| sourceRanges |

	sourceRanges := sourceCodeMethodNode sourceRangesAssignmentsTo: extractedNode.
	sourceRanges := sourceRanges 
		asSortedCollection: [ :leftSourceRange :rightSourceRange | leftSourceRange first < rightSourceRange first].
		
	^ sourceRanges first! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 4/Jan/2026, 7:53:49 pm (UTC)'!
renameNodeToNewVariableName: aTempVariableNode 

	aTempVariableNode name: self newVariableName! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 27/Dec/2025, 2:46:28 pm (UTC)'!
renameVariableIn: sourceCode

	| sourceCodeMethodNode firstTempVarAssignmentSourceRange variableSourceRangesAfterFirstAssignment |

	sourceCodeMethodNode := Parser parse: sourceCode class: methodNode methodClass noPattern: true.
	
	firstTempVarAssignmentSourceRange := self firstTempVarAssignmentSourceRangeIn: sourceCodeMethodNode.
	
	variableSourceRangesAfterFirstAssignment := self 
		variableSourceRangesAfterFirstAssignmentIn: sourceCodeMethodNode 
		using: firstTempVarAssignmentSourceRange.
		
	^ sourceCode 
		copyReplacing: variableSourceRangesAfterFirstAssignment
		with: self variableName ! !

!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code - private' stamp: 'JEC 31/Dec/2025, 1:44:04 pm (UTC)'!
variableSourceRangesAfterFirstAssignmentIn: sourceCodeMethodNode using: firstTempVarAssignmentSourceRange 
	
	| variableSourceRanges |
	
	variableSourceRanges := SortedCollection sortBlock: [ :leftInterval :rightInterval | leftInterval first < rightInterval first ].
	sourceCodeMethodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		"this is because sourceCodeMothodNode is a doIt method, and the variable is a literal node"
		(parseNode isVariableNode and: [ parseNode isNamed: extractedNode name ])
			ifTrue: [ variableSourceRanges addAll: sourceRanges ] ].
	
	"all the variable sourceRanges that are after the assignment"
	variableSourceRanges := variableSourceRanges select: [ :sourceRange | sourceRange first > firstTempVarAssignmentSourceRange last ].
	
	"Add the variable to be assigned interval"
	variableSourceRanges add: (firstTempVarAssignmentSourceRange first to: firstTempVarAssignmentSourceRange first + self realVariableName size - 1).
	
	^ variableSourceRanges! !

!ExtractMethodTempVarCodeModifier methodsFor: 'private' stamp: 'JEC 8/Jan/2026, 10:58:59 pm (UTC)'!
completeTempVarAssignmentsDo: aBlock

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentToTemporaryAssigningTo: extractedNode)
			ifTrue: [ aBlock value: parseNode value: sourceRanges ] ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'private' stamp: 'JEC 8/Jan/2026, 11:09:11 pm (UTC)'!
isNodePartiallyExtracted: parseNode

	^ parseNode 
		anySourceRangesOfAssigningValueIn: methodNode
		satisfy: [ :sourceRange | (intervalToExtract intersectsRange: sourceRange) 
				and: [ (intervalToExtract includesAllOf: sourceRange) not ] ]! !

!ExtractMethodTempVarCodeModifier methodsFor: 'remove' stamp: 'JEC 28/Dec/2025, 9:34:09 pm (UTC)'!
removeTemporaryDeclarationIn: aSourceCode

	| intervalToRemove |
	
	intervalToRemove := self intervalToRemoveDeclarationIn: aSourceCode.

	^ aSourceCode 
		copyReplaceFrom: intervalToRemove first 
		to: intervalToRemove last 
		with: ''.! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 8/Jan/2026, 12:58:10 am (UTC)'!
initializeNewSourceCode

	| shouldReturnAnyVariable |
	
	shouldReturnAnyVariable := extractMethodProgrammer shouldReturnAnyVariable.

	newSourceCode := String streamContents: [ :stream |
		stream
			nextPutAll: self newMessageString;
			nextPutAll: self startingMethodIdentation;
			nextPutAllValueOf: [ extractMethodProgrammer declareTempVarsCode ] when: extractMethodProgrammer shouldDeclareAnyVariable;
			nextPutAll: self returnCharacterIfNeeded when: shouldReturnAnyVariable not;
			nextPutAll: sourceCodeToExtract;
			newLineTab:1 when: shouldReturnAnyVariable;
			nextPutAllValueOf: [ extractMethodProgrammer returnTempVarsCode ] when: shouldReturnAnyVariable ]! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 4/Jan/2026, 10:39:52 pm (UTC)'!
modifySourceCodeToExtractIfNecessary

	sourceCodeToExtract := extractMethodProgrammer modifyExtractedSourceCodeIfNecessary: sourceCodeToExtract.! !

!ExtractMethodNewMethodSourceCode methodsFor: 'adjusting' stamp: 'JEC 13/Apr/2026, 10:00:36 pm (UTC)'!
addAssignmentBlock

	extractMethodProgrammer adjustNameWithAssignmentBlock.
	
	"Added here because this will modify all newMessage in the replacements"
	extractMethodProgrammer addAssignmentBlockToNewMessage.! !

!ExtractMethodNewMethodSourceCode methodsFor: 'adjusting' stamp: 'JEC 13/Apr/2026, 8:30:47 pm (UTC)'!
addReturningBlock

	extractMethodProgrammer adjustNameWithReturningBlock.
	
	"Added here because this will modify all newMessage in the replacements"
	extractMethodProgrammer addReturningBlockToNewMessage.! !


!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 27/Dec/2025, 5:05:49 pm (UTC)'!
is: tempVarNode usedBefore: firstAssignmentRange 

	| intervalBeforeAssignment tempVarSourceRanges valueToAssignRange |

	tempVarSourceRanges := (methodNodeToRefactor completeSourceRangesOf: tempVarNode ifAbsent: nil).
	intervalBeforeAssignment := intervalToExtract first to: firstAssignmentRange first - 1.
	valueToAssignRange := (firstAssignmentRange first + tempVarNode name size + 1) to: firstAssignmentRange last.
	
	^ tempVarSourceRanges 
		detect: [ :sourceRange | 
			"Is used before the first assignment or is read in the assignment"
			(intervalBeforeAssignment includesAllOf: sourceRange) 
			or: [ valueToAssignRange includesAllOf: sourceRange ] ]
		ifFound: [ :sourceRange | true ]
		ifNone: [ false ]! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 28/Dec/2025, 12:13:42 am (UTC)'!
isExtractedAlongWithItsAssignment: tempVarNode 
	
	^ methodNodeToRefactor
		anyAssignmentNodeWithin: intervalToExtract
		assignsTo: tempVarNode! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 28/Dec/2025, 12:19:58 am (UTC)'!
isExtractedAlongWithItsDeclaration: tempVarNode

	^ methodNodeToRefactor
		anyDeclarationNodeWithin: intervalToExtract
		declaresVariable: tempVarNode! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 27/Dec/2025, 5:03:23 pm (UTC)'!
isReadBeforeAssignment: tempVarNode 

	| assignmentRanges  |

	assignmentRanges := self orderedAssignmentRangesInIntervalOf: tempVarNode.
	^ self is: tempVarNode usedBefore: assignmentRanges first! !

!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 28/Dec/2025, 11:09:02 pm (UTC)'!
orderedAssignmentRangesInIntervalOf: tempVarNode 

	| completeAssignmentSourceRanges |

	completeAssignmentSourceRanges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges | 
		(parseNode isAssignmentToTemporaryAssigningTo: tempVarNode)
			ifTrue: [ 
				completeAssignmentSourceRanges 
					addAll: (sourceRanges select: [ :sourceRange | intervalToExtract includesAnyOf: sourceRange ]) ] ].
	
	^ completeAssignmentSourceRanges! !


!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 31/Dec/2025, 1:18:14 pm (UTC)'!
assignmentBlockNameIntervalIn: aString 

	^ self 
		intervalOf: self multipleAssignmentBlockName
		in: aString! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
assignmentBlockString

	^ String streamContents: [ :stream |
		stream
			nextPut: $[;
			space.
		
		self extractedTempVarCodeModifiers
			select: [ :extractedTempVar | extractedTempVar hasExtractedAssignment ] 
			thenDo: [ :extractedTempVar | 
				extractedTempVar writeNewNameDeclarationOn: stream.
				stream space ].
			
		stream 
			nextPut: $|;
			space.
		
		self extractedTempVarCodeModifiers 
			select: [ :extractedTempVar | extractedTempVar shouldBeAssigned ] 
			thenDo: [ :extractedTempVar | 
				extractedTempVar writeAssignationOn: stream.
				stream
					nextPut: $.;
					space. ].
		
		stream
			nextPut: $] ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 8/Jan/2026, 12:47:45 am (UTC)'!
hasNestedReturn

	^ self extractedReturnCodeModifiers anySatisfy: [ :returnModifier | returnModifier isNestedReturn ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 8/Jan/2026, 12:47:45 am (UTC)'!
hasUnnestedReturn

	^ self extractedReturnCodeModifiers anySatisfy: [ :returnModifier | returnModifier isUnnestedReturn ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 31/Dec/2025, 1:17:38 pm (UTC)'!
intervalOf: aSubstring in: aString

	| substringIndex |

	substringIndex := aString findString: aSubstring.
	^ substringIndex to: substringIndex + aSubstring size! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 31/Dec/2025, 1:16:20 pm (UTC)'!
returningBlockNameIntervalIn: aString 

	^ self 
		intervalOf: self returnBlockName
		in: aString! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 4/Jan/2026, 10:30:07 pm (UTC)'!
returningBlockString

	| newVarName |
	
	newVarName := ExtractMethodValidNameGenerator 
		validReturningBlockVariableNameFor: methodNode.

	^ String streamContents: [ :stream | 
		stream
			nextPut: $[;
			space;
			nextPut: $:;
			nextPutAll: newVarName;
			space;
			nextPut: $|;
			space;
			nextPut: $^;
			space;
			nextPutAll: newVarName;
			space;
			nextPut: $] ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 4/Jan/2026, 3:41:49 pm (UTC)'!
sendCollaborationStringForMultipleVariablesTo: collaborationString

	| intervalToReplace |

	"This is needed because, otherwise, it would replace a character after the assignment block string."
	intervalToReplace := self assignmentBlockNameIntervalIn: collaborationString.

	^ collaborationString 
		copyReplacing: { intervalToReplace first to: intervalToReplace last - 1 }
		with: self assignmentBlockString.! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 4/Jan/2026, 3:34:47 pm (UTC)'!
sendCollaborationStringUsingReturnBlockTo: collaborationString

	^ collaborationString 
		copyReplacing: { self returningBlockNameIntervalIn: collaborationString }
		with: self returningBlockString.! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 7/Jan/2026, 11:00:55 pm (UTC)'!
shouldAddReturn

	^ self hasUnnestedReturn! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 13/Apr/2026, 5:07:41 pm (UTC)'!
shouldAddReturningBlock

	^ newMethodIsUsingReturningBlock or: [ self hasNestedReturn ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 13/Apr/2026, 10:44:37 pm (UTC)'!
shouldAssignSendCollaborationToSingleVariable

	^ (self shouldDeclareOneVariable or: [ self shouldReturnOnlyOneVariable ]) 
		and: [ self hasUnnestedReturn not 
		and: [ newMethodIsUsingAssignmentBlock not ] ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
shouldAssingSendCollaborationToSingleVariableIfTrueDo: aBlock

	^ self shouldAssignSendCollaborationToSingleVariable
		ifTrue: [
			self extractedTempVarCodeModifiers
				select: [ :extractedTempVar | extractedTempVar shouldBeAssigned ] 
				thenDo: [ :extractedTempVar | extractedTempVar assignVariableUsing: aBlock ] ]! !

!ExtractMethodProgrammer methodsFor: 'send collaboration - private' stamp: 'JEC 1/Jan/2026, 5:05:03 pm (UTC)'!
shouldBeEnclosedWithParens

	| notOptimizedMethodNode initialNode finalNode parseNodesInCommon initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	notOptimizedMethodNode := compiledMethod notOptimizedMethodNode .
	initialNodeAncestors  := notOptimizedMethodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors := notOptimizedMethodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon := initialNodeAncestors intersection: finalNodeAncestors.

	initialNode := (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode := (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions := initialNode isMessageNode and: [ finalNode isMessageNode ].

	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 8/Jan/2026, 1:32:41 am (UTC)'!
declaredVariablesToReturn

	^ self variablesToDeclare
		select: [ :tempVar | tempVar isUsedOutsideExtractedInterval ]! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 8/Jan/2026, 12:47:56 am (UTC)'!
extractedReturnCodeModifiers

	^ extractedCodeModifiers select: [ :modifier | modifier is: #ExtractMethodReturnCodeModifier ]! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 8/Jan/2026, 12:47:56 am (UTC)'!
extractedTempVarCodeModifiers

	^ extractedCodeModifiers select: [ :modifier | modifier is: #ExtractMethodTempVarCodeModifier ]! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:22:17 pm (UTC)'!
multipleAssignmentBlockName

	^ ExtractMethodValidNameGenerator 
		validAssignmentBlockVarNameFor: methodNode! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 14/Apr/2026, 1:11:47 pm (UTC)'!
singleVariableToReturn

	^ self declaredVariablesToReturn first realVariableName ! !

!ExtractMethodProgrammer methodsFor: 'accessing' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
variablesToDeclare

	^ self extractedTempVarCodeModifiers select: [ :extractedTempVar | extractedTempVar shouldBeDeclared ]! !

!ExtractMethodProgrammer methodsFor: 'source code - private' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
declareTempVarsCode

	^ String streamContents: [ :stream |
		stream 
			nextPut: $|; 
			space.
		
		self extractedTempVarCodeModifiers
			select: [ :extractedTempVar | extractedTempVar shouldBeDeclared ] 
			thenDo: [ :extractedTempVar | 
				extractedTempVar writeNameOn: stream.
				stream space. ].
		
		stream 
			nextPut: $|;
			newLineTab ]! !

!ExtractMethodProgrammer methodsFor: 'source code - private' stamp: 'JEC 4/Jan/2026, 7:55:08 pm (UTC)'!
renameTempVarsIfNeeded: aReturnNode

	RenameNodeVariables 
		using: self
		applyVisitOn: aReturnNode.
	
	^ aReturnNode! !

!ExtractMethodProgrammer methodsFor: 'source code - private' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
replaceTemporaryVariableNameIfNeededOf: aTempVariableNode 

	self extractedTempVarCodeModifiers
		detect: [ :tempVarModifier | (tempVarModifier isTempVarEquivalentTo: aTempVariableNode) 
			and: [ tempVarModifier shouldRenameVariable ] ]
		ifFound: [ :tempVarModifier | tempVarModifier renameNodeToNewVariableName: aTempVariableNode ]
		ifNone: []! !

!ExtractMethodProgrammer methodsFor: 'source code - private' stamp: 'JEC 13/Apr/2026, 8:09:13 pm (UTC)'!
returnTempVarsCode

	| variablesToReturn |

	variablesToReturn := self variablesToReturn.

	^ String streamContents: [ :stream |
		stream 
			nextPut: $^;
			space.
		
		variablesToReturn size = 1
			ifTrue: [ variablesToReturn first writeNameOn: stream ]
			ifFalse: [ 
			stream nextPutAll: self multipleAssignmentBlockName.
			
				variablesToReturn do: [ :extractedTempVar |
					stream
						space
						nextPutAll: 'value:';
						space.
					
					extractedTempVar writeNameOn: stream ] ] ].! !

!ExtractMethodProgrammer methodsFor: 'source code - private' stamp: 'JEC 13/Apr/2026, 8:09:00 pm (UTC)'!
variablesToReturn

	| variablesToDeclare |

	variablesToDeclare := self variablesToDeclare.

	^ newMethodIsUsingAssignmentBlock
		ifTrue: [ variablesToDeclare ]
		ifFalse: [ variablesToDeclare select: [ :declaredVariable | 
				declaredVariable isUsedOutsideExtractedInterval ] ]! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 12:57:56 am (UTC)'!
anyDeclaredTempVarShouldBeReturned

	^ self variablesToDeclare 
		anySatisfy: [ :tempVar | tempVar isUsedOutsideExtractedInterval ]! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 13/Apr/2026, 8:05:33 pm (UTC)'!
shouldAddAssignmentBlock

	^ newMethodIsUsingAssignmentBlock
		or: [ self variablesToDeclare size > 1 
			and: [ self shouldReturnMultipleVariables
			and: [ self hasUnnestedReturn not ] ] ]! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 12:47:36 am (UTC)'!
shouldDeclareAnyVariable

	^ self extractedTempVarCodeModifiers anySatisfy: [ :extractedTempVar | extractedTempVar shouldBeDeclared ]! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 27/Dec/2025, 11:15:01 pm (UTC)'!
shouldDeclareOneVariable

	^ self variablesToDeclare size = 1! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 13/Apr/2026, 8:05:33 pm (UTC)'!
shouldReturnAnyVariable

	^ newMethodIsUsingAssignmentBlock 
		or: [ self hasUnnestedReturn not and: [ self anyDeclaredTempVarShouldBeReturned ] ]! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 8/Apr/2026, 1:45:48 pm (UTC)'!
shouldReturnMultipleVariables

	^ self declaredVariablesToReturn size > 1! !

!ExtractMethodProgrammer methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 1:32:31 am (UTC)'!
shouldReturnOnlyOneVariable

	^ self declaredVariablesToReturn size = 1! !

!ExtractMethodProgrammer methodsFor: 'new message - private' stamp: 'JEC 13/Apr/2026, 8:01:56 pm (UTC)'!
addAssignmentBlockToNewMessage

	newMessage 
		addKeyword: (self assignmentsKeywordNameUsing: newMessage) 
		withArgument: self multipleAssignmentBlockName! !

!ExtractMethodProgrammer methodsFor: 'new message - private' stamp: 'JEC 13/Apr/2026, 8:02:16 pm (UTC)'!
addReturningBlockToNewMessage

	newMessage 
		addKeyword: (self returnKeywordNameUsing: newMessage) 
		withArgument: self returnBlockName! !

!ExtractMethodProgrammer methodsFor: 'new message - private' stamp: 'JEC 4/Jan/2026, 10:17:35 pm (UTC)'!
assignmentsKeywordNameUsing: aNewMessage

	^ ExtractMethodValidNameGenerator 
		for: methodNode
		validAssignmentsKeywordNameTo: aNewMessage! !

!ExtractMethodProgrammer methodsFor: 'new message - private' stamp: 'JEC 4/Jan/2026, 10:07:19 pm (UTC)'!
returnBlockName

	^ ExtractMethodValidNameGenerator 
		validReturningBlockVarNameFor: methodNode! !

!ExtractMethodProgrammer methodsFor: 'new message - private' stamp: 'JEC 4/Jan/2026, 10:20:11 pm (UTC)'!
returnKeywordNameUsing: aNewMessage
	
	^ ExtractMethodValidNameGenerator 
		for: methodNode 
		validReturningBlockKeywordNameTo: aNewMessage! !

!ExtractMethodProgrammer methodsFor: 'remove' stamp: 'JEC 13/Apr/2026, 11:24:48 pm (UTC)'!
removeTemporaryDeclarationsIfNecessaryIn: sourceCode

	| updatedSourceCode |

	updatedSourceCode := sourceCode.
	
	self extractedTempVarCodeModifiers reverse 
		select: [ :extractedTempVar | extractedTempVar shouldDeclarationBeRemoved ]
		thenDo: [ :extractedTempVar | 
			updatedSourceCode := extractedTempVar removeTemporaryDeclarationIn: updatedSourceCode ].

	^ updatedSourceCode! !

!ExtractMethodProgrammer methodsFor: 'new message' stamp: 'JEC 13/Apr/2026, 8:12:15 pm (UTC)'!
adjustNameWithAssignmentBlock

	newMethodIsUsingAssignmentBlock := true.! !

!ExtractMethodProgrammer methodsFor: 'new message' stamp: 'JEC 13/Apr/2026, 8:31:18 pm (UTC)'!
adjustNameWithReturningBlock

	newMethodIsUsingReturningBlock := true! !

!ExtractMethodProgrammer methodsFor: 'new message' stamp: 'JEC 14/Apr/2026, 3:39:27 pm (UTC)'!
messageFullName

	^ newMessage fullName! !

!ExtractMethodProgrammer methodsFor: 'send collaboration' stamp: 'JEC 14/Apr/2026, 3:39:23 pm (UTC)'!
sendCollaborationString

	| collaborationString |

	collaborationString := 'self ' , self messageFullName.
	
	self shouldAddAssignmentBlock 
		ifTrue: [ collaborationString := self sendCollaborationStringForMultipleVariablesTo: collaborationString ].
	
	self shouldAddReturningBlock
		ifTrue: [ collaborationString := self sendCollaborationStringUsingReturnBlockTo: collaborationString ].
	
	self shouldBeEnclosedWithParens 
		ifTrue: [ collaborationString := '(' , collaborationString , ')' ].
		
	self shouldAssingSendCollaborationToSingleVariableIfTrueDo:
		[ :tempVar | collaborationString := tempVar, ' := ', collaborationString ].
		
	self shouldAddReturn 
		ifTrue: [ ^ '^ ' , collaborationString ].
	
	^ collaborationString! !

!ExtractMethodProgrammer methodsFor: 'source code' stamp: 'JEC 8/Jan/2026, 12:47:56 am (UTC)'!
modifyExtractedSourceCodeIfNecessary: sourceCode

	| updatedSourceCode |
	
	updatedSourceCode := sourceCode.
	
	extractedCodeModifiers
		select: [ :modifier | modifier shouldModifyExtractedSourceCode ]
		thenDo: [ :modifier | updatedSourceCode := modifier modifyExtractedSourceCode: updatedSourceCode  ].
	
	^ updatedSourceCode! !

!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'JEC 8/Jan/2026, 10:52:47 pm (UTC)'!
initializeExtractedModifiers

	| returnNodesWithSourceRanges |
	
	returnNodesWithSourceRanges := OrderedCollection new.
	extractedCodeModifiers := SortedCollection sortBlock: [ :leftExtractedTempVar :rightExtractedTempVar |
			leftExtractedTempVar firstSourceCodeIndex < rightExtractedTempVar firstSourceCodeIndex ].

	methodNode 
		completeSourceRangesWithin: intervalToExtract 
		do: [ :parseNode :sourceRanges |
			(parseNode isTemp and: [ parseNode isArg not ])
				ifTrue: [
					extractedCodeModifiers add: 
						(ExtractMethodTempVarCodeModifier 
							for: parseNode 
							from: intervalToExtract 
							of: methodNode 
							to: newMessage) ].
			
			parseNode isReturn
				ifTrue: [ returnNodesWithSourceRanges add: parseNode -> sourceRanges ] ].
	
	"It is necessary to know, for the return nodes, whether the temporary variables in them 
	are going to be renamed.
	This is because, when replacing the code, if the temporary variables are renamed, 
	we no longer have that information and won’t be able to find the place to replace."
	returnNodesWithSourceRanges do: [ :returnNodeWithSourceRanges |
		| returnNode sourceRanges renamedVarsReturnNode |
		
		returnNode := returnNodeWithSourceRanges key.
		sourceRanges := returnNodeWithSourceRanges value.
		
		"If we don't do this with a veryDeepCopy, it will rename all the nodes in the method node also"
		renamedVarsReturnNode := self renameTempVarsIfNeeded: returnNode veryDeepCopy.
		
		sourceRanges withIndexDo: [ :sourceRange :index |
			extractedCodeModifiers add:
			(ExtractMethodReturnCodeModifier
				for: returnNode 
				from: intervalToExtract 
				of: methodNode 
				to: newMessage
				sourceRange: sourceRange
				renamedVarsNode: renamedVarsReturnNode) ] ]
	! !

!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'JEC 14/Apr/2026, 3:25:47 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage

	intervalToExtract := anIntervalToExtract.
	compiledMethod := aMethodToExtractCodeFrom.
	methodNode := aMethodToExtractCodeFrom methodNode.
	newMessage := aNewMessage copy.
	newMethodIsUsingAssignmentBlock := false.
	newMethodIsUsingReturningBlock := false.
	
	self initializeExtractedModifiers.! !


!ExtractMethodProgrammer class methodsFor: 'instance creation' stamp: 'JEC 3/Feb/2026, 10:23:10 pm (UTC)'!
from: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage

	^ self new
		initializeFrom: anIntervalToExtract 
		of: aMethodToExtractCodeFrom 
		to: aNewMessage! !


!ExtractMethodValidNameGenerator methodsFor: 'validation' stamp: 'JEC 3/Jan/2026, 3:48:38 pm (UTC)'!
isAvailableKeywordName: newKeyword

	^ (methodNode methodClass lookupSelector: newKeyword asSymbol) isNil! !

!ExtractMethodValidNameGenerator methodsFor: 'validation' stamp: 'JEC 3/Jan/2026, 3:35:12 pm (UTC)'!
isNewVariableNameAvailable: newVarName 

	^ (methodNode methodClass allInstVarNames includes: newVarName) not
		and: [ (methodNode hasArgumentOrTemporaryNamed: newVarName) not ]! !

!ExtractMethodValidNameGenerator methodsFor: 'name generator - private' stamp: 'JEC 4/Jan/2026, 12:13:52 am (UTC)'!
validNameWithPrefix: aNamePrefix andSuffix: aSuffix validatingWith: aValidationBlock

	| newVarName number |

	newVarName := aNamePrefix , aSuffix.
	number := 0.
	[ aValidationBlock value: newVarName ] whileFalse: [
		number := number + 1.
		newVarName := aNamePrefix , number asString , aSuffix ].

	^ newVarName! !

!ExtractMethodValidNameGenerator methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 12:15:59 am (UTC)'!
validNewKeywordName: aNewKeyword addingTo: aMessageSelector 

	^ self 
		validNameWithPrefix: aNewKeyword 
		andSuffix: ':'
		validatingWith: [ :aKeyword | self isAvailableKeywordName: aMessageSelector , aKeyword ]! !

!ExtractMethodValidNameGenerator methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 12:16:48 am (UTC)'!
validNewVarNameWithPrefix: aNamePrefix 

	^ self 
		validNameWithPrefix: aNamePrefix
		andSuffix: ''
		validatingWith: [ :aNewVarName | self isNewVariableNameAvailable: aNewVarName ]! !

!ExtractMethodValidNameGenerator methodsFor: 'initialization' stamp: 'JEC 4/Jan/2026, 10:08:11 pm (UTC)'!
initializeFor: aMethodNode 

	methodNode := aMethodNode.! !


!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:16:56 pm (UTC)'!
for: aMethodNode validAssignmentsKeywordNameTo: aNewMessage

	^ self for: aMethodNode
		validNewKeywordName: (self assignmentsNewKeywordNameTo: aNewMessage) 
		addingTo: aNewMessage selector! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:07:45 pm (UTC)'!
for: aMethodNode validNewKeywordName: aNewKeyword addingTo: aMessageSelector
	
	^ (self for: aMethodNode)
		validNewKeywordName: aNewKeyword addingTo: aMessageSelector! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:07:59 pm (UTC)'!
for: aMethodNode validNewVarNameWithPrefix: aPrefix

	^ (self for: aMethodNode)
		validNewVarNameWithPrefix: aPrefix 
	! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:28:57 pm (UTC)'!
for: aMethodNode validRenameVariableOf: aVariableName

	^ self 
		for: aMethodNode
		validNewVarNameWithPrefix: 'new' , aVariableName capitalized 
	! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:26:38 pm (UTC)'!
for: aMethodNode validReturningBlockKeywordNameTo: aNewMessage

	^ self 
		for: aMethodNode
		validNewKeywordName: (self returningBlockNewKeywordNameTo: aNewMessage) 
		addingTo: aNewMessage selector! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:26:13 pm (UTC)'!
validAssignmentBlockVarNameFor: aMethodNode

	^ self 
		for: aMethodNode
		validNewVarNameWithPrefix: self assignmentBlockNamePrefix! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:26:22 pm (UTC)'!
validReturningBlockVarNameFor: aMethodNode

	^ self 
		for: aMethodNode
		validNewVarNameWithPrefix: self returningBlockNamePrefix! !

!ExtractMethodValidNameGenerator class methodsFor: 'name generator' stamp: 'JEC 4/Jan/2026, 10:31:03 pm (UTC)'!
validReturningBlockVariableNameFor: aMethodNode

	^ self 
		for: aMethodNode 
		validNewVarNameWithPrefix: self returningBlockVariableNamePrefix
	! !

!ExtractMethodValidNameGenerator class methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:22:32 pm (UTC)'!
assignmentBlockNamePrefix

	^ 'anAssignmentBlock'! !

!ExtractMethodValidNameGenerator class methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:17:08 pm (UTC)'!
assignmentsNewKeywordNameTo: aMessage

	^ aMessage arguments size > 0 
		ifTrue: [ #assigningWith ] 
		ifFalse: [ #assigningWith capitalized ]! !

!ExtractMethodValidNameGenerator class methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:04:21 pm (UTC)'!
returningBlockNamePrefix

	^ 'aReturningBlock'! !

!ExtractMethodValidNameGenerator class methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:20:31 pm (UTC)'!
returningBlockNewKeywordNameTo: aMessage

	^ aMessage arguments size > 0
		ifTrue: [ #returningWith ]
		ifFalse: [ #returningWith capitalized ].! !

!ExtractMethodValidNameGenerator class methodsFor: 'accessing' stamp: 'JEC 4/Jan/2026, 10:31:10 pm (UTC)'!
returningBlockVariableNamePrefix
	
	^ 'res'! !

!ExtractMethodValidNameGenerator class methodsFor: 'instance creation' stamp: 'JEC 4/Jan/2026, 10:08:20 pm (UTC)'!
for: aMethodNode

	^ self new initializeFor: aMethodNode! !


!ExtractMethod methodsFor: 'private - applying' stamp: 'JEC 14/Apr/2026, 1:14:42 pm (UTC)'!
allReplacementsReturnsADifferentSingleVariable

	| singleVariablesToReturn |
			
	singleVariablesToReturn := collectionOfReplacements 
		select: [ :replacement | replacement shouldReturnOneVariable ]
		thenCollect: [ :replacement | replacement singleVariableToReturn ].
	
	^ singleVariablesToReturn asSet size > 1! !

!ExtractMethod methodsFor: 'private - applying' stamp: 'JEC 14/Apr/2026, 2:01:37 pm (UTC)'!
unifyNewMethodAndReplacements

	| shouldAddAssignmentBlock shouldAddReturningBlock |

	shouldAddAssignmentBlock := shouldAddReturningBlock := false.

	collectionOfReplacements do: [ :replacement |
		replacement needsAssignmentBlock ifTrue: [ shouldAddAssignmentBlock := true ].
		replacement needsReturningBlock ifTrue: [ shouldAddReturningBlock := true ] ].
	
	"If all replacements returns a single variable, but a different one between them, 
	they must use assignmentBlock"
	shouldAddAssignmentBlock ifFalse: [ 
		shouldAddAssignmentBlock := self allReplacementsReturnsADifferentSingleVariable ].
	
	shouldAddAssignmentBlock ifTrue: [ extractMethodNewMethod addAssignmentBlock ].
	shouldAddReturningBlock ifTrue: [ extractMethodNewMethod addReturningBlock ].

	collectionOfReplacements do: [ :replacement | 
		shouldAddAssignmentBlock ifTrue: [ replacement addAssignmentBlock ].
		shouldAddReturningBlock ifTrue: [ replacement addReturningBlock ] ]! !


!ExtractMethodNewMethod methodsFor: 'initialization' stamp: 'JEC 13/Apr/2026, 7:28:33 pm (UTC)'!
initializeOf: aMethodToExtractCodeFrom in: aCategory newSourceCodeFrom: aNewSourceCodeGenerator   

	existingMethod := aMethodToExtractCodeFrom.
	categoryOfNewSelector := aCategory.
	newSourceCodeGenerator := aNewSourceCodeGenerator ! !

!ExtractMethodNewMethod methodsFor: 'private - applying' stamp: 'JEC 13/Apr/2026, 7:30:49 pm (UTC)'!
compileNewSourceCode

	| selector classToCompile |
	
	classToCompile := existingMethod methodClass.

	selector := classToCompile
		compile: newSourceCode 
		classified: categoryOfNewSelector.
		
	^ classToCompile >> selector.! !

!ExtractMethodNewMethod methodsFor: 'private - applying' stamp: 'JEC 13/Apr/2026, 7:29:49 pm (UTC)'!
generateNewSourceCode

	newSourceCode := newSourceCodeGenerator value
! !

!ExtractMethodNewMethod methodsFor: 'adjusting' stamp: 'JEC 13/Apr/2026, 7:42:53 pm (UTC)'!
addAssignmentBlock

	newSourceCodeGenerator addAssignmentBlock! !

!ExtractMethodNewMethod methodsFor: 'adjusting' stamp: 'JEC 13/Apr/2026, 8:30:10 pm (UTC)'!
addReturningBlock

	newSourceCodeGenerator addReturningBlock! !


!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'HAW 2/Sep/2021, 9:36:44 pm (UTC)'!
intervalToExtract
	
	^intervalToExtract! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'JEC 13/Apr/2026, 7:20:13 pm (UTC)'!
needsAssignmentBlock

	^ extractMethodProgrammer shouldAddAssignmentBlock ! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'JEC 13/Apr/2026, 7:20:22 pm (UTC)'!
needsReturningBlock

	^ extractMethodProgrammer shouldAddReturningBlock ! !

!ExtractMethodReplacement methodsFor: 'testing' stamp: 'JEC 14/Apr/2026, 1:07:57 pm (UTC)'!
shouldReturnOneVariable

	^ extractMethodProgrammer shouldReturnOnlyOneVariable! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'JEC 13/Apr/2026, 10:02:24 pm (UTC)'!
addAssignmentBlock

	extractMethodProgrammer adjustNameWithAssignmentBlock.
	
	extractMethodProgrammer addAssignmentBlockToNewMessage.! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'JEC 13/Apr/2026, 10:02:09 pm (UTC)'!
addReturningBlock

	extractMethodProgrammer adjustNameWithReturningBlock.
	
	"Added here because this will modify all newMessage in the replacements"
	extractMethodProgrammer addReturningBlockToNewMessage.! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'JEC 14/Apr/2026, 1:11:09 pm (UTC)'!
singleVariableToReturn

	"Here we know that a single variable is going to be returned"
	^ extractMethodProgrammer singleVariableToReturn! !


!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'exceptions' stamp: 'JEC 4/Feb/2026, 12:39:21 pm (UTC)'!
signalSourceCodeIncludesAReturnStatementError

	self refactoringError: self class errorMessageForSourceCodeIncludingAReturnStatement! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'pre-conditions' stamp: 'JEC 4/Feb/2026, 12:39:30 pm (UTC)'!
assertSourceCodeDoesNotIncludeReturnExpressions

	self thereAreReturnExpressions
		ifTrue: [ self signalSourceCodeIncludesAReturnStatementError ]! !

!SourceCodeOfTemporaryToBeExtractedPrecondition methodsFor: 'private' stamp: 'JEC 4/Feb/2026, 12:38:54 pm (UTC)'!
thereAreReturnExpressions

	^ (methodNode
		allParseNodesWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isReturn ]) not! !


!SourceCodeOfTemporaryToBeExtractedPrecondition class methodsFor: 'error messages' stamp: 'JEC 4/Feb/2026, 12:40:23 pm (UTC)'!
errorMessageForSourceCodeIncludingAReturnStatement

	^ self refactoringErrorMessagePrefix , 'the selected code includes a return statement'! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 11/Jan/2026, 2:06:36 am (UTC)'!
addToEquivalentsNodesPartiallyOf: aCascadeNode 

	| intervalToAdd cascadeNodeSourceRange |
	
	cascadeNodeSourceRange := self completeSourceRangeOf: aCascadeNode.
	
	intervalToAdd := (cascadeNodeSourceRange includesAnyOf: selectionToKeep)
		ifTrue: [ selectionToKeep ]
		ifFalse: [
			(cascadeNodeSourceRange first 
				to: (cascadeNodeSourceRange first + selectionToKeep size - 1))
				asSourceCodeInterval ].

	equivalentNodesRanges add: intervalToAdd! !


!ExtractMethodNewMethodSourceCode methodsFor: 'value' stamp: 'JEC 13/Apr/2026, 8:00:40 pm (UTC)'!
value

	self 
		initializeSourceCodeToExtract;
		modifySourceCodeToExtractIfNecessary;
		initializeNewSourceCode;
		renameArguments. 
	
	^newSourceCode ! !

!ExtractMethodNewMethodSourceCode methodsFor: 'initialization' stamp: 'JEC 13/Apr/2026, 10:08:02 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage withArgumentNames: renamedArguments  
	intervalToExtract := anIntervalToExtract.
	existingMethod := aMethodToExtractCodeFrom.
	newMessage := aNewMessage copy.
	newArgumentNames := renamedArguments.
	
	extractMethodProgrammer := ExtractMethodProgrammer 
		from: intervalToExtract 
		of: existingMethod 
		to: newMessage! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 4/Jan/2026, 10:42:01 pm (UTC)'!
initializeSourceCodeToExtract

	sourceCodeToExtract := existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last.
	
	((sourceCodeToExtract endsWith: '.') not and: [ extractMethodProgrammer shouldReturnAnyVariable ])
		ifTrue: [ sourceCodeToExtract := sourceCodeToExtract , '.' ].! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 29/Dec/2025, 12:36:28 am (UTC)'!
needsExplicitReturn: extractedMethodNode

	^ (extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ])
		or: [ extractedMethodNode hasReturnExpressions ]! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 14/Apr/2026, 3:38:40 pm (UTC)'!
newMessageString

	^ extractMethodProgrammer messageFullName! !

!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'JEC 29/Dec/2025, 12:33:58 am (UTC)'!
returnCharacterIfNeeded

	| extractedMethodNode |
	
	extractedMethodNode := Parser parse: sourceCodeToExtract class: self sourceClass noPattern: true.

	^ (self needsExplicitReturn: extractedMethodNode) ifTrue: [ '' ] ifFalse: [ '^ ' ]! !


!ExtractMethodParametersDetector methodsFor: 'private' stamp: 'JEC 28/Dec/2025, 11:31:13 pm (UTC)'!
shouldBeParameterized: parseNode appearingIn: sourceRanges
	
	^ (self intervalToExtractIncludesAnyOf: sourceRanges)
		and: [ parseNode isTempOrArg 
		and: [ (self definedInOuterScope: parseNode )
		and: [ (self isExtractedAlongWithItsDeclaration: parseNode) not
		and: [ (self isExtractedAlongWithItsAssignment: parseNode) not
			or: [ self isReadBeforeAssignment: parseNode ] ] ] ] ]! !


!ExtractMethod methodsFor: 'private - applying' stamp: 'JEC 13/Apr/2026, 11:24:36 pm (UTC)'!
applyReplacements
	
	| replacementsByMethod |
	
	"Do not use equivalence by #= with compiledMethods because it does not compare the selectors!!"
	replacementsByMethod := collectionOfReplacements groupBy: [ :aReplacement | 
		aReplacement methodToExtractFrom classAndSelector ].
	replacementsByMethod valuesDo: [ :aMethodReplacements | self applyMethodReplacements: aMethodReplacements ].
	! !

!ExtractMethod methodsFor: 'applying' stamp: 'JEC 13/Apr/2026, 7:15:45 pm (UTC)'!
apply
	
	self 
		unifyNewMethodAndReplacements;
		createNewMethod;
		applyReplacements ! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'JEC 13/Apr/2026, 7:15:17 pm (UTC)'!
newDefinition: anExtractMethodNewMethod replacements: aCollectionOfReplacements 
	
	^ self new 
		initializeNewDefinition: anExtractMethodNewMethod 
		replacements: aCollectionOfReplacements! !


!ExtractMethodNewMethod methodsFor: 'applying' stamp: 'JEC 13/Apr/2026, 7:31:10 pm (UTC)'!
apply

	^ self 
		generateNewSourceCode;
		compileNewSourceCode.! !


!ExtractMethodNewMethod class methodsFor: 'instance creation' stamp: 'JEC 13/Apr/2026, 7:28:13 pm (UTC)'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments

	| trimmedIntervalToExtract newSourceCodeGenerator |

	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom notOptimizedMethodNode.

	newSourceCodeGenerator := ExtractMethodNewMethodSourceCode
		fromInterval: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		withArgumentNames: renamedArguments.

	^ self new
		initializeOf: aMethodToExtractCodeFrom
		in: aCategory
		newSourceCodeFrom: newSourceCodeGenerator! !


!ExtractMethodReplacement methodsFor: 'applying' stamp: 'JEC 13/Apr/2026, 3:38:05 pm (UTC)'!
apply

	| updatedSourceCode |
	
	self initializeSendCollaboration.
	
	updatedSourceCode := self updatedSourceCodeOfExistingMethod.

	updatedSourceCode := extractMethodProgrammer removeTemporaryDeclarationsIfNecessaryIn: updatedSourceCode.

	self sourceClass
		compile: updatedSourceCode
		classified: methodToExtractFrom category! !

!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'JEC 13/Apr/2026, 10:03:46 pm (UTC)'!
initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage

	intervalToExtract := anIntervalToExtract.
	methodToExtractFrom := aMethodToExtractCodeFrom.
	newMessage := aNewMessage copy.

	extractMethodProgrammer := ExtractMethodProgrammer 
		from: intervalToExtract 
		of: methodToExtractFrom 
		to: aNewMessage
! !

!ExtractMethodReplacement methodsFor: 'initialization' stamp: 'JEC 13/Apr/2026, 5:08:23 pm (UTC)'!
initializeSendCollaboration

	sendCollaboration := extractMethodProgrammer sendCollaborationString! !

!ExtractMethodReplacement methodsFor: 'private - source code' stamp: 'JEC 3/Jan/2026, 12:27:16 am (UTC)'!
newMessageString

	^ extractMethodProgrammer sendCollaborationString! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'evaluating' stamp: 'JEC 29/Dec/2025, 12:20:31 am (UTC)'!
value

	self
		initializeParseNodesMatchingSelectionInterval;
		assertSourceCodeIsNotLeftSideOfAssignment;
		assertSourceCodeIsNotPartOfTemporariesDeclaration;
		assertSourceCodeIsNotPartOfMethodSignature;
		assertSourceCodeDoesNotHaveTemporaryVariablesUsedOutsideOfSelectionInterval;
		assertSourceCodeContainsAValidExpression! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'JEC 8/Jan/2026, 9:08:45 pm (UTC)'!
isNotATempDeclarationWithUsagesOutOfIntervalToExtract

	initialNode key isTemporariesDeclaration ifFalse: [ ^ true ].
	
	^ intervalToExtract last = sourceCode size
		or: [
			(methodNode
				anyParseNodeWithin: (intervalToExtract last to: sourceCode size)
				satisfy: [ :parseNode |
					parseNode isVariableNode
					and: [ initialNode key declaresVariable: parseNode ] ]) not ]! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'JEC 3/Jan/2026, 12:20:58 am (UTC)'!
startAndEndNodesShareAParentNode  

	| parseNodesInCommon |
	parseNodesInCommon := self parseNodesInCommon.

	^ parseNodesInCommon notEmpty and: [
		(self trimmed: parseNodesInCommon first value) includesAllOf: intervalToExtract]


! !

!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'JEC 18/Dec/2025, 11:39:59 pm (UTC)'!
startAndEndParseNodesAreTheSame

	^ initialNode key = finalNode key
		and: [ initialNode value = finalNode value ]! !

!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition class #errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration stamp: 'jmv 3/Jul/2026, 7:43:45 pm (UTC)'!
SourceCodeOfMethodToBeExtractedPrecondition class removeSelector: #errorMessageForExtractedTemporaryAssignmentWithoutItsDeclaration!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #thereAreNoLocalVariableAssignmentsWithoutDeclaration stamp: 'jmv 3/Jul/2026, 7:43:45 pm (UTC)'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #thereAreNoLocalVariableAssignmentsWithoutDeclaration!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #isNotDeclaredWithinIntervalToExtract: stamp: 'jmv 3/Jul/2026, 7:43:45 pm (UTC)'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #isNotDeclaredWithinIntervalToExtract:!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration stamp: 'jmv 3/Jul/2026, 7:43:45 pm (UTC)'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #assertSourceCodeDoesNotIncludeTemporaryAssignmentsWithoutDeclaration!
!methodRemoval: SourceCodeOfMethodToBeExtractedPrecondition #signalTemporaryAssignmentWithoutDeclarationError stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
SourceCodeOfMethodToBeExtractedPrecondition removeSelector: #signalTemporaryAssignmentWithoutDeclarationError!
!methodRemoval: SourceCodeIntervalPrecondition class #errorMessageForSourceCodeIncludingAReturnStatement stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
SourceCodeIntervalPrecondition class removeSelector: #errorMessageForSourceCodeIncludingAReturnStatement!
!methodRemoval: SourceCodeIntervalPrecondition #assertSourceCodeDoesNotIncludeReturnExpressions stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
SourceCodeIntervalPrecondition removeSelector: #assertSourceCodeDoesNotIncludeReturnExpressions!
!methodRemoval: SourceCodeIntervalPrecondition #signalSourceCodeIncludesAReturnStatementError stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
SourceCodeIntervalPrecondition removeSelector: #signalSourceCodeIncludesAReturnStatementError!
!methodRemoval: ExtractMethodNewMethod #initializeOf:in:newSourceCode: stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
ExtractMethodNewMethod removeSelector: #initializeOf:in:newSourceCode:!
!methodRemoval: ExtractMethodParametersDetector #isNotExtractedAlongWithItsDeclaration: stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
ExtractMethodParametersDetector removeSelector: #isNotExtractedAlongWithItsDeclaration:!
!methodRemoval: ExtractMethodNewMethodSourceCode #initiliazeNewSourceCode stamp: 'jmv 3/Jul/2026, 7:43:46 pm (UTC)'!
ExtractMethodNewMethodSourceCode removeSelector: #initiliazeNewSourceCode!
!classDefinition: #ExtractMethodNewMethodSourceCode category: #'Tools-Refactoring'!
Object subclass: #ExtractMethodNewMethodSourceCode
	instanceVariableNames: 'intervalToExtract newMessage newSourceCode existingMethod newArgumentNames sourceCodeToExtract extractMethodProgrammer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'categoryOfNewSelector existingMethod newSourceCode newSourceCodeGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration newCompiledMethod extractMethodProgrammer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
