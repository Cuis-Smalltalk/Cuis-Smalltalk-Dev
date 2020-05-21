'From Cuis 5.0 [latest update: #4133] on 9 May 2020 at 4:10:58 pm'!
"Change Set:		4134-CuisCore-AuthorName-2020May04-19h23m
Date:			9 May 2020
Author:			Nahuel Garbezza

Main changes:

Introduce the SourceCodeInterval class as an especialization of Interval, capable of dealing with source code transformations. Start to use SourceCodeInterval in the source ranges reported by the Parser, and on the intervals created on refactorings. This helped us to reduce utilitary methods related source code on the Refactoring and ParseNode classes.

Changes on refactorings:

* [extract temporary] allow to extract entire statements without introducing an unnecessary extra statement
* [extract temporary] do not allow the user to extract on a smalltalk editor that does not contain a method
* [extract temporary] change the #apply message to return the updated source code
* [extract method] allow to extract expressions with multiple levels of parentheses and spaces between them"!

!classDefinition: #SourceCodeInterval category: #'Compiler-Kernel'!
Interval subclass: #SourceCodeInterval
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!SourceCodeInterval commentStamp: 'RNG 5/8/2020 22:10:02' prior: 0!
I represent a special case of interval, I refer to source code intervals. There are two main users of me:

* The debugger (to display which is the current piece of code being debugged)
* Refactorings (to select code for refactoring, validating against other intervals and rewriting code)!


!Interval methodsFor: 'converting' stamp: 'RNG 5/8/2020 20:59:03'!
asSourceCodeInterval

	^ SourceCodeInterval from: start to: stop! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'RNG 5/9/2020 15:41:07'!
expandToMatchExpressionOn: aSourceCode
	"takes a source code and if the source range references an expression
	that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstChar lastChar |
	firstChar _ aSourceCode at: start - 1 ifAbsent: [ nil ].
	lastChar _ aSourceCode at: stop + 1 ifAbsent: [ nil ].
	^ (self canBeExpandedStartingWith: firstChar endingWith: lastChar)
		ifTrue: [ (self expandBy: 1) expandToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'RNG 5/8/2020 21:42:20'!
trimToMatchExpressionOn: aSourceCode

	| startOffset endOffset initialChar endingChar shouldTrimStart shouldTrimEnd shouldTrimBoth |
	startOffset := 0.
	endOffset := 0.
	initialChar := aSourceCode at: start ifAbsent: [ nil ].
	endingChar := aSourceCode at: stop ifAbsent: [ nil ].
	shouldTrimBoth := initialChar = $( and: [ endingChar = $) ].
	shouldTrimStart := self canBeTrimmed: initialChar.
	shouldTrimEnd := self canBeTrimmed: endingChar.
	(shouldTrimBoth or: [ shouldTrimStart ]) ifTrue: [ startOffset := 1 ]. 
	(shouldTrimBoth or: [ shouldTrimEnd ]) ifTrue: [ endOffset := 1 ]. 
	^ (shouldTrimBoth or: [ shouldTrimStart ] or: [ shouldTrimEnd ])
		ifTrue: [ (self trimLeft: startOffset right: endOffset) trimToMatchExpressionOn: aSourceCode ]
		ifFalse: [ self ]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/9/2020 15:39:55'!
canBeExpandedStartingWith: firstChar endingWith: lastChar

	^ (firstChar = $( and: [ lastChar = $) ]) or: [ firstChar = $` and: [ lastChar = $` ] ]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/8/2020 21:30:59'!
canBeTrimmed: initialChar

	^ initialChar notNil and: [ initialChar isSeparator or: [ initialChar = $. ] ]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/9/2020 15:38:43'!
expandBy: quantity

	^ (start - quantity to: stop + quantity) asSourceCodeInterval! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'RNG 5/8/2020 21:41:42'!
trimLeft: startOffset right: endOffset

	^ (start + startOffset to: stop - endOffset) asSourceCodeInterval! !


!ParseNode methodsFor: 'source ranges' stamp: 'RNG 5/8/2020 21:59:21'!
expandRange: parentSourceRange basedOnChildRange: childSourceRange

	^ ((parentSourceRange first min: childSourceRange first) to: parentSourceRange last) asSourceCodeInterval! !


!MethodNode methodsFor: 'source mapping' stamp: 'RNG 5/6/2020 23:25:56'!
singleCompleteSourceRangeOf: requestedParseNode ifPresent: sourceRangePresentBlock ifAbsent: sourceRangeAbsentBlock
	"Finds the source range associated with the requested parse node.
	If it is present, evaluates sourceRangePresentBlock with the result.
	Otherwise, it evaluates sourceRangeAbsentBlock.
	Raises an error if the requested parse node has multiple source ranges"

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [
			sourceRanges size > 1 ifTrue: [
				self error: 'there are multiple source ranges for the parse node: ' , requestedParseNode printString ].
			^ sourceRangePresentBlock value: sourceRanges first ] ].
	^ sourceRangeAbsentBlock value! !


!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/4/2020 19:29:08'!
addAssignmentToCurrentStatement

	self
		insertAt: intervalToExtract first
		newCodeWith: newVariableName , ' ' , self preferredAssignmentOperator , ' '! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/6/2020 23:20:14'!
extractingAnEntireStatement

	^ self siblingStatementsOfTemporaryAssignment anySatisfy: [ :statement |
		methodNodeToRefactor
			singleCompleteSourceRangeOf: statement
			ifPresent: [ :sourceRange | sourceRange = intervalToExtract ]
			ifAbsent: [ false ] ]! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/6/2020 22:46:32'!
positionToInsertNewVariableDeclaration

	^ (methodNodeToRefactor singleCompleteSourceRangeOf: parseNodeWithNewVariableScope temporariesDeclaration) last - 1! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/4/2020 19:32:43'!
resolveNewVariableAssignment

	self extractingAnEntireStatement
		ifTrue: [ self addAssignmentToCurrentStatement ]
		ifFalse: [
			self
				replaceExtractedCodeWithNewTemporaryVariable;
				writeAssignmentStatementOfNewTemporaryVariable ]! !


!ParseNode methodsFor: 'source ranges' stamp: 'RNG 5/8/2020 21:59:47'!
expandRange: aSourceRange basedOn: sourceRangesOfChildNode

	| intervals |
	intervals := self consolidateAsCollection: sourceRangesOfChildNode.
	intervals withIndexDo: [ :interval :index |
		(interval first > aSourceRange first) ifTrue: [
			^ self expandRange: aSourceRange basedOnChildRange: (intervals at: index - 1 ifAbsent: [ intervals last ]) ] ].
	^ self expandRange: aSourceRange basedOnChildRange: intervals last! !

!ParseNode methodsFor: 'source ranges' stamp: 'RNG 5/9/2020 15:41:07'!
expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

	^ (self consolidateAsCollection: sourceRanges)
		collect: [ :sourceRange | sourceRange expandToMatchExpressionOn: sourceCode ]! !


!Encoder methodsFor: 'source mapping' stamp: 'RNG 5/8/2020 21:10:16'!
addMultiRange: aRange for: aNode

	| ranges |
	
	"I'm using an OrderedCollection because ranges are added in order, while parsing the source code.
	If this constrain is not hold, a SortedCollection should be used - Hernan"
	ranges := sourceRanges at: aNode ifAbsentPut: [ OrderedCollection new ].
	ranges add: aRange asSourceCodeInterval.
	
	^aNode ! !

!Encoder methodsFor: 'source mapping' stamp: 'RNG 5/8/2020 21:10:23'!
noteSourceRange: range forNode: node

	sourceRanges at: node put: range asSourceCodeInterval! !


!BraceNode methodsFor: 'private' stamp: 'RNG 5/6/2020 23:50:26'!
hasEquivalentElementsTo: aBraceNode

	elements size ~= aBraceNode elements size ifTrue: [ ^ false ]. 

	elements with: aBraceNode elements do: [ :myElement :otherElement |
		(myElement equivalentTo: otherElement) ifFalse: [ ^ false ] ].
	^ true! !


!CascadeNode methodsFor: 'testing' stamp: 'RNG 5/6/2020 23:51:24'!
hasEquivalentMessagesWith: aCascadeNode

	messages size ~= aCascadeNode messages size ifTrue: [ ^ false ].

	messages with: aCascadeNode messages do: [ :myMessage :otherNodeMessage |
		(myMessage equivalentTo: otherNodeMessage) ifFalse: [ ^ false ] ].
	^ true! !


!CodeNode methodsFor: 'private' stamp: 'RNG 5/6/2020 23:52:46'!
hasEquivalentArgumentsWith: aCodeNode

	self arguments size ~= aCodeNode arguments size ifTrue: [ ^ false ].

	self arguments with: aCodeNode arguments do: [ :myArgument :otherCodeNodeArgument |
		(myArgument equivalentTo: otherCodeNodeArgument) ifFalse: [ ^ false ] ].
	^ true! !


!BlockNode methodsFor: 'private' stamp: 'RNG 5/6/2020 23:54:27'!
hasEquivalentStatementsWith: aBlockNode

	statements size ~= aBlockNode statements size ifTrue: [ ^ false ].

	statements with: aBlockNode statements do: [ :myStatement :otherBlockNodeStatement |
		(myStatement equivalentTo: otherBlockNodeStatement) ifFalse: [ ^ false ] ].
	^ true! !


!MethodNode methodsFor: 'source mapping' stamp: 'RNG 5/6/2020 23:24:10'!
singleCompleteSourceRangeOf: requestedParseNode
	"Returns the source range associated with the requested parse node.
	Fails if there is no source range, or if there are multiple source ranges."

	self
		singleCompleteSourceRangeOf: requestedParseNode
		ifPresent: [ :sourceRange | ^ sourceRange ]
		ifAbsent: [ self error: 'could not find source range for node: ' , requestedParseNode printString ]! !


!MessageNode methodsFor: 'private' stamp: 'RNG 5/6/2020 23:53:18'!
compare: myArguments with: othersArguments

	myArguments size ~= othersArguments size ifTrue: [ ^ false ].

	myArguments with: othersArguments do: [ :myArgument :otherArgument |
		(myArgument equivalentTo: otherArgument) ifFalse: [ ^ false ] ].
	^ true! !


!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'RNG 5/6/2020 23:55:08'!
declaresSameVariablesThan: aTemporariesDeclarationNode

	tempDeclarationNodes size ~= aTemporariesDeclarationNode temporaryDeclarationNodes size ifTrue: [ ^ false ].

	tempDeclarationNodes with: aTemporariesDeclarationNode temporaryDeclarationNodes do: [ :myTempDeclaration :otherTempDeclaration |
		(myTempDeclaration equivalentTo: otherTempDeclaration) ifFalse: [ ^ false ] ].
	^ true! !


!SmalltalkEditor methodsFor: 'extract to temporary' stamp: 'RNG 5/8/2020 21:30:47'!
extractToTemporary

	"To prevent the refactoring to be evaluated on editors w/o methods like the workspace - Nahuel"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractToTemporaryApplier createAndValueHandlingExceptions: [
			ExtractToTemporaryApplier
				for: self selectionInterval asSourceCodeInterval
				of: self codeProvider currentCompiledMethod ] ]! !

!SmalltalkEditor methodsFor: 'extract method' stamp: 'RNG 5/8/2020 21:30:28'!
extractMethod

	"To prevent the refactoring to be evaluated on editors w/o methods like the workspace - Nahuel"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractMethodApplier createAndValueHandlingExceptions: [
			ExtractMethodApplier
				for: self selectionInterval asSourceCodeInterval
				of: self codeProvider currentCompiledMethod ] ]! !


!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 5/8/2020 21:12:34'!
intervalCoversCompleteAstNodes 

	^ (self trimmed: (initialNode value first to: finalNode value last) asSourceCodeInterval) = intervalToExtract! !

!ExtractMethodExpressionValidation methodsFor: 'validation - private' stamp: 'RNG 5/8/2020 21:12:12'!
trimmed: aSourceCodeInterval

	^ aSourceCodeInterval trimToMatchExpressionOn: sourceCode! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'RNG 5/8/2020 21:03:17'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory

	| trimmedIntervalToExtract |
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToExtractCodeFrom sourceCode.
	self
		assert: newMessage selector canBeDefinedIn: aMethodToExtractCodeFrom methodClass;
		assert: trimmedIntervalToExtract isValidIntervalOn: aMethodToExtractCodeFrom;
		assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodToExtractCodeFrom methodNode.

	^ self new
		initializeFrom: trimmedIntervalToExtract
		of: aMethodToExtractCodeFrom
		to: newMessage
		in: aCategory! !


!ExtractToTemporary methodsFor: 'applying' stamp: 'RNG 5/9/2020 15:12:17'!
apply

	self
		resolveNewVariableAssignment;
		declareNewTemporaryVariable;
		reflectSourceCodeChanges.
	^ updatedSourceCode! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/6/2020 22:46:41'!
addNewTemporaryVariableToExistingDeclarationStatement

	self
		insertAt: self positionToInsertNewVariableDeclaration
		newCodeWith: ' ' , newVariableName! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/6/2020 23:28:33'!
reflectSourceCodeChanges

	methodToRefactor methodClass
		compile: updatedSourceCode
		classified: methodToRefactor category! !

!ExtractToTemporary methodsFor: 'private - applying steps ' stamp: 'RNG 5/6/2020 23:29:53'!
statementNodeIncludingCodeToExtract

	^ self siblingStatementsOfTemporaryAssignment detect: [ :statement |
		methodNodeToRefactor
			singleCompleteSourceRangeOf: statement
			ifPresent: [ :sourceRange | sourceRange last >= intervalToExtract last ]
			ifAbsent: [ false ] ]! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'RNG 5/8/2020 21:04:28'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor methodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		declaringTempIn: codeNodeForNewVariable! !

!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'RNG 5/8/2020 21:04:18'!
assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract

	|  trimmedSourceCodeToExtract methodNodeToExtract sourceCodeToExtract trimmedIntervalToExtract originalSourceCode |

	self assert: anIntervalToExtract isValidIntervalOn: aMethodToRefactor.
	originalSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := originalSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: originalSourceCode.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	methodNodeToExtract := self tryToParse: trimmedSourceCodeToExtract on: aMethodToRefactor methodClass.
	self
		assertHasOneStatement: methodNodeToExtract;
		assertIsNotReturn: trimmedSourceCodeToExtract;
		assert: trimmedIntervalToExtract enclosesACompleteExpressionOn: aMethodToRefactor;
		assert: trimmedIntervalToExtract enclosesAValidNodeOn: aMethodToRefactor! !

!ExtractToTemporary class methodsFor: 'private - preconditions' stamp: 'RNG 5/8/2020 21:04:03'!
anySourceRangeOf: aMethodToRefactor matches: anIntervalToExtract

	| sourceRangeCollections allSourceRanges sourceCode |
	sourceRangeCollections := aMethodToRefactor methodNode completeSourceRanges values.
	allSourceRanges := sourceRangeCollections
		inject: Set new
		into: [ :sourceRanges :sourceRangeCollection | sourceRanges addAll: sourceRangeCollection; yourself ].
	sourceCode := aMethodToRefactor sourceCode.
	^ allSourceRanges anySatisfy: [ :sourceRange |
		(sourceRange trimToMatchExpressionOn: sourceCode) = anIntervalToExtract ]! !


!ExtractMethodApplier class methodsFor: 'instance creation' stamp: 'RNG 5/8/2020 21:03:23'!
for: anIntervalToExtract of: aMethodToExtractCodeFrom

	| trimmedIntervalToExtract sourceCode |
	sourceCode := aMethodToExtractCodeFrom sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode..

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new initializeFor: trimmedIntervalToExtract of: aMethodToExtractCodeFrom! !

!methodRemoval: ExtractToTemporary #lastTemporaryDeclaration stamp: 'RNG 5/6/2020 22:47:15'!
ExtractToTemporary removeSelector: #lastTemporaryDeclaration!
!methodRemoval: Refactoring class #trim:toMatchExpressionOn: stamp: 'RNG 5/8/2020 21:26:08'!
Refactoring class removeSelector: #trim:toMatchExpressionOn:!
!methodRemoval: Refactoring class #trim:matchingParenthesesOn: stamp: 'RNG 5/4/2020 20:23:33'!
Refactoring class removeSelector: #trim:matchingParenthesesOn:!
!methodRemoval: Refactoring class #shouldTrimToMatchExpressionOn:atIndex: stamp: 'RNG 5/4/2020 20:23:36'!
Refactoring class removeSelector: #shouldTrimToMatchExpressionOn:atIndex:!
!methodRemoval: ParseNode #expandIfEnclosed:on: stamp: 'RNG 5/9/2020 15:36:56'!
ParseNode removeSelector: #expandIfEnclosed:on:!

!Interval reorganize!
('accessing' anyOne at: at:put: extent first increment last rangeIncludes: size)
('comparing' = hash)
('adding' add:)
('removing' remove:)
('enumerating' do: permutationsDo: reverseDo:)
('printing' printOn: storeOn:)
('private' setFrom:to:count: species)
('testing' includes: isInterval)
('arithmetic' + -)
('converting' asSourceCodeInterval)
!


!SourceCodeInterval reorganize!
('source code' expandToMatchExpressionOn: trimToMatchExpressionOn:)
('private - source code' canBeExpandedStartingWith:endingWith: canBeTrimmed: expandBy: trimLeft:right:)
!


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('comment' comment comment:)
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRange:basedOn: expandRange:basedOnChildRange: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBlockNode isBraceNode isCascadeNode isComplex isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
!


!Refactoring class reorganize!
('exceptions' canNotRefactorDueToReferencesError:references:to: canNotRefactorDueToReferencesErrorClass refactoringError: refactoringErrorClass)
('warnings' refactoringWarning: refactoringWarningClass referencesWarningClass warnAboutReferences:with:of:toAll:)
('testing' canRefactor:)
('allowed to refactor class compilers' addAllowedToRefactorClassCompiler: allowedToRefactorClassCompilers allowedToRefactorClassCompilersIncludes:)
!


!ExtractToTemporary reorganize!
('applying' apply)
('initialization' initializeNamed:extractingCodeAt:from:declaringTempIn:)
('private - applying steps ' addAssignmentToCurrentStatement addNewTemporaryVariableToExistingDeclarationStatement declareNewTemporaryVariable extractingAnEntireStatement formattedNewVariableAssignment hasTemporariesDeclarationBlock insertAt:newCodeWith: insertNewTemporaryDeclarationWithNewVariable positionToInsertNewTemporaryVariableAssignment positionToInsertNewVariableDeclaration preferredAssignmentOperator reflectSourceCodeChanges replaceExtractedCodeWithNewTemporaryVariable resolveNewVariableAssignment siblingStatementsOfTemporaryAssignment statementNodeIncludingCodeToExtract writeAssignmentStatementOfNewTemporaryVariable)
!


!ExtractToTemporaryApplier class reorganize!
('instance creation' for:of:)
('pre-conditions' assertCanApplyRefactoringOn:at:)
!

