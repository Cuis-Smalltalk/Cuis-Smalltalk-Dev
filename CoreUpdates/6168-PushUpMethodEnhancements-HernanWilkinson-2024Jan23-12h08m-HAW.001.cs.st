'From Cuis6.3 [latest update: #6167] on 23 January 2024 at 12:43:32 pm'!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
TransferMethod subclass: #PushUpMethod
	instanceVariableNames: 'subclassesToRemoveMethodFrom methods '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
TransferMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: 'subclassesToRemoveMethodsFrom dependantMethods '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ParseNode methodsFor: 'testing' stamp: 'RNG 12/31/2017 19:10:30'!
sendsMessageToSelf: aSelector

	^ false! !


!BlockNode methodsFor: 'testing' stamp: 'RNG 12/31/2017 19:11:55'!
sendsMessageToSelf: aSelector

	^ self statements anySatisfy: [ :statementNode | statementNode sendsMessageToSelf: aSelector ]! !


!BlockNode class methodsFor: 'instance creation' stamp: 'RNG 1/19/2018 21:06:55'!
empty

	^ self statements: #() returns: false! !


!MethodNode methodsFor: 'testing' stamp: 'MEGL 7/30/2023 20:56:02'!
sendsMessageToSelf: aSelector

	^ self block sendsMessageToSelf: aSelector! !


!SelectorNode methodsFor: 'testing' stamp: 'HAW 3/14/2019 11:14:25'!
isNamed: aSelectorName

	^ self key = aSelectorName! !


!VariableNode class methodsFor: 'instance creation' stamp: 'RNG 1/19/2018 20:50:48'!
named: aString 

	^ self new name: aString; yourself! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 3/14/2019 11:13:47'!
sendsMessageToSelf: aSelector

	^ self receiver referencesSelf and: [ self selector isNamed: aSelector ]! !


!ReturnNode methodsFor: 'testing' stamp: 'MEGL 7/30/2023 20:56:22'!
sendsMessageToSelf: aSelector

	^expr sendsMessageToSelf: aSelector! !


!TransferMethod methodsFor: 'applying - private' stamp: 'MEGL 8/20/2023 21:13:45'!
methodCategory: aMethod

	^aMethod methodClass organization categoryOfElement: aMethod selector! !

!TransferMethod methodsFor: 'applying - private' stamp: 'MEGL 8/20/2023 21:15:51'!
removeMethod: aMethod

	aMethod methodClass removeSelector: aMethod selector.
 ! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 8/21/2023 14:09:29'!
compileMethodInSuperclass: aMethod

	aMethod methodClass superclass
		compile: aMethod sourceCode
		classified: (self methodCategory: aMethod).! !

!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 1/23/2024 12:40:28'!
pushUpMethods

	methods do: [ :methodToPushUp | 
		self 
			compileMethodInSuperclass: methodToPushUp;
			removeMethod: methodToPushUp ]! !

!PushUpMethod methodsFor: 'initialization' stamp: 'MEGL 8/21/2023 15:43:39'!
initializeFor: aMethods

	methods := aMethods.
	subclassesToRemoveMethodFrom := #().! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'MEGL 8/21/2023 15:44:19'!
for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom pushingUp: dependantMethods
	
	dependantMethods add: aMethodToPushUp.

	self assertIsValidToPushUp: dependantMethods.
	self assertIsValidToRemove: subclassesToRemoveMethodFrom.
	
	^self new initializeFor: dependantMethods additionallyRemovingFrom: subclassesToRemoveMethodFrom.! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'MEGL 8/15/2023 23:52:22'!
assert: aMethodToRemove isValidToRemoveFrom: subclassesToRemoveMethodFrom

	self assert: subclassesToRemoveMethodFrom areSiblingsOf: aMethodToRemove methodClass.
	self assert: aMethodToRemove isImplementedIn: subclassesToRemoveMethodFrom.
	self assert: aMethodToRemove isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom.

! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'MEGL 8/21/2023 15:23:05'!
assertIsValidToPushUp: methods

	| instanceVariablesToPushUp methodsImplementedInSuperClass |
	
	instanceVariablesToPushUp := self selectInstanceVariablesToPushUpFrom: methods.
	
	instanceVariablesToPushUp ifNotEmpty: [ self refactoringWarning: (self accessingInstanceVariableWarningMessage: instanceVariablesToPushUp)].

	methodsImplementedInSuperClass := (methods 	select: [ 
		:method |  		self checkIfMethodIsImplementedInSuperClass: method. 
		]) collect: [:method | method selector].
		
	methodsImplementedInSuperClass ifNotEmpty: [ self refactoringWarning: (self methodWithDifferentImplementationInSuperClass: methodsImplementedInSuperClass)].
	
	
! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'MEGL 8/15/2023 23:52:58'!
assertIsValidToRemove: subclassesToRemoveMethodFrom

	subclassesToRemoveMethodFrom associationsDo: [:assoc | 
		self assert: assoc key isValidToRemoveFrom: assoc value].

! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MEGL 8/16/2023 00:02:00'!
methodWithDifferentImplementationInSuperClass: methods

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The method(s): ';
			nextPutAll: methods asCommaStringAnd;
			nextPutAll: ' have an implementation not equivalent in the superclass and will be pushed up' ]! !

!PushUpMethod class methodsFor: 'instance variables' stamp: 'MEGL 8/21/2023 15:21:28'!
selectAccessedInstanceVariablesThatCanBePushedUpFrom: aMethodToPushUp

	| accessedInstanceVariables |

	accessedInstanceVariables := self selectAccessedInstanceVariables: aMethodToPushUp.

	accessedInstanceVariables do: [ :variable | PushUpInstanceVariable assertCanMove: variable from: aMethodToPushUp methodClass ].

	^accessedInstanceVariables! !

!PushUpMethod class methodsFor: 'instance variables' stamp: 'MEGL 8/21/2023 15:54:51'!
selectInstanceVariablesToPushUpFrom: methods

	^ (methods collect: 
				[:method | self selectAccessedInstanceVariablesThatCanBePushedUpFrom: method ]
	    ) asArray flatten asSet 
! !

!PushUpMethod class methodsFor: 'implemented superclass' stamp: 'MEGL 8/14/2023 00:04:24'!
checkIfMethodIsImplementedInSuperClass: aMethodToTransfer

		aMethodToTransfer methodClass superclass
		compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superClassMethod | ^(self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superClassMethod) not]
		ifAbsent: [^false]! !

!PushUpMethod class methodsFor: 'equivalent methods' stamp: 'MEGL 8/20/2023 00:36:04'!
assert: aMethod implementsEquivalentMethodIn: aSubclass

	^ aSubclass ~= aMethod methodClass and: [
		aSubclass
			compiledMethodAt: aMethod selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: aMethod ]
			ifAbsent: [ false ]]! !

!PushUpMethod class methodsFor: 'equivalent methods' stamp: 'MEGL 8/21/2023 16:20:37'!
selectSiblingsWithEquivalentMethod: methods

	| withEquivalentMethod |
	
	withEquivalentMethod := Dictionary new.
	
	methods do: [:method | withEquivalentMethod add: method -> 
		(method methodClass superclass subclasses select:
			[ :aSubclass | self assert: method implementsEquivalentMethodIn: aSubclass ]
		)
	].

	^withEquivalentMethod reject: [:assoc | assoc value isEmpty ].
! !

!PushUpMethod class methodsFor: 'dependant methods' stamp: 'MEGL 8/20/2023 20:52:27'!
selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: aMethodToTransfer

	| selectors |

	selectors := Set new.
	self selectDependantSelectorsFrom: aMethodToTransfer using: selectors.
	
	selectors remove: aMethodToTransfer selector.
	
	^selectors! !

!PushUpMethod class methodsFor: 'dependant methods' stamp: 'MEGL 8/21/2023 16:21:04'!
selectDependantSelectorsFrom: aMethod using: visitedSelectors

	| selectors |

	visitedSelectors add: aMethod selector.

	selectors := self selectorsSentToSelf: aMethod.

	(selectors difference: visitedSelectors) do: [:m | self selectDependantSelectorsFrom: (aMethod methodClass >> m) using: visitedSelectors]! !

!PushUpMethod class methodsFor: 'dependant methods' stamp: 'MEGL 8/20/2023 18:41:44'!
selectorsSentToSelf: aMethod

	^ aMethod methodClass selectors select: [ :method | aMethod methodNode sendsMessageToSelf: method]! !


!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/21/2023 16:00:10'!
askMethodsToPushUpFrom: aDependantMethods

	| answer |

	answer := PopUpMenu 
		withCaption: (self withDependantMethodsMessage: aDependantMethods) 
		chooseFrom: #('Only PushUp this method' 'PushUp this method and all the dependant methods').

	answer = 1 ifTrue: [ ^dependantMethods := Set new ].
	answer = 2 ifTrue: [ ^dependantMethods := aDependantMethods ].
	self endRequest ! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/21/2023 15:59:38'!
selectDependantMethods
	|methods|

	methods := ((self refactoringClass selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: methodToTransfer )
	                   collect:  [:method | methodToTransfer methodClass >> method ]) asSet.

	methods ifNotEmpty: [ self askMethodsToPushUpFrom: methods ].! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/21/2023 13:57:32'!
selectSiblingsWithEquivalentMethod
	| methods withEquivalentMethod |
	
	methods := Set withAll: dependantMethods.
	methods add: methodToTransfer.
	
	withEquivalentMethod := self refactoringClass selectSiblingsWithEquivalentMethod: methods.

	withEquivalentMethod ifNotEmpty: [ self askMethodsToRemoveFrom: withEquivalentMethod ].
! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/19/2023 16:39:15'!
withDependantMethodsMessage: aDependantMethods

	^ String streamContents: [ :stream |
		stream
			nextPutAll: methodToTransfer selector;
			nextPutAll: ' has the following dependant method(s): ';
			nextPutAll: (aDependantMethods collect: [:method | method selector ] ) asCommaStringAnd;
			nextPutAll: '. How do you want to proceed?' ]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/20/2023 00:39:08'!
withEquivalentMethodsMessage: withEquivalentMethod

	^ String streamContents: [ :stream |		
		withEquivalentMethod associationsDo: [:assoc |

			stream
				nextPutAll: assoc key selector;
				nextPutAll: ' has equivalent methods declared in ';
				nextPutAll: assoc value asCommaStringAnd;
				nextPutAll: ', ' 
		].

		stream
			nextPutAll: 'how do you want to proceed?' ].! !


!TransferMethod methodsFor: 'applying - private' stamp: 'MEGL 8/20/2023 21:13:54'!
methodCategory

	^self methodCategory: method.! !

!TransferMethod methodsFor: 'applying - private' stamp: 'MEGL 8/20/2023 21:16:07'!
removeMethod

	self removeMethod: method.
 ! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 8/21/2023 15:46:55'!
moveMethod

	self 
		pushUpAccessedVariables;
		pushUpMethods.! !

!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 8/21/2023 15:56:46'!
pushUpAccessedVariables

	|instanceVariablesToPushUp methodClass|
	
	instanceVariablesToPushUp := self class selectInstanceVariablesToPushUpFrom: methods.
	methodClass := methods anyOne methodClass.

	instanceVariablesToPushUp do: [:var | (PushUpInstanceVariable named: var from: methodClass) apply ]! !

!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 8/21/2023 15:46:14'!
removeMethod

	subclassesToRemoveMethodFrom associationsDo:
		[:assoc | assoc value do:
			[:class | class removeSelector: assoc key selector ]
		].! !

!PushUpMethod methodsFor: 'initialization' stamp: 'MEGL 8/21/2023 15:43:06'!
initializeFor: aMethods additionallyRemovingFrom: aSubclassesToRemoveMethodFrom

	methods := aMethods.
	subclassesToRemoveMethodFrom := aSubclassesToRemoveMethodFrom.! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'MEGL 8/19/2023 17:11:17'!
for: aMethodToPushUp

	^self for: aMethodToPushUp additionallyRemovingFrom: #() pushingUp: Set new.! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:19:38'!
assert: subclassesToRemoveMethodFrom areSiblingsOf: aClassToPushUpMethodFrom

	| noSiblings |
	
	noSiblings := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass superclass = aClassToPushUpMethodFrom superclass ].
	
	noSiblings ifNotEmpty: [ self signalNoSiblings: noSiblings ].! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:29:39'!
assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom
	
	| notEquivalent |
	
	notEquivalent := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass >> aMethodToPushUp selector equivalentTo: aMethodToPushUp ].
	
	notEquivalent ifNotEmpty: [ self signalNotEquivalentMethodsFor: notEquivalent ].! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:10:49'!
assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom
	
	| subclassesNotImplementing |
	
	subclassesNotImplementing := subclassesToRemoveMethodFrom reject: [ :aSubclass | 
		aSubclass includesSelector: aMethodToPushUp selector ].
	
	subclassesNotImplementing ifNotEmpty: [
		self signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing ]
	! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MEGL 8/21/2023 15:56:56'!
accessingInstanceVariableWarningMessage: accessedInstanceVariables

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The instance variable(s) ';
			nextPutAll: accessedInstanceVariables asCommaStringAnd;
			nextPutAll: ' will be pushed up' ]! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 11/12/2019 11:38:58'!
errorMessageForMethodCannotAccessInstanceVariable
	
	^ 'Can not push up a method that access an instance variable'! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:14:45'!
errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method because is not implemented in:';
			newLine.

		subclassesNotImplementing asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
		! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:24:10'!
errorMessageForNoSiblings: noSiblings

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method from classes that are not siblings:';
			newLine.

		noSiblings asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:29:24'!
errorMessageForNotEquivalentMethods: notEquivalent

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove not equivalent methods from';
			newLine.

		notEquivalent asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'MSC 10/19/2019 23:29:03'!
signalMethodCannotAccessInstanceVariable
	
	self refactoringError: self errorMessageForMethodCannotAccessInstanceVariable! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:11:17'!
signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing
	
	self refactoringError: (self errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing)! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:24:10'!
signalNoSiblings: noSiblings 

	self refactoringError: (self errorMessageForNoSiblings: noSiblings)! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:29:39'!
signalNotEquivalentMethodsFor: notEquivalent

	self refactoringError: (self errorMessageForNotEquivalentMethods: notEquivalent)! !

!PushUpMethod class methodsFor: 'instance variables' stamp: 'MEGL 7/12/2023 16:49:35'!
selectAccessedInstanceVariables: aMethodToPushUp

	^aMethodToPushUp methodClass instVarNames
		select: [ :instVarName |  aMethodToPushUp accessesInstanceVariable: instVarName ]! !


!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'MEGL 8/21/2023 16:00:33'!
createRefactoring

	^self refactoringClass for: methodToTransfer additionallyRemovingFrom: subclassesToRemoveMethodsFrom pushingUp: dependantMethods! !

!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'MSC 10/14/2019 19:36:48'!
refactoringClass

	^PushUpMethod! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/21/2023 16:00:22'!
askMethodsToRemoveFrom: withEquivalentMethod

	| answer |
	
	answer := PopUpMenu 
		withCaption: (self withEquivalentMethodsMessage: withEquivalentMethod) 
		chooseFrom: #('Only PushUp' 'PushUp and delete all equivalent methods').

	answer = 1 ifTrue: [ ^subclassesToRemoveMethodsFrom := Dictionary new ].
	answer = 2 ifTrue: [ ^subclassesToRemoveMethodsFrom := withEquivalentMethod ].
	self endRequest ! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 8/14/2023 22:49:08'!
requestRefactoringParameters

	self selectDependantMethods;
		selectSiblingsWithEquivalentMethod
		! !

!PushUpMethodApplier methodsFor: 'initialization' stamp: 'MEGL 7/30/2023 21:10:14'!
initialize 

	subclassesToRemoveMethodsFrom := #().
	dependantMethods := #()! !


!ParseNode reorganize!
('code generation' pc pc:)
('code generation (closures)' optimizedBlockHoistTempsInto:)
('code generation (new scheme)' emitCodeForBlockValue:encoder: emitCodeForBranchOn:dist:pop:encoder: emitCodeForEffect:encoder: emitCodeForJump:encoder: emitCodeForReturn:encoder: sizeCode:forBranchOn:dist: sizeCode:forJump: sizeCodeForBlockValue: sizeCodeForEffect: sizeCodeForReturn:)
('converting' asReturnNode)
('encoding' encodeSelector:)
('printing' nodePrintOn:indent: printCommentOn:indent: printOn: printOn:indent: printOn:indent:precedence: printSourceCode printWithClosureAnalysis printWithClosureAnalysisOn: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('source ranges' expandRange:basedOn: expandRange:basedOnChildRange: expandRanges:basedOn:using:)
('testing' assignmentCheck:at: canCascade ensureCanCascade: equivalentTo: isArg isAssignmentNode isAssignmentToTemporary isBacktickNode isBlockNode isBraceNode isCascadeNode isComplex isConstantCharacter isConstantNumber isFalsePseudoVariable isInstanceVariableNode isJust: isLiteralNode isLiteralVariableNode isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isMethodNode isNilPseudoVariable isOnlySubnodeOf:in: isReturn isReturnSelf isReturningIf isSelectorNode isSelfBasicNewMessageSend isSelfNewMessageSend isSelfPseudoVariable isSpecialConstant isSuperPseudoVariable isTemp isTempOrArg isTemporariesDeclaration isTemporaryDeclaration isThisContextPseudoVariable isTruePseudoVariable isUndefTemp isUnusedTemp isVariableNode isVariableReference nowHasDef nowHasRef printsInNewLine referencesSelf referencesSuper sendsMessageToSelf: toDoIncrement:)
('tiles' currentValueIn:)
('visiting' accept: nodesDo:)
('private' consolidateAsCollection: ifNilReceiver nextWordFrom:setCharacter: printSingleComment:on:indent:)
('accessing' addComment: comment comment: receiverOrCascadeReceiver)
!


!BlockNode reorganize!
('accessing' addArgument: arguments arguments: block closureCreationNode firstArgument nArgsSlot nArgsSlot: numberOfArguments optimized returnLast returnNilIfNoOther returnSelfIfNoOther: startOfLastStatement startOfLastStatement: temporaries temporaries: temporariesDeclaration temporariesDeclaration: tempsMark tempsMark:)
('code generation' code)
('code generation (closures)' actualScope addHoistedTemps: addRemoteTemp:rootNode: addTempNode: analyseArguments:temporaries:rootNode: analyseTempsWithin:rootNode:assignmentPools: blockExtent computeCopiedValues: constructClosureCreationNode: deoptimize emitCodeForClosureValue:encoder: emitCodeForEvaluatedClosureValue:encoder: ifHasRemoteTempNodeEnsureInitializationStatementExists: makeTemporariesRemovable nilReadBeforeWrittenTemps noteOptimizedIn: optimizedBlockHoistTempsInto: postNumberingProcessTempsWithin:rootNode: reindexingLocalsDo:encoder: remoteTempNodeName removeTempNode:ifAbsent: sizeCodeForClosureValue: sizeCodeForEvaluatedClosureValue:)
('code generation (new scheme)' emitCodeExceptLast:encoder: emitCodeForEvaluatedEffect:encoder: emitCodeForEvaluatedValue:encoder: emitCodeForValue:encoder: sizeCodeExceptLast: sizeCodeForEvaluatedEffect: sizeCodeForEvaluatedValue: sizeCodeForValue:)
('enumerating' statementsDo:)
('equation translation' statements statements:)
('initialization' arguments:statements:returns:from: noteSourceRangeStart:end:encoder: statements:returns:)
('printing' decompileString printArgumentsOn:indent: printOn:indent: printStatementsOn:indent: printTemporaries:on:doPrior: printWithClosureAnalysisArgumentsOn:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisStatementsOn:indent: printWithClosureAnalysisTemporariesOn:indent:)
('source ranges' expandRanges:basedOn:using:)
('testing' equivalentTo: hasArgumentOrTemporaryNamed: isBlockNode isComplex isJust: isJustCaseError isQuick printsInNewLine referencesSelf referencesSuper returns sendsMessageToSelf:)
('visiting' accept:)
('private' hasEquivalentStatementsWith:)
!


!BlockNode class reorganize!
('instance creation' empty statements:returns: withJust:)
!


!MessageNode reorganize!
('visiting' accept: argumentsInEvaluationOrder)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('equation translation' arguments arguments: eval originalArguments originalReceiver receiver receiver: receiverOrCascadeReceiver selector)
('printing' macroPrinter precedence printCaseOn:indent: printIfNil:indent: printIfNilNotNil:indent: printIfOn:indent: printKeywords:arguments:on:indent: printOn:indent: printOn:indent:precedence: printParenReceiver:on:indent: printReceiver:on:indent: printRepeatOn:indent: printToDoOn:indent: printWhileOn:indent: printWithClosureAnalysisCaseOn:indent: printWithClosureAnalysisIfNil:indent: printWithClosureAnalysisIfNilNotNil:indent: printWithClosureAnalysisIfOn:indent: printWithClosureAnalysisKeywords:arguments:on:indent: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence: printWithClosureAnalysisParenReceiver:on:indent: printWithClosureAnalysisReceiver:on:indent: printWithClosureAnalysisToDoOn:indent: printWithClosureAnalysisWhileOn:indent:)
('source ranges' expandRanges:basedOn:using: keywordAndParameterPositionAt:encodedWith:ifAbsent: keywordPositionAt: keywordRanges receiverSourceRangesFrom:)
('testing' canCascade ensureCanCascade: equivalentTo: isCascade isComplex isInfix isKeywordMessageSend isMessage isMessage:receiver:arguments: isMessageNamed: isMessageNode isNilIf isOptimized isOptimizedLoop isReturningIf isSelfBasicNewMessageSend isSelfNewMessageSend isUnaryMessageSend referencesSelf referencesSuper sendsMessageToSelf: toDoIncrement: toDoLimit:)
('cascading' receiver:arguments:precedence:)
('private' checkBlock:as:from:maxArgs: compare:with: hasEquivalentArgumentsWith: hasEquivalentReceiverWith: ifNilReceiver pvtCheckForPvtSelector: transform:)
('code generation' emitCodeForEffect:encoder: emitCodeForRepeat:encoder:value: emitCodeForToDo:encoder:value: emitCodeForValue:encoder: sizeCodeForCase:value: sizeCodeForEffect: sizeCodeForRepeat:value: sizeCodeForToDo:value: sizeCodeForValue: sizeCodeForWhile:value:)
('code generation (new scheme)' emitCodeForCase:encoder:value: emitCodeForIf:encoder:value: emitCodeForIfNil:encoder:value: emitCodeForWhile:encoder:value: sizeCodeForIf:value: sizeCodeForIfNil:value:)
('macro transformations' noteSpecialSelector: toDoFromWhileWithInit: transformAnd: transformBoolean: transformCase: transformIfFalse: transformIfFalseIfTrue: transformIfNil: transformIfNilIfNotNil: transformIfNotNilIfNil: transformIfTrue: transformIfTrueIfFalse: transformOr: transformRepeat: transformToDo: transformWhile:)
('initialization' optimizeSpecialSends: receiver:selector:arguments:precedence: receiver:selector:arguments:precedence:from: receiver:selector:arguments:precedence:from:sourceRange:keywordsRanges: selector:)
('accessing' selectorSymbol)
('initialize-release' receiver:selector:arguments:precedence:from:sourceRange:)
('expression types' cascadeReceiver)
!

!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
TransferMethod subclass: #PushUpMethod
	instanceVariableNames: 'subclassesToRemoveMethodFrom methods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom: for:additionallyRemovingFrom:pushingUp:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn: assert:isValidToRemoveFrom: assertIsValidToPushUp: assertIsValidToRemove:)
('warnings' accessingInstanceVariableWarningMessage: methodWithDifferentImplementationInSuperClass: warnIfMethodAccessInstanceVariable:)
('exceptions' errorMessageForMethodCannotAccessInstanceVariable errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotAccessInstanceVariable signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
('instance variables' selectAccessedInstanceVariables: selectAccessedInstanceVariablesThatCanBePushedUpFrom: selectInstanceVariablesToPushUpFrom:)
('implemented superclass' checkIfMethodIsImplementedInSuperClass:)
('equivalent methods' assert:implementsEquivalentMethodIn: selectSiblingsWithEquivalentMethod:)
('dependant methods' selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: selectDependantSelectorsFrom:using: selectorsSentToSelf:)
!

!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
TransferMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: 'subclassesToRemoveMethodsFrom dependantMethods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
