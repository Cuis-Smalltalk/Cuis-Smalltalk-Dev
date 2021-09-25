'From Cuis 5.0 [latest update: #4879] on 24 September 2021 at 8:40:55 pm'!
!classDefinition: #ExtractAsParameter category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractAsParameter
	instanceVariableNames: 'intervalToExtract sourceMethod addParameter intervalToReplace intervals '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractAsParameterApplier category: #'Tools-Refactoring'!
AddParameterApplier subclass: #ExtractAsParameterApplier
	instanceVariableNames: 'interval intervals '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractAsParameter methodsFor: 'applying - private' stamp: 'HAW 9/24/2021 17:52:51'!
newSourceCode: anIntervalToReplace from: sourceCode originalSize: originalSourceCodeSize
	
	| newInterval newSourceCode |
	
	newInterval := anIntervalToReplace  + (sourceCode size - originalSourceCodeSize).
	newSourceCode := sourceCode copyReplaceFrom: newInterval first to: newInterval last with: addParameter newParameter.
	
	^newSourceCode ! !

!ExtractAsParameter methodsFor: 'initialization' stamp: 'HAW 9/24/2021 17:31:51'!
initializeReplacingAll: allIntervals at: aSourceMethod addingParameterWith: anAddParameter 
	
	intervals := allIntervals.
	sourceMethod := aSourceMethod.
	addParameter := anAddParameter.! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/24/2021 20:18:37'!
assertAndCreateNamed: aNewParameter 
	extractedFromAll: allIntervals
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection
	creatingAddParameterWith: addParameterCreator

	| addParameter sourceCodeToExtract trimmedIntervalToReplace intervals |
	
	self assert: aSourceMethod isInImplementors: implementorsCollection.
	intervals := self assertCanExtractedFromAll: allIntervals for: aSourceMethod.
	
	trimmedIntervalToReplace := intervals first second.
	sourceCodeToExtract := aSourceMethod sourceCode copyFrom: trimmedIntervalToReplace first to: trimmedIntervalToReplace last.
	
	addParameter := addParameterCreator value: sourceCodeToExtract.

	^self new 
		initializeReplacingAll: (intervals collect: [ :both | both first ])
		at: aSourceMethod 
		addingParameterWith: addParameter! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/24/2021 18:07:32'!
assertCanExtractFrom: anInterval for: sourceCode methodNode: methodNode last: lastIntervalsAndNode

	| trimmedIntervalToReplace node intervalToReplace |

	ExtractMethodNewMethod assertIntervalToExtractIsNotEmpty: anInterval.
	ExtractMethodNewMethod assert: anInterval isWithinBoundsOf: sourceCode.
	
	intervalToReplace := self removeDotsAt: anInterval in: sourceCode.
	trimmedIntervalToReplace := intervalToReplace asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: intervalToReplace.
	self assertIsValidToExtract: node.
	lastIntervalsAndNode ifNotNil: [ self assertIsSameExpressionToExtractFrom: node to: lastIntervalsAndNode third ].
	
	^{ intervalToReplace. trimmedIntervalToReplace. node }! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/24/2021 18:17:03'!
assertCanExtractedFromAll: allIntervals for: aSourceMethod  

	| sourceCode methodNode lastIntervalsAndNode |
	
	allIntervals isEmpty ifTrue: [ self signalNoExpressionToExtract ].
	
	sourceCode := aSourceMethod sourceCode.
	methodNode := aSourceMethod methodNode.
	lastIntervalsAndNode := nil.
	
	^allIntervals collect: [ :anInterval | 
		lastIntervalsAndNode := self 
			assertCanExtractFrom: anInterval 
			for: sourceCode 
			methodNode: methodNode 
			last: lastIntervalsAndNode ]	! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/24/2021 20:19:16'!
assertIsSameExpressionToExtractFrom: node to: lastNode

	(node = lastNode or: [ node equivalentTo: lastNode ]) ifFalse: [
		self signalNotAllExpressionsToExtractAreEqual ]! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/24/2021 17:23:26'!
named: aNewParameter 
	extractedFromAll: intervals
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFromAll: intervals
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			AddParameter 
				named: aNewParameter 
				initializedWith: sourceCodeToExtract 
				toUnarySelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/24/2021 17:23:26'!
named: aNewParameter 
	extractedFromAll: intervals
	at: aNewKeywordIndex
	newKeyword: newKeyword
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		assertAndCreateNamed: aNewParameter 
		extractedFromAll: intervals
		at: aSourceMethod  
		implementors: implementorsCollection 
		senders: sendersCollection
		creatingAddParameterWith: [ :sourceCodeToExtract | 
			AddParameter 
				named: aNewParameter 
				at: aNewKeywordIndex
				initializedWith: sourceCodeToExtract 
				using: newKeyword 
				toKeywordSelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'error messages' stamp: 'HAW 9/24/2021 18:15:58'!
errorMessageForNoExpressionToExtract
	
	^'No expression to extract'! !

!ExtractAsParameter class methodsFor: 'error messages' stamp: 'HAW 9/24/2021 17:58:54'!
errorMessageNotAllExpressionsToExtractAreEqual
	
	^'Expressions to extract are not equal'! !

!ExtractAsParameter class methodsFor: 'exceptions' stamp: 'HAW 9/24/2021 18:17:30'!
signalNoExpressionToExtract
	
	self refactoringError: self errorMessageForNoExpressionToExtract ! !

!ExtractAsParameter class methodsFor: 'exceptions' stamp: 'HAW 9/24/2021 18:07:12'!
signalNotAllExpressionsToExtractAreEqual
	
	self refactoringError: self errorMessageNotAllExpressionsToExtractAreEqual! !

!ExtractAsParameter class methodsFor: 'intervals' stamp: 'HAW 9/24/2021 20:23:08'!
intervalsForEquivalentExpressionIn: method at: interval

	| methodNode node sourceCode trimmedIntervalToReplace rangeOrRanges |
	
	sourceCode := method sourceCode. 
	methodNode := method methodNode.
	trimmedIntervalToReplace := interval asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: interval.
	
	rangeOrRanges := methodNode rangeForNode: node ifAbsent: [ self shouldNotHappenBecause: 'node already exist' ].	
	^(methodNode isMultipleRanges: rangeOrRanges) 
		ifTrue: [ rangeOrRanges ]
		ifFalse: [ self intervalsForEquivalentNodesTo: node in: methodNode ]
				! !

!ExtractAsParameter class methodsFor: 'intervals' stamp: 'HAW 9/24/2021 20:24:05'!
intervalsForEquivalentNodesTo: node in: methodNode

	| completeSourceRanges intervalsForEquivalentNodes | 
	
	intervalsForEquivalentNodes := OrderedCollection new.
	completeSourceRanges := methodNode completeSourceRanges.
	
	methodNode nodesDo: [ :aNode | 
		(aNode equivalentTo: node) ifTrue: [ 
			"There can not be more than one range because of the is not a multi range node. See senders - Hernan"
			intervalsForEquivalentNodes add: (completeSourceRanges at: aNode) first ]].
	
	^intervalsForEquivalentNodes! !


!ExtractAsParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/24/2021 18:51:07'!
askToReplaceAll
	
	| extractAll |
	
	extractAll := PopUpMenu 
		confirm: 'Do you want to extract all ocurrences?' 
		trueChoice: 'Yes, extract all ocurrences' 
		falseChoice: 'No, extract only the selected one'.
		
	extractAll ifFalse: [ intervals := { interval } ].! !

!ExtractAsParameterApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/24/2021 18:47:00'!
requestRefactoringParameters

	intervals size > 1 ifTrue: [ self askToReplaceAll ].
	
	super requestRefactoringParameters.! !

!ExtractAsParameterApplier methodsFor: 'initialization' stamp: 'HAW 9/24/2021 20:22:13'!
initializeIntervals
	
	intervals := self refactoringClass intervalsForEquivalentExpressionIn: selectedClass >> oldSelector at: interval.! !


!CodeNode methodsFor: 'private' stamp: 'HAW 9/24/2021 20:01:42'!
hasEquivalentTemporariesDeclarationWith: aCodeNode

	(self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration isNil ]) ifTrue: [ ^true ].
	(self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration notNil ]) ifTrue: [ ^false ].
	(self temporariesDeclaration notNil and: [ aCodeNode temporariesDeclaration isNil ]) ifTrue: [ ^false ].
	
	^self temporariesDeclaration equivalentTo: aCodeNode temporariesDeclaration ! !


!ExtractAsParameter methodsFor: 'applying - private' stamp: 'HAW 9/24/2021 17:54:19'!
useNewParameter
	
	| newSourceCodeWithAllReplacements originalSourceCodeSize |
	
	originalSourceCodeSize := sourceMethod sourceCode size.
	newSourceCodeWithAllReplacements := intervals 
		inject: (sourceMethod methodClass >> self newSelector) sourceCode
		into: [ :newSourceCode :intervalToReplace | self newSourceCode: intervalToReplace from: newSourceCode originalSize: originalSourceCodeSize ].
		
	sourceMethod methodClass compile: newSourceCodeWithAllReplacements! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 9/24/2021 17:42:58'!
nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: intervalToReplace
	
	| nodeWithRangeToExtract nodesWithFirstPosition |
	
	nodesWithFirstPosition := methodNode parseNodesPathAt: trimmedIntervalToReplace first ifAbsent: [ self signalInvalidSelection ].
	nodeWithRangeToExtract := nodesWithFirstPosition 
		detect: [ :nodeAndRange | self is: nodeAndRange value equalTo: trimmedIntervalToReplace or: intervalToReplace ]
		ifNone: [ self signalInvalidSelection ].
	 
	^nodeWithRangeToExtract key.
	! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/24/2021 17:22:20'!
named: aNewParameter 
	extractedFrom: anInterval
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		extractedFromAll: { anInterval }
		at: aSourceMethod 
		implementors: implementorsCollection 
		senders: sendersCollection ! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 9/24/2021 17:24:05'!
named: aNewParameter 
	extractedFrom: anInterval
	at: aNewKeywordIndex
	newKeyword: newKeyword
	at: aSourceMethod  
	implementors: implementorsCollection 
	senders: sendersCollection

	^self 
		named: aNewParameter 
		extractedFromAll: { anInterval }
		at: aNewKeywordIndex 
		newKeyword: newKeyword 
		at: aSourceMethod 
		implementors: implementorsCollection 
		senders: sendersCollection ! !


!ExtractAsParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/24/2021 18:51:16'!
createRefactoringForKeywordSelector
	
	^self refactoringClass 
		named: newParameter 
		extractedFromAll:  intervals
		at:  parameterIndex 
		newKeyword:  newKeyword 
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders ! !

!ExtractAsParameterApplier methodsFor: 'refactoring - creation' stamp: 'HAW 9/24/2021 18:51:25'!
createRefactoringForUnarySelector
	
	^self refactoringClass 
		named: newParameter 
		extractedFromAll: intervals
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders ! !

!ExtractAsParameterApplier methodsFor: 'initialization' stamp: 'HAW 9/24/2021 18:27:27'!
initializeInterval: anInterval 
	
	interval := anInterval.
	
	self initializeIntervals.! !


!ExtractAsParameterApplier class methodsFor: 'instance creation' stamp: 'HAW 9/24/2021 17:40:01'!
from: anInterval on: aModel for: anOldSelector in: aClassToRefactor 

	ExtractAsParameter assertCanExtractedFromAll: { anInterval } for: aClassToRefactor >> anOldSelector.
	
	^(super on: aModel for: anOldSelector in: aClassToRefactor) initializeInterval: anInterval ! !

!methodRemoval: ExtractAsParameterApplier #intervalsForEquivalentNodesTo:in: stamp: 'HAW 9/24/2021 20:24:32'!
ExtractAsParameterApplier removeSelector: #intervalsForEquivalentNodesTo:in:!
!methodRemoval: ExtractAsParameterApplier #initializeIntervalsLookingForSameExpressionAt:to: stamp: 'HAW 9/24/2021 20:07:44'!
ExtractAsParameterApplier removeSelector: #initializeIntervalsLookingForSameExpressionAt:to:!
!methodRemoval: ExtractAsParameter class #assertCanExtractedFrom:for: stamp: 'HAW 9/24/2021 17:39:50'!
ExtractAsParameter class removeSelector: #assertCanExtractedFrom:for:!
!methodRemoval: ExtractAsParameter class #assertAndCreateNamed:extractedFrom:at:implementors:senders:creatingAddParameterWith: stamp: 'HAW 9/24/2021 17:23:26'!
ExtractAsParameter class removeSelector: #assertAndCreateNamed:extractedFrom:at:implementors:senders:creatingAddParameterWith:!
!methodRemoval: ExtractAsParameter class #assertCanExtractFrom:for:methodNode: stamp: 'HAW 9/24/2021 18:07:22'!
ExtractAsParameter class removeSelector: #assertCanExtractFrom:for:methodNode:!
!methodRemoval: ExtractAsParameter #newSourceCode: stamp: 'HAW 9/24/2021 17:57:52'!
ExtractAsParameter removeSelector: #newSourceCode:!
!methodRemoval: ExtractAsParameter #initializeExtractedFrom:replacingAll:at:addingParameterWith: stamp: 'HAW 9/24/2021 17:31:58'!
ExtractAsParameter removeSelector: #initializeExtractedFrom:replacingAll:at:addingParameterWith:!
!methodRemoval: ExtractAsParameter #newSourceCode stamp: 'HAW 9/24/2021 17:33:54'!
ExtractAsParameter removeSelector: #newSourceCode!
!methodRemoval: ExtractAsParameter #initializeExtractedFrom:replacing:at:addingParameterWith: stamp: 'HAW 9/24/2021 17:30:26'!
ExtractAsParameter removeSelector: #initializeExtractedFrom:replacing:at:addingParameterWith:!
!classDefinition: #ExtractAsParameter category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractAsParameter
	instanceVariableNames: 'sourceMethod addParameter intervals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractAsParameter class reorganize!
('pre-conditions' assert:isInImplementors: assertAndCreateNamed:extractedFromAll:at:implementors:senders:creatingAddParameterWith: assertCanExtractFrom:for:methodNode:last: assertCanExtractedFromAll:for: assertIsSameExpressionToExtractFrom:to: assertIsValidAssigmentToExtract: assertIsValidBlockNodeToExtract: assertIsValidKeywordForNewParameter: assertIsValidLiteralNodeToExtract: assertIsValidMessageNodeToExtract: assertIsValidParameterName: assertIsValidTempOrArgNodeToExtract: assertIsValidToExtract: is:equalTo:or: nodeToExtractFrom:at:or: removeDotsAt:in:)
('instance creation' named:extractedFrom:at:implementors:senders: named:extractedFrom:at:newKeyword:at:implementors:senders: named:extractedFromAll:at:implementors:senders: named:extractedFromAll:at:newKeyword:at:implementors:senders:)
('error messages' errorMessageForInvalidExpressionToExtractAsParameter errorMessageForInvalidSelection errorMessageForNoExpressionToExtract errorMessageForOrigialMethodMustBeInImplementorsToChange errorMessageNotAllExpressionsToExtractAreEqual)
('exceptions' signalInvalidExpressionToExtractAsParameter signalInvalidSelection signalNoExpressionToExtract signalNotAllExpressionsToExtractAreEqual signalOrigialMethodMustBeInImplementorsToChange)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inSystem:)
('intervals' intervalsForEquivalentExpressionIn:at: intervalsForEquivalentNodesTo:in:)
!

!classDefinition: #ExtractAsParameterApplier category: #'Tools-Refactoring'!
AddParameterApplier subclass: #ExtractAsParameterApplier
	instanceVariableNames: 'interval intervals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractAsParameterApplier reorganize!
('refactoring - parameters request' askNewParameterValue askToReplaceAll requestRefactoringParameters)
('refactoring - creation' createRefactoringForKeywordSelector createRefactoringForUnarySelector refactoringClass)
('initialization' initializeInterval: initializeIntervals)
!


!ExtractAsParameterApplier class reorganize!
('instance creation' from:on:for:in:)
!

