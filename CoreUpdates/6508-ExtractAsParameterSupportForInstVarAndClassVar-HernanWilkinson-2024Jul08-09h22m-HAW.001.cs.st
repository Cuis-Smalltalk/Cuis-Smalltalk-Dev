'From Cuis6.3 [latest update: #6350] on 8 July 2024 at 12:42:26 pm'!

!LiteralVariableNode methodsFor: 'testing' stamp: 'HAW 7/8/2024 10:59:20'!
isReferencingClassVariable
	
	"If it is not a global, I assume it is a class var"
	^self isReferencingGlobal not and: [ self isUndeclared not ]! !

!LiteralVariableNode methodsFor: 'testing' stamp: 'HAW 7/8/2024 11:04:22'!
isUndeclared
	
	^Undeclared includesKey: name ! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 11:57:38'!
assertHasExtractableBraceElements: aBraceNode asPartOf: methodNode at: trimmedIntervalToReplace   
	
	aBraceNode elementsDo: [ :anElementNode | self assertIsValidToExtract: anElementNode asPartOf: methodNode at: trimmedIntervalToReplace ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:18:11'!
assertIsNotLeftAssigment: aNode asPartOf: methodNode at: intervalToReplace  
	
	methodNode nodesDo: [ :traversedNode | 
		(self isAssignment: traversedNode and: aNode leftPartIn: methodNode at: intervalToReplace)
			ifTrue: [ self signalInvalidExpressionToExtractAsParameter ]]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:00:51'!
assertIsValidAssigmentToExtract: anAssignmentNode asPartOf: methodNode at: intervalToReplace   
	
	self assertIsValidToExtract: anAssignmentNode variable asPartOf: methodNode at: intervalToReplace.
	self assertIsValidToExtract: anAssignmentNode value asPartOf: methodNode at: intervalToReplace ! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:00:39'!
assertIsValidBlockNodeToExtract: aBlockNode asPartOf: methodNode at: intervalToReplace   
	
	aBlockNode block statementsDo: [ :aStatement | self assertIsValidToExtract: aStatement asPartOf: methodNode at: intervalToReplace ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:00:18'!
assertIsValidLiteralNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace  

	aNode isBraceNode ifTrue: [ ^self assertHasExtractableBraceElements: aNode asPartOf: methodNode at: intervalToReplace].

	^ (aNode isLiteralNode 
		or: [ aNode isTruePseudoVariable
		or: [ aNode isFalsePseudoVariable 
		or: [ aNode isNilPseudoVariable 
		or: [ (aNode isLiteralVariableNode and: [ 
				aNode isReferencingBehavior 
					or: [ aNode isReferencingGlobal
					or: [ aNode isReferencingClassVariable ]]])]]]]) ifFalse: [ self signalInvalidExpressionToExtractAsParameter ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:00:27'!
assertIsValidMessageNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace  

	self assertIsValidToExtract: aNode receiver asPartOf: methodNode at: intervalToReplace.
	aNode arguments do: [ :anArgument | self assertIsValidToExtract: anArgument asPartOf: methodNode at: intervalToReplace ]! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:14:47'!
assertIsValidToExtract: aNode asPartOf: methodNode at: intervalToReplace  

	aNode isMessageNode ifTrue: [ ^self assertIsValidMessageNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace].
	aNode isBlockNode ifTrue: [ ^self assertIsValidBlockNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace ].
	aNode isTempOrArg ifTrue: [  ^self assertIsValidTempOrArgNodeToExtract: aNode ].
	aNode isAssignmentToTemporary ifTrue: [ ^self assertIsValidAssigmentToExtract: aNode asPartOf: methodNode at: intervalToReplace ].
	aNode isInstanceVariableNode ifTrue: [ ^self assertIsNotLeftAssigment: aNode asPartOf: methodNode at: intervalToReplace ].
	self assertIsValidLiteralNodeToExtract: aNode asPartOf: methodNode at: intervalToReplace ! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 12:19:25'!
isAssignment: aPotentialAssigmentNode and: aPotentialLeftAssigment leftPartIn: methodNode at: interval

	^ aPotentialAssigmentNode isAssignmentNode 
		and: [ aPotentialLeftAssigment = aPotentialAssigmentNode variable 
		and: [ | assigmentRange |
			assigmentRange := (methodNode completeSourceRangesOf: aPotentialAssigmentNode ifAbsent: [Set with: (0 to: 0)]) anyOne.
			assigmentRange includesAllOf: interval ]]! !


!AssignmentNode methodsFor: 'testing' stamp: 'HAW 7/8/2024 11:31:47'!
isAssignmentToTemporary

	^ variable isTemp ! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 7/8/2024 11:55:21'!
assertCanExtractFrom: anInterval for: sourceCode methodNode: methodNode last: lastIntervalsAndNode

	| trimmedIntervalToReplace node intervalToReplace |

	ExtractMethodNewMethod assertIntervalToExtractIsNotEmpty: anInterval.
	ExtractMethodNewMethod assert: anInterval isWithinBoundsOf: sourceCode.
	
	intervalToReplace := self removeDotsAt: anInterval in: sourceCode.
	trimmedIntervalToReplace := intervalToReplace asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	node := self nodeToExtractFrom: methodNode at: trimmedIntervalToReplace or: intervalToReplace.
	self assertIsValidToExtract: node asPartOf: methodNode at: trimmedIntervalToReplace.
	lastIntervalsAndNode ifNotNil: [ self assertIsSameExpressionToExtractFrom: node to: lastIntervalsAndNode third ].
	
	^{ intervalToReplace. trimmedIntervalToReplace. node }! !

!methodRemoval: ExtractAsParameter class #assertIsValidBlockNodeToExtract: stamp: 'HAW 7/8/2024 11:45:26'!
ExtractAsParameter class removeSelector: #assertIsValidBlockNodeToExtract:!
!methodRemoval: ExtractAsParameter class #assertIsValidMessageNodeToExtract: stamp: 'HAW 7/8/2024 11:45:43'!
ExtractAsParameter class removeSelector: #assertIsValidMessageNodeToExtract:!
!methodRemoval: ExtractAsParameter class #assertHasExtractableBraceElements: stamp: 'HAW 7/8/2024 11:44:13'!
ExtractAsParameter class removeSelector: #assertHasExtractableBraceElements:!
!methodRemoval: ExtractAsParameter class #assertIsValidLiteralNodeToExtract: stamp: 'HAW 7/8/2024 11:56:45'!
ExtractAsParameter class removeSelector: #assertIsValidLiteralNodeToExtract:!
!methodRemoval: ExtractAsParameter class #assertIsValidAssigmentToExtract: stamp: 'HAW 7/8/2024 11:45:07'!
ExtractAsParameter class removeSelector: #assertIsValidAssigmentToExtract:!
!methodRemoval: ExtractAsParameter class #assertIsValidToExtract: stamp: 'HAW 7/8/2024 11:43:24'!
ExtractAsParameter class removeSelector: #assertIsValidToExtract:!
!methodRemoval: ExtractAsParameter class #assertIsValidMessageNodeToExtract:asPartOf: stamp: 'HAW 7/8/2024 11:58:46'!
ExtractAsParameter class removeSelector: #assertIsValidMessageNodeToExtract:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertIsValidBlockNodeToExtract:asPartOf: stamp: 'HAW 7/8/2024 11:58:03'!
ExtractAsParameter class removeSelector: #assertIsValidBlockNodeToExtract:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertIsNotLeftAssigment:asPartOf: stamp: 'HAW 7/8/2024 12:01:11'!
ExtractAsParameter class removeSelector: #assertIsNotLeftAssigment:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertIsValidLiteralNodeToExtract:asPartOf: stamp: 'HAW 7/8/2024 11:59:49'!
ExtractAsParameter class removeSelector: #assertIsValidLiteralNodeToExtract:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertHasExtractableBraceElements:asPartOf: stamp: 'HAW 7/8/2024 11:57:38'!
ExtractAsParameter class removeSelector: #assertHasExtractableBraceElements:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertIsValidAssigmentToExtract:asPartOf: stamp: 'HAW 7/8/2024 11:57:51'!
ExtractAsParameter class removeSelector: #assertIsValidAssigmentToExtract:asPartOf:!
!methodRemoval: ExtractAsParameter class #assertIsValidToExtract:asPartOf: stamp: 'HAW 7/8/2024 11:55:21'!
ExtractAsParameter class removeSelector: #assertIsValidToExtract:asPartOf:!
!methodRemoval: ParseNode #isAssignmentToInstanceVariable stamp: 'HAW 7/8/2024 11:46:12'!
ParseNode removeSelector: #isAssignmentToInstanceVariable!

!ExtractAsParameter class reorganize!
('pre-conditions' assert:isInImplementors: assertAndCreateNamed:extractedFromAll:at:implementors:senders:creatingAddParameterWith: assertCanExtractFrom:for:methodNode:last: assertCanExtractedFromAll:for: assertHasExtractableBraceElements:asPartOf:at: assertIsNotLeftAssigment:asPartOf:at: assertIsSameExpressionToExtractFrom:to: assertIsValidAssigmentToExtract:asPartOf:at: assertIsValidBlockNodeToExtract:asPartOf:at: assertIsValidKeywordForNewParameter: assertIsValidLiteralNodeToExtract:asPartOf:at: assertIsValidMessageNodeToExtract:asPartOf:at: assertIsValidParameterName: assertIsValidTempOrArgNodeToExtract: assertIsValidToExtract:asPartOf:at: is:equalTo:or: isAssignment:and:leftPartIn:at: nodeToExtractFrom:at:or: removeDotsAt:in:)
('instance creation' addParameterClass named:extractedFrom:at:implementors:senders: named:extractedFrom:at:newKeyword:at:implementors:senders: named:extractedFromAll:at:implementors:senders: named:extractedFromAll:at:newKeyword:at:implementors:senders:)
('error messages' errorMessageForInvalidExpressionToExtractAsParameter errorMessageForInvalidSelection errorMessageForNoExpressionToExtract errorMessageForOrigialMethodMustBeInImplementorsToChange errorMessageNotAllExpressionsToExtractAreEqual)
('exceptions' signalInvalidExpressionToExtractAsParameter signalInvalidSelection signalNoExpressionToExtract signalNotAllExpressionsToExtractAreEqual signalOrigialMethodMustBeInImplementorsToChange)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inSystem:)
('intervals' intervalsForEquivalentExpressionIn:at: intervalsForEquivalentNodesTo:in:)
!

