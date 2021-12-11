'From Cuis 5.0 [latest update: #4993] on 6 December 2021 at 12:11:43 pm'!

!LiteralVariableNode methodsFor: 'testing' stamp: 'HAW 12/6/2021 12:03:38'!
isReferencingGlobal
	
	^(Smalltalk bindingOf: key name) notNil  ! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 12/6/2021 12:08:17'!
assertHasExtractableBraceElements: aBraceNode 
	
	aBraceNode elementsDo: [ :anElementNode | self assertIsValidToExtract: anElementNode ]! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 12/6/2021 12:01:58'!
assertIsValidLiteralNodeToExtract: aNode

	aNode isBraceNode ifTrue: [ ^self assertHasExtractableBraceElements: aNode].

	^ (aNode isLiteralNode 
		or: [ aNode isTruePseudoVariable
		or: [ aNode isFalsePseudoVariable 
		or: [ aNode isNilPseudoVariable 
		or: [ (aNode isLiteralVariableNode and: [ 
				aNode isReferencingBehavior or: [ aNode isReferencingGlobal ]])]]]]) ifFalse: [ self signalInvalidExpressionToExtractAsParameter ]! !


!LiteralVariableNode reorganize!
('visiting' accept:)
('code generation' emitCodeForLoad:forValue:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForLoad:forValue: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('testing' assignmentCheck:at: isLiteralVariableNode isReferencingBehavior isReferencingGlobal)
!


!ExtractAsParameter class reorganize!
('pre-conditions' assert:isInImplementors: assertAndCreateNamed:extractedFromAll:at:implementors:senders:creatingAddParameterWith: assertCanExtractFrom:for:methodNode:last: assertCanExtractedFromAll:for: assertHasExtractableBraceElements: assertIsSameExpressionToExtractFrom:to: assertIsValidAssigmentToExtract: assertIsValidBlockNodeToExtract: assertIsValidKeywordForNewParameter: assertIsValidLiteralNodeToExtract: assertIsValidMessageNodeToExtract: assertIsValidParameterName: assertIsValidTempOrArgNodeToExtract: assertIsValidToExtract: is:equalTo:or: nodeToExtractFrom:at:or: removeDotsAt:in:)
('instance creation' addParameterClass named:extractedFrom:at:implementors:senders: named:extractedFrom:at:newKeyword:at:implementors:senders: named:extractedFromAll:at:implementors:senders: named:extractedFromAll:at:newKeyword:at:implementors:senders:)
('error messages' errorMessageForInvalidExpressionToExtractAsParameter errorMessageForInvalidSelection errorMessageForNoExpressionToExtract errorMessageForOrigialMethodMustBeInImplementorsToChange errorMessageNotAllExpressionsToExtractAreEqual)
('exceptions' signalInvalidExpressionToExtractAsParameter signalInvalidSelection signalNoExpressionToExtract signalNotAllExpressionsToExtractAreEqual signalOrigialMethodMustBeInImplementorsToChange)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inSystem:)
('intervals' intervalsForEquivalentExpressionIn:at: intervalsForEquivalentNodesTo:in:)
!

