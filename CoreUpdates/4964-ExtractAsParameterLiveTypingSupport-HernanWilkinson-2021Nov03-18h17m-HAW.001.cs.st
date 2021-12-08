'From Cuis 5.0 [latest update: #4913] on 3 November 2021 at 7:32:45 pm'!

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 11/3/2021 19:11:04'!
addParameterClass

	"This method is important for LiveTyping parameterization. Do not remove - Hernan"
	^ AddParameter! !


!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 11/3/2021 19:09:24'!
assertIsValidKeywordForNewParameter: aNewKeyword

	self addParameterClass assertIsValidKeywordForNewParameter: aNewKeyword! !

!ExtractAsParameter class methodsFor: 'pre-conditions' stamp: 'HAW 11/3/2021 19:09:24'!
assertIsValidParameterName: aName

	self addParameterClass assertIsValidParameterName: aName ! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 11/3/2021 19:09:24'!
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
			self addParameterClass 
				named: aNewParameter 
				initializedWith: sourceCodeToExtract 
				toUnarySelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'instance creation' stamp: 'HAW 11/3/2021 19:09:24'!
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
			self addParameterClass 
				named: aNewParameter 
				at: aNewKeywordIndex
				initializedWith: sourceCodeToExtract 
				using: newKeyword 
				toKeywordSelector: aSourceMethod selector 
				implementors: implementorsCollection 
				senders: sendersCollection ]! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 11/3/2021 19:09:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor

	^self addParameterClass addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 11/3/2021 19:09:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	^self addParameterClass addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 11/3/2021 19:09:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	^self addParameterClass addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 11/3/2021 19:09:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass

	^self addParameterClass addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass
! !

!ExtractAsParameter class methodsFor: 'implementors and senders' stamp: 'HAW 11/3/2021 19:09:24'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem

	^self addParameterClass addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem
! !


!ChangeSelectorApplier class methodsFor: 'instance creation' stamp: 'HAW 1/1/2020 21:54:30'!
on: aBrowser for: aSelector in: aSelectedClass

	self assertCanApplyRefactoringFor: aSelector in: aSelectedClass.

	^self new initializeOn: aBrowser for: aSelector in: aSelectedClass
	! !


!ExtractAsParameter class reorganize!
('pre-conditions' assert:isInImplementors: assertAndCreateNamed:extractedFromAll:at:implementors:senders:creatingAddParameterWith: assertCanExtractFrom:for:methodNode:last: assertCanExtractedFromAll:for: assertIsSameExpressionToExtractFrom:to: assertIsValidAssigmentToExtract: assertIsValidBlockNodeToExtract: assertIsValidKeywordForNewParameter: assertIsValidLiteralNodeToExtract: assertIsValidMessageNodeToExtract: assertIsValidParameterName: assertIsValidTempOrArgNodeToExtract: assertIsValidToExtract: is:equalTo:or: nodeToExtractFrom:at:or: removeDotsAt:in:)
('instance creation' addParameterClass named:extractedFrom:at:implementors:senders: named:extractedFrom:at:newKeyword:at:implementors:senders: named:extractedFromAll:at:implementors:senders: named:extractedFromAll:at:newKeyword:at:implementors:senders:)
('error messages' errorMessageForInvalidExpressionToExtractAsParameter errorMessageForInvalidSelection errorMessageForNoExpressionToExtract errorMessageForOrigialMethodMustBeInImplementorsToChange errorMessageNotAllExpressionsToExtractAreEqual)
('exceptions' signalInvalidExpressionToExtractAsParameter signalInvalidSelection signalNoExpressionToExtract signalNotAllExpressionsToExtractAreEqual signalOrigialMethodMustBeInImplementorsToChange)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inSystem:)
('intervals' intervalsForEquivalentExpressionIn:at: intervalsForEquivalentNodesTo:in:)
!

