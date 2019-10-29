'From Cuis 5.0 [latest update: #3943] on 29 October 2019 at 11:08:22 am'!

!ChangeList methodsFor: 'as yet unclassified' stamp: 'HAW 10/29/2019 11:06:21'!
classCategoryFrom: tokens

	^self field: #category: from: tokens ifAbsentOrNil: [ Categorizer default ]! !

!ChangeList methodsFor: 'as yet unclassified' stamp: 'HAW 10/29/2019 10:53:32'!
field: aFieldName from: tokens ifAbsentOrNil: aBlock

	| fieldValue fieldIndex |
	
	fieldIndex _ tokens indexOf: aFieldName ifAbsent: [ ^aBlock value ].
	fieldValue _ tokens at: fieldIndex + 1 ifAbsent: [ ^aBlock value ].
	
	^ fieldValue ifNil: aBlock! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/29/2019 11:06:27'!
classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category _ self classCategoryFrom: tokens.
	stamp _ self stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: category 
		meta: isMeta 
		stamp: stamp.
		
	record markAsTest: (classDefinition beginsWith: TestCase name asString).
	
	^record
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/29/2019 10:54:41'!
stampFrom: tokens

	^self field: #stamp: from: tokens ifAbsentOrNil: [ '' ]	
	
! !

!methodRemoval: ChangeList #categoryFrom: stamp: 'HAW 10/29/2019 11:06:21'!
ChangeList removeSelector: #categoryFrom:!

!ChangeList reorganize!
('initialization-release' addItem:text: initialize wantsPrettyDiffOption)
('scanning' classDefinitionRecordFrom: itemIsRecognized: notSeparatorChar priorMethodReferenceFrom: scanAndIgnore:in: scanCategory:class:meta:stamp:prior: scanClassComment: scanClassDefinition: scanClassRemoval: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodRemoval: scanSpecificChangeRecordTypeIfNotAtEnd: scanUpTo:informing: stampFrom:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods xtraVersion)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent contentsSymbolQuints diffedVersionContents list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange file showsVersions)
('shout styling' shouldStyle:with:)
('private' clearSelections)
('testing' isNewMethod:)
('*TDDGuruFirstBootstraping' scanSpecificChangeRecordType)
('as yet unclassified' classCategoryFrom: field:from:ifAbsentOrNil:)
!

