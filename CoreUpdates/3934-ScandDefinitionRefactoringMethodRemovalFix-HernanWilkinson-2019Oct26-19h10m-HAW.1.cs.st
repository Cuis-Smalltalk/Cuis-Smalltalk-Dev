'From Cuis 5.0 [latest update: #3931] on 26 October 2019 at 11:35:30 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 20:10:35'!
classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName stamp |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	stamp _ self stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: tokens last 
		meta: isMeta 
		stamp: stamp.
		
	^record
! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 20:12:15'!
scanClassDefinition: tokens

	| record |
	
	record _ self classDefinitionRecordFrom: tokens.
						
	self addItem: record text: 'classDefinition: ', record changeClassName.
	! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 23:23:14'!
scanMethodRemoval: tokens

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #methodMoveToSomePackage:.

	sourceCode _ file nextChunk.
	isMeta _ tokens third == #class.	
	selector _ isMeta ifTrue: [ tokens fourth ] ifFalse: [ tokens third ].
	stamp _ self stampFrom: tokens.

	methodReference _ MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.
		
	record _ MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description _ doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion! !


!ChangeList reorganize!
('initialization-release' addItem:text: initialize wantsPrettyDiffOption)
('scanning' classDefinitionRecordFrom: itemIsRecognized: notSeparatorChar priorMethodReferenceFrom: scanAndIgnore:in: scanCategory:class:meta:stamp:prior: scanClassComment: scanClassDefinition: scanClassRemoval: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodRemoval: scanSpecificChangeRecordTypeIfNotAtEnd: scanUpTo:informing: stampFrom:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods xtraVersion)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent contentsSymbolQuints diffedVersionContents list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange file showsVersions)
('shout styling' shouldStyle:with:)
('private' clearSelections)
('testing' isNewMethod:)
('*TDDGuruFirstBootstraping' scanSpecificChangeRecordType scanTestClassDefinition: scanTestRun:)
!

