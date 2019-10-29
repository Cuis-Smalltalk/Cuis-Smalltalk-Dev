'From Cuis 5.0 [latest update: #3931] on 26 October 2019 at 6:02:11 pm'!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:59:01'!
priorMethodReferenceFrom: tokens

	| priorMethodReference tagIndex |
	
	tagIndex _ tokens indexOf: #prior: ifAbsent: [ ^ nil ].
	priorMethodReference _ tokens at: tagIndex + 1.
	
	^ priorMethodReference
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 18:00:17'!
scanCategory: category class: class meta: meta stamp: stamp prior: aPriorMethod 

	| itemPosition method |

	[
		itemPosition _ file position.
		method _ file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new 
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp
					prior: aPriorMethod)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:58:04'!
stampFrom: tokens

	| stamp stampIndex |
	
	stampIndex _ tokens indexOf: #stamp: ifAbsent: [ ^'' ].
	stamp _ tokens at: stampIndex + 1.
	
	^ stamp
! !


!ChangeRecord methodsFor: 'access' stamp: 'HAW 10/26/2019 18:01:07'!
prior

	^prior! !

!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/26/2019 18:00:29'!
file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior

	self file: aFile position: aPosition type: aType.
	class _ aClassName.
	category _ aClassCategory.
	meta _ isMeta.
	stamp _ aStamp.
	prior _ aPrior.! !


!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/26/2019 17:53:44'!
stamp

	^stamp! !


!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/26/2019 17:53:57'!
stamp

	^stamp! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:58:04'!
scanClassDefinition: tokens

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
					
	self addItem: record text: 'classDefinition: ', classDefinition.! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:58:04'!
scanClassRemoval: tokens

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #classMoveToSomePackage:.
	stamp _ self stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ClassDeletionChangeRecord 
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description _ doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:59:01'!
scanMethodDefinition: tokens

	| stamp className priorMethod |
	
	className _ tokens first.
	stamp _ self stampFrom: tokens.
	priorMethod _ self priorMethodReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorMethod ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorMethod ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:58:04'!
scanMethodRemoval: tokens

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #methodMoveToSomePackage:.

	sourceCode _ file nextChunk.
	isMeta _ tokens third == #class.	
	selector _ tokens at: tokens size - 2.
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


!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/26/2019 17:22:59'!
file: aFile position: aPosition type: aType

	file _ aFile.
	position _ aPosition.
	type _ aType.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !

!ChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/26/2019 18:00:17'!
file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp

	self 
		file: aFile 
		position: aPosition 
		type: aType 
		class: aClassName 
		category: aClassCategory 
		meta: isMeta 
		stamp: aStamp 
		prior: nil
! !


!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/26/2019 18:01:23'!
changeType

	^ #classRemoval! !

!methodRemoval: ChangeRecord #priorMethod stamp: 'HAW 10/26/2019 18:01:07'!
ChangeRecord removeSelector: #priorMethod!
!methodRemoval: ChangeRecord #file:position:type:class:category:meta:stamp:priorMethod: stamp: 'HAW 10/26/2019 18:00:17'!
ChangeRecord removeSelector: #file:position:type:class:category:meta:stamp:priorMethod:!
!methodRemoval: ChangeList #scanCategory:class:meta:stamp: stamp: 'HAW 10/26/2019 17:20:31'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:!
!methodRemoval: ChangeList #scanChangeStamp: stamp: 'HAW 10/26/2019 17:58:04'!
ChangeList removeSelector: #scanChangeStamp:!
!methodRemoval: ChangeList #scanPriorMethodReference: stamp: 'HAW 10/26/2019 17:59:01'!
ChangeList removeSelector: #scanPriorMethodReference:!

!ChangeList reorganize!
('initialization-release' addItem:text: initialize wantsPrettyDiffOption)
('scanning' itemIsRecognized: notSeparatorChar priorMethodReferenceFrom: scanAndIgnore:in: scanCategory:class:meta:stamp:prior: scanClassComment: scanClassDefinition: scanClassRemoval: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodRemoval: scanSpecificChangeRecordType scanSpecificChangeRecordTypeIfNotAtEnd: scanUpTo:informing: stampFrom:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods xtraVersion)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent contentsSymbolQuints diffedVersionContents list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange file showsVersions)
('shout styling' shouldStyle:with:)
('private' clearSelections)
('testing' isNewMethod:)
!

!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeRecord reorganize!
('access' category changeClass changeClassName changeType compilerClass dateAndTime fileName fileOutOn: isMetaClassChange methodSelector prior stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: file:position:type:class:category:meta:stamp:prior: fileIn)
('testing' isDoIt)
('printing' printOn:)
!

