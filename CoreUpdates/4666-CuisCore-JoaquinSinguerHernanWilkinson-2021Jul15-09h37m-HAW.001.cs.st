'From Cuis 5.0 [latest update: #4665] on 15 July 2021 at 10:48:24 am'!

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/15/2021 10:09:27'!
addCategoriesOf: aClass to: categories separatingWith: lines

	| classCategories reject  |
	
	classCategories := aClass methodCategoriesAsSortedCollection.
	reject := classCategories asSet.
	aClass isMeta ifTrue: [ 
		categories add: Categorizer instanceCreation.
		classCategories remove: Categorizer instanceCreation ifAbsent: [].
		reject add: Categorizer instanceCreation].
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	categories addAll: classCategories.	
	
	aClass allSuperclasses do: [ :superclass | | superclassCategories |
		superclassCategories := superclass methodCategoriesAsSortedCollection reject: [ :cat | reject includes: cat].
		superclassCategories isEmpty ifFalse: [
			lines add: categories size.
			categories addAll: superclassCategories.
			reject addAll: superclassCategories]].
	
! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'HAW 7/15/2021 09:58:10'!
methodCategories

	| categories |
	
	categories := OrderedCollection withAll: self organization categories.
	categories remove: ClassOrganizer nullCategory asSymbol ifAbsent: nil.
	
	^categories ! !

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'HAW 7/15/2021 09:54:49'!
methodCategoriesAsSortedCollection

	^self methodCategories asSortedCollection: [ :leftCategory :rightCategory | leftCategory asLowercase < rightCategory asLowercase ]
! !


!Debugger methodsFor: 'context stack menu' stamp: 'HAW 7/15/2021 10:09:20'!
askForCategoryIn: aClass default: aDefaultCategory

	| categories index category lines |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	self addCategoriesOf: aClass to: categories separatingWith: lines.
	
	index := (PopUpMenu labelArray: categories lines: lines)
		startUpWithCaption: 'Please provide a good category for the new method!!'.
	index = 0 ifTrue: [^ aDefaultCategory].
	category := index = 1 
		ifTrue: [self request: 'Enter category name:' initialAnswer: '']
		ifFalse: [categories at: index].
		
	^ category isEmpty ifTrue: [aDefaultCategory] ifFalse: [category]! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'HAW 7/15/2021 09:59:16'!
allMethodCategoriesIntegratedThrough: mostGenericClass
	"Answer a list of all the method categories of the receiver and all its superclasses, up through mostGenericClass"

	| aColl |
	aColl _ OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		(aClass includesBehavior: mostGenericClass)
			ifTrue: [ aColl addAll: aClass methodCategories ]].
	
	^ aColl asSet asSortedCollection: [ :a :b | a asLowercase < b asLowercase ]

"ColorTileMorph allMethodCategoriesIntegratedThrough: TileMorph"! !

