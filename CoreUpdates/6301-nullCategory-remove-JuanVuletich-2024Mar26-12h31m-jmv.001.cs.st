'From Cuis6.3 [latest update: #6298] on 26 March 2024 at 12:35:02 pm'!
!classDefinition: #Categorizer category: #'Kernel-Classes'!
Object subclass: #Categorizer
	instanceVariableNames: 'categoryArray categoryStops elementArray '
	classVariableNames: 'Default NullCategory '
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!Browser methodsFor: 'message functions' stamp: 'jmv 3/26/2024 11:59:55'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList scarySelector |
	selectedMessageName := self selectedMessageName.
	oldMessageList := self messageList.
	 self metaClassIndicated ifTrue: [
		scarySelector := self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: scarySelector) not
			and: [Metaclass isScarySelector: scarySelector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (scarySelector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	category := self selectedMessageCategoryName ifNotNil: [ :n | n asSymbol ].
	category == ClassOrganizer allCategory ifTrue: [
		"Current category for existing methods or if a method was selected,
		nil for new methods when no previous method was selected."
		category := self categoryOfCurrentMethod ].
	selector := self selectedClassOrMetaClass
				compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/26/2024 11:55:46'!
methodCategories

	^self organization categories! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 3/26/2024 12:14:52'!
categories
	"Answer an Array of categories (names)."
	^categoryArray! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 3/26/2024 11:52:39'!
classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	
	realHeading := heading isNil
		ifTrue: [Default]
		ifFalse: [heading withBlanksTrimmed ].
	
	"done if already under that category"
	(catName := self categoryOfElement: element) = realHeading ifTrue: [^ self].  

	catName ifNotNil: [
		"return if non-Default category already assigned in memory"
		(aBoolean and: [realHeading = Default]) ifTrue: [^ self].	 
		"remove if in another category"
		self removeElement: element].

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex := categoryArray indexOf: realHeading.
	elemIndex := catIndex > 1
		ifTrue: [categoryStops at: catIndex - 1]
		ifFalse: [0].
	[(elemIndex := elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray := elementArray copyReplaceFrom: elemIndex to: elemIndex-1 with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: [ :i |
		categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: Default) isEmpty ifTrue: [self removeCategory: Default]! !


!Categorizer class methodsFor: 'class initialization' stamp: 'jmv 3/26/2024 11:58:40'!
initialize
	"
	Categorizer initialize	
	"
	Default := 'as yet unclassified' asSymbol.! !


!MethodCategoriesPrompter methodsFor: 'categories initialization - private' stamp: 'jmv 3/26/2024 11:57:57'!
createCategories

	| classCategories |
	
	categories := OrderedCollection with: 'new ...'.
	lines := OrderedCollection with: 1. 
	classCategories := startClass methodCategoriesAsSortedCollection.
	
	reject := classCategories asSet.
	reject
		add: ClassOrganizer default.
	
	startClass isMeta ifTrue: [ self initializeCategoriesWhenMeta: classCategories].
	rejectingFirst ifFalse: [ categories addAll: classCategories ]! !

!methodRemoval: Categorizer class #nullCategory stamp: 'jmv 3/26/2024 12:33:07'!
Categorizer class removeSelector: #nullCategory!
!classDefinition: #Categorizer category: #'Kernel-Classes'!
Object subclass: #Categorizer
	instanceVariableNames: 'categoryArray categoryStops elementArray'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Kernel-Classes'!
Categorizer initialize!
