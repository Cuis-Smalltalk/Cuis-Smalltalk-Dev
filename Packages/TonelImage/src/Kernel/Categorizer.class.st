Class {
	#name : 'Categorizer',
	#superclass : 'Object',
	#instVars : [
		'categoryArray',
		'categoryStops',
		'elementArray'
	],
	#classVars : [
		'Default'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'well known categories' }
Categorizer class >> accessing [

	^'accessing'
]

{ #category : 'class initialization' }
Categorizer class >> allCategory [
	"Return a symbol that represents the virtual all methods category."

	^ #'-- all --'
]

{ #category : 'class initialization' }
Categorizer class >> defaultCategory [
	^Default
]

{ #category : 'documentation' }
Categorizer class >> documentation [
	"Instances consist of an Array of category names (categoryArray), each of 
	which refers to an Array of elements (elementArray). This association is 
	made through an Array of stop indices (categoryStops), each of which is 
	the index in elementArray of the last element (if any) of the 
	corresponding category. For example: categories := Array with: 'firstCat' 
	with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
	elements := Array with: #a with: #b with: #c with: #d. This means that 
	category firstCat has only #a, secondCat has #b, #c, and #d, and 
	thirdCat has no elements. This means that stops at: stops size must be the 
	same as elements size." 
]

{ #category : 'instance creation' }
Categorizer class >> empty [
	"Answer an instance of me with initial elements from the argument"

	^self new initEmpty
]

{ #category : 'well known categories' }
Categorizer class >> initialization [
	
	^'initialization'
]

{ #category : 'class initialization' }
Categorizer class >> initialize [
	"
	Categorizer initialize	
	"
	Default := 'as yet unclassified' asSymbol.
]

{ #category : 'well known categories' }
Categorizer class >> instanceCreation [

	^'instance creation'
]

{ #category : 'accessing' }
Categorizer >> addCategory: newCategory [
	^ self addCategory: newCategory before: nil 
]

{ #category : 'accessing' }
Categorizer >> addCategory: catString before: nextCategory [
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	
	| index newCategory default |
	
	newCategory := catString withBlanksTrimmed asSymbol.
	
	"heading already exists, so done"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^self].	
	
	index := categoryArray indexOf: nextCategory ifAbsent: [categoryArray size + 1].
	categoryArray := categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops := categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).

	"remove empty default category"
	default := self class defaultCategory.
	(newCategory ~= default and: [(self listAtCategoryNamed: default) isEmpty])
		ifTrue: [self removeCategory: default]
]

{ #category : 'accessing' }
Categorizer >> allMethodSelectors [
	"give a list of all method selectors."

	^ elementArray copy sort
]

{ #category : 'accessing' }
Categorizer >> categories [
	"Answer an Array of categories (names)."
	^categoryArray
]

{ #category : 'accessing' }
Categorizer >> categories: anArray  [
	"Reorder my categories to be in order of the argument, anArray. If the 
	resulting organization does not include all elements, then give an error."

	| newCategories newStops newElements catName list runningTotal | 
	newCategories := Array new: anArray size.
	newStops := Array new: anArray size.
	newElements := #().
	runningTotal := 0.
	1 to: anArray size do:
		[:i |
		catName := (anArray at: i) asSymbol.
		list := self listAtCategoryNamed: catName.
				newElements := newElements, list.
				newCategories at: i put: catName.
				newStops at: i put: (runningTotal := runningTotal + list size)].
	elementArray do: [ :element | "check to be sure all elements are included"
		(newElements includes: element)
			ifFalse: [^self error: 'New categories must match old ones']].
	"Everything is good, now update my three arrays."
	categoryArray := newCategories.
	categoryStops := newStops.
	elementArray := newElements
]

{ #category : 'accessing' }
Categorizer >> categoryOfElement: element  [
	"Answer the category associated with the argument, element."

	| index |
	index := self numberOfCategoryOfElement: element.
	index = 0
		ifTrue: [^nil]
		ifFalse: [^categoryArray at: index]
]

{ #category : 'accessing' }
Categorizer >> changeFromCategorySpecs: categorySpecs  [
	"Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane."

	| oldElements newElements newCategories newStops currentStop temp cc catSpec |
	oldElements := elementArray asSet.
	newCategories := Array new: categorySpecs size.
	newStops := Array new: categorySpecs size.
	currentStop := 0.
	newElements := WriteStream on: (Array new: 16).
	1 to: categorySpecs size do: [ :i | 
		catSpec := categorySpecs at: i.
		newCategories at: i put: catSpec first asSymbol.
		catSpec allButFirst asArray sort do:
			[:elem |
			(oldElements remove: elem ifAbsent: nil) ifNotNil: [
				newElements nextPut: elem.
				currentStop := currentStop+1]].
		newStops at: i put: currentStop].

	"Ignore extra elements but don't lose any existing elements!"
	oldElements := oldElements collect: [ :elem | Array with: (self categoryOfElement: elem) with: elem].
	newElements := newElements contents.
	categoryArray := newCategories.
	(cc := categoryArray asSet) size = categoryArray size ifFalse: [ "has duplicate element"
		temp := categoryArray asOrderedCollection.
		temp removeAll: categoryArray asSet asOrderedCollection.
		temp do: [ :each | | dup ii |
			dup := each.
			ii := categoryArray indexOf: dup.
			[ dup := (dup,' #2') asSymbol.  cc includes: dup ] whileTrue.
			cc add: dup.
			categoryArray at: ii put: dup]].
	categoryStops := newStops.
	elementArray := newElements.
	oldElements do: [:pair | self classify: pair last under: pair first]
]

{ #category : 'accessing' }
Categorizer >> changeFromString: aString  [
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs := Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self initEmpty].

	^ self changeFromCategorySpecs: categorySpecs
]

{ #category : 'accessing' }
Categorizer >> classify: element under: heading  [
	self classify: element under: heading suppressIfDefault: true
]

{ #category : 'accessing' }
Categorizer >> classify: element under: heading suppressIfDefault: aBoolean [
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading default |

	default := self class defaultCategory.

	realHeading := heading isNil
		ifTrue: [default]
		ifFalse: [heading withBlanksTrimmed ].

	"done if already under that category"
	(catName := self categoryOfElement: element) = realHeading ifTrue: [^ self].  

	catName ifNotNil: [
		"return if non-Default category already assigned in memory"
		(aBoolean and: [realHeading = default]) ifTrue: [^ self].	 
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

	(self listAtCategoryNamed: default) isEmpty ifTrue: [self removeCategory: default]
]

{ #category : 'accessing' }
Categorizer >> classifyAll: aCollection under: heading [

	aCollection do:
		[:element | self classify: element under: heading]
]

{ #category : 'private' }
Categorizer >> elementArray [

	^ elementArray
]

{ #category : 'accessing' }
Categorizer >> elementCategoryDict [
	| dict firstIndex lastIndex |
	elementArray ifNil: [^ nil].
	dict := Dictionary new.
	1to: categoryStops size do: [:cat |
		firstIndex := self firstIndexOfCategoryNumber: cat.
		lastIndex := self lastIndexOfCategoryNumber: cat.
		firstIndex to: lastIndex do: [:el |
			dict at: (elementArray at: el) put: (categoryArray at: cat)].
	].
	^ dict.
]

{ #category : 'private' }
Categorizer >> firstIndexOfCategoryNumber: anInteger [
	anInteger < 1 ifTrue: [^ nil].
	^ (anInteger > 1
			ifTrue: [(categoryStops at: anInteger - 1) + 1]
			ifFalse: [1]).
]

{ #category : 'testing' }
Categorizer >> hasAnyCategoriesSuchThat: aBlock [
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ false].
	(categoryArray size = 1 
		and: [categoryArray first = self class defaultCategory & elementArray isEmpty])
		ifTrue: [^false].
	^categoryArray anySatisfy: aBlock
]

{ #category : 'accessing' }
Categorizer >> initEmpty [
	categoryArray :=#().
	categoryStops := #().
	elementArray := #().
]

{ #category : 'accessing' }
Categorizer >> isEmptyCategoryNumber: anInteger [

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ true].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^ firstIndex > lastIndex
]

{ #category : 'testing' }
Categorizer >> isPackageCategoryExtension: aCategory [

	^aCategory beginsWith: '*'
]

{ #category : 'testing' }
Categorizer >> isPackageCategoryExtension: aCategory fromPackage: aPackageName [

	^ (self isPackageCategoryExtension: aCategory) and: [ aCategory 	is: aPackageName substringAt: 2 ]
]

{ #category : 'private' }
Categorizer >> lastIndexOfCategoryNumber: anInteger [
	anInteger > categoryStops size ifTrue: [^ nil].
	^ categoryStops at: anInteger
]

{ #category : 'accessing' }
Categorizer >> listAtCategoryNamed: categoryName [
	"Answer the array of elements associated with the name, categoryName."

	^ self listAtCategoryNumber: (categoryArray indexOf: categoryName ifAbsent: [^ #()])
]

{ #category : 'accessing' }
Categorizer >> listAtCategoryNumber: anInteger  [
	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ nil].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^elementArray copyFrom: firstIndex to: lastIndex
]

{ #category : 'accessing' }
Categorizer >> moveCategoryBottom: catString [
	| category newCategories |
	category := catString asSymbol.
	newCategories := (categoryArray copyWithout: category), (Array with: category).
	self categories: newCategories.
	^ newCategories size
]

{ #category : 'accessing' }
Categorizer >> moveCategoryDown: catString [
	| category index newIndex newCategories |
	category := catString asSymbol.
	newCategories := categoryArray copy.
	index := newCategories indexOf: category.
	newIndex := 0.
	(index > 0 and: [index < newCategories size]) ifTrue: [
		newIndex := index+1.
		newCategories swap: index with: newIndex.
		self categories: newCategories].
	^newIndex
]

{ #category : 'accessing' }
Categorizer >> moveCategoryTop: catString [
	| category newCategories |
	category := catString asSymbol.
	newCategories := (Array with: category), (categoryArray copyWithout: category).
	self categories: newCategories
]

{ #category : 'accessing' }
Categorizer >> moveCategoryUp: catString [
	| category index newIndex newCategories |
	category := catString asSymbol.
	newCategories := categoryArray copy.
	index := newCategories indexOf: category.
	newIndex := 0.
	index > 1 ifTrue: [
		newIndex := index-1.
		newCategories swap: index with: newIndex.
		self categories: newCategories].
	^newIndex
]

{ #category : 'accessing' }
Categorizer >> numberOfCategoryOfElement: element  [
	"Answer the index of the category with which the argument, element, is 
	associated."

	| categoryIndex elementIndex |
	categoryIndex := 1.
	elementIndex := 0.
	[(elementIndex := elementIndex + 1) <= elementArray size]
		whileTrue: 
			["point to correct category"
			[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: [categoryIndex := categoryIndex + 1].
			"see if this is element"
			element = (elementArray at: elementIndex) ifTrue: [^categoryIndex]].
	^0
]

{ #category : 'copying' }
Categorizer >> postCopy [
	categoryArray := categoryArray copy.
	categoryStops := categoryStops copy. 
	elementArray := elementArray copy
]

{ #category : 'printing' }
Categorizer >> printCategory: aCategory at: aCategoryIndex with: aStartElementIndex on: aStream [

	| elementIndex |
	
	elementIndex := aStartElementIndex.
	aStream nextPut: $(.
	aCategory printOn: aStream.
	
	[elementIndex <= (categoryStops at: aCategoryIndex)] whileTrue: [
		aStream space; nextPutAll: (elementArray at: elementIndex).
		elementIndex := elementIndex + 1].
	
	aStream nextPut: $); newLine.
	
	^elementIndex 
]

{ #category : 'printing' }
Categorizer >> printOn: aStream  [
	"Refer to the comment in Object|printOn:."

	self printOn: aStream excludingExtensions: false
]

{ #category : 'printing' }
Categorizer >> printOn: aStream excludingExtensions: shouldExcludeExtensions [
	"Refer to the comment in Object|printOn:."

	| elementIndex |
	
	elementIndex := 1.
	1 to: categoryArray size do: [ :categoryIndex | | category |
		category := (categoryArray at: categoryIndex) asString.
		(shouldExcludeExtensions and: [ self isPackageCategoryExtension: category ]) ifFalse: [
			elementIndex := self printCategory: category at: categoryIndex with: elementIndex on: aStream.]]
]

{ #category : 'accessing' }
Categorizer >> removeCategory: cat  [
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index := categoryArray indexOf: cat ifAbsent: [^self].
	lastStop := 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category'].
	categoryArray := categoryArray copyReplaceFrom: index to: index with: #().
	categoryStops := categoryStops copyReplaceFrom: index to: index with: #().

]

{ #category : 'accessing' }
Categorizer >> removeElement: element  [
	"Remove the selector, element, from all categories."
	| categoryIndex elementIndex nextStop newElements |
	categoryIndex := 1.
	elementIndex := 0.
	nextStop := 0.
	"nextStop keeps track of the stops in the new element array"
	newElements := WriteStream on: (Array new: elementArray size).
	[(elementIndex := elementIndex + 1) <= elementArray size]
		whileTrue: 
			[[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: 
					[categoryStops at: categoryIndex put: nextStop.
					categoryIndex := categoryIndex + 1].
			(elementArray at: elementIndex) = element
				ifFalse: 
					[nextStop := nextStop + 1.
					newElements nextPut: (elementArray at: elementIndex)]].
	[categoryIndex <= categoryStops size]
		whileTrue: 
			[categoryStops at: categoryIndex put: nextStop.
			categoryIndex := categoryIndex + 1].
	elementArray := newElements contents
]

{ #category : 'accessing' }
Categorizer >> removeEmptyCategories [
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories := WriteStream on: (Array new: 16).
	keptStops := WriteStream on: (Array new: 16).
	currentStop := categoryIndex := 0.
	[(categoryIndex := categoryIndex + 1) <= categoryArray size]
		whileTrue: 
			[(categoryStops at: categoryIndex) > currentStop
				ifTrue: 
					[keptCategories nextPut: (categoryArray at: categoryIndex).
					keptStops nextPut: (currentStop := categoryStops at: categoryIndex)]].
	categoryArray := keptCategories contents.
	categoryStops := keptStops contents.

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."
]

{ #category : 'accessing' }
Categorizer >> renameCategory: oldCatString to: newCatString [
	"Rename a category. No action if new name already exists, or if old name does not exist."
	
	| index newCategory |
	
	newCategory := newCatString withBlanksTrimmed.
	"new name exists, so no action"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^ self].
	
	"old name not found, so no action"
	(index := categoryArray indexOf: oldCatString) = 0 ifTrue: [^ self].	
	
	"need to change identity so smart list update will notice the change"
	categoryArray := categoryArray copy.  
	categoryArray at: index put: newCategory
]

{ #category : 'fileIn/Out' }
Categorizer >> scanFrom: aStream [
	"Reads in the organization from the next chunk on aStream.
	Categories or elements not found in the definition are not affected.
	New elements are ignored."

	self changeFromString: aStream nextChunk
]

{ #category : 'accessing' }
Categorizer >> sortCategories [
	| privateCategories publicCategories newCategories |

	privateCategories := self categories select: [ :one |
		(one findString: 'private' startingAt: 1 caseSensitive: false) = 1].
	publicCategories := self categories copyWithoutAll: privateCategories.
	newCategories := publicCategories asArray sort,
		privateCategories asArray sort.
	self categories: newCategories
]
