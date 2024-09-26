'From Cuis7.1 [latest update: #6750] on 26 September 2024 at 2:06:13 pm'!

!Categorizer class methodsFor: 'class initialization' stamp: 'jmv 9/26/2024 10:50:53'!
defaultCategory
	^Default! !


!SystemOrganizer class methodsFor: 'class initialization' stamp: 'jmv 9/26/2024 10:52:37'!
defaultCategory
	^  self defaultClassCategory! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 9/26/2024 10:55:35'!
addCategory: catString before: nextCategory
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
		ifTrue: [self removeCategory: default]! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 9/26/2024 10:56:07'!
classify: element under: heading suppressIfDefault: aBoolean
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

	(self listAtCategoryNamed: default) isEmpty ifTrue: [self removeCategory: default]! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 9/26/2024 10:56:35'!
removeCategory: cat 
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
	categoryArray isEmpty
		ifTrue: [
			categoryArray := Array with: self class defaultCategory.
			categoryStops := Array with: 0]
! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 9/26/2024 10:56:43'!
removeEmptyCategories
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
	categoryArray isEmpty
		ifTrue: [
			categoryArray := Array with: self class defaultCategory.
			categoryStops := Array with: 0]

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."! !

!Categorizer methodsFor: 'private' stamp: 'jmv 9/26/2024 10:56:52'!
setDefaultList: anArray

	categoryArray := Array with: self class defaultCategory.
	categoryStops := Array with: anArray size.
	elementArray := anArray! !

!Categorizer methodsFor: 'testing' stamp: 'jmv 9/26/2024 10:56:25'!
hasAnyCategoriesSuchThat: aBlock
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ false].
	(categoryArray size = 1 
		and: [categoryArray first = self class defaultCategory & elementArray isEmpty])
		ifTrue: [^false].
	^categoryArray anySatisfy: aBlock! !


!SystemOrganizer class methodsFor: 'class initialization' stamp: 'jmv 9/26/2024 10:52:51'!
defaultClassCategory
	^ #Unclassified! !


!SystemOrganizer class reorganize!
('class initialization' defaultCategory defaultClassCategory)
!

