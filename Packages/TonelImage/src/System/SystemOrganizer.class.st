"
My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.
"
Class {
	#name : 'SystemOrganizer',
	#superclass : 'Categorizer',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'class initialization' }
SystemOrganizer class >> defaultCategory [
	^  self defaultClassCategory
]

{ #category : 'class initialization' }
SystemOrganizer class >> defaultClassCategory [
	^ #Unclassified
]

{ #category : 'categories' }
SystemOrganizer >> allClassesAt: aCategoryName [
	"Same as >>classesAt: but if aCategoryName is a top category it includes all classes in subcategories."
	^ (self fullListAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName]
]

{ #category : 'remove' }
SystemOrganizer >> categoriesMatching: matchString [
	"Return all matching categories"
	^ self categories select: [:c | matchString match: c]
]

{ #category : 'source code management' }
SystemOrganizer >> classMethodCountOf: category [

	^  (self allClassesAt: category) sum: [ :cl | cl class selectors size ] ifEmpty: 0.
]

{ #category : 'categories' }
SystemOrganizer >> classesAt: aCategoryName [
	^ (self listAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName]
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> fileOutAllCategories [
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	DirectoryEntry fileOutsDirQuery: 'Cuis-AllCode.st' writeStreamDo: [ :fileStream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: fileStream ]].
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> fileOutCategory: aCategoryRoot [
	"FileOut all the classes in the named system category."

	DirectoryEntry fileOutsDirQuery: (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
			self fileOutCategory: category on: fileStream initializing: true ]].
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> fileOutCategory: category on: aFileStream initializing: aBool [
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes := (self superclassOrderIn: category).
	poolSet := Set new.
	classes do: [ :class |
		class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet notEmpty ifTrue: [
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet := poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first := true.
	classes do: [ :class | 
		first
			ifTrue: [first := false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].
	aBool ifTrue: [classes do:[:cls| cls fileOutInitializerOn: aFileStream]].
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> fileOutCategoryNoPoolsNoInit: category on: aFileStream [
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first classes |
	classes := self superclassOrderIn: category.
	first := true.
	classes do: [ :class | 
		first
			ifTrue: [first := false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false
			sortMethodsByCategory: true ].
]

{ #category : 'categories' }
SystemOrganizer >> fullListAtCategoryNamed: aCategoryNameOrPrefix [
	"Same as >>listAtCategoryNamed: but include subcategories if aCategoryName is a top category."
	| answer |
	answer := self listAtCategoryNamed: aCategoryNameOrPrefix.
	"If it's a top category, include the classes of all its subcategories:"
	(aCategoryNameOrPrefix includes: $-) ifFalse:
		[| prefix |
		prefix := aCategoryNameOrPrefix, '-'.
		self categories do: [:each|
			(each beginsWith: prefix) ifTrue: [
				answer := answer, (self listAtCategoryNamed: each)]]].
	^ answer
]

{ #category : 'categories' }
SystemOrganizer >> hasCategory: aCategory [

	^self categories includes: aCategory 
]

{ #category : 'source code management' }
SystemOrganizer >> instanceMethodCountOf: category [

	^ (self allClassesAt: category) sum: [ :cl | cl selectors size ] ifEmpty: 0.
]

{ #category : 'source code management' }
SystemOrganizer >> linesOfCodeOf: category [
"
SystemOrganization linesOfCodeOf: #'System-Files'

See also #messageSendsCountOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl linesOfCode ] ifEmpty: 0.
]

{ #category : 'source code management' }
SystemOrganizer >> messageSendsCountOf: category [
"
SystemOrganization messageSendsCountOf: #'System-Files'

See also #linesOfCodeOf:
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl messageSendsCount ] ifEmpty: 0.
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

self == SystemOrganization ifTrue: [
	dp := DiskProxy global: #SystemOrganization selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp].
^ self

]

{ #category : 'remove' }
SystemOrganizer >> removeEmptySystemCategories [
	"
	SystemOrganization removeEmptySystemCategories.
	SystemOrganization categories do: [ :c |
	(SystemOrganization listAtCategoryNamed: c) isEmpty ifTrue: [
		c print ]].
	"
	self categories do: [ :c |
		(self listAtCategoryNamed: c) isEmpty ifTrue: [
			self removeCategory: c ]]
]

{ #category : 'remove' }
SystemOrganizer >> removeMissingClasses [
	"Remove any class names that are no longer in the Smalltalk dictionary. Used for cleaning up after garbage collecting user-generated classes."
	"SystemOrganization removeMissingClasses"

	elementArray copy do: [:el |
		(Smalltalk includesKey: el) ifFalse: [self removeElement: el]].

]

{ #category : 'remove' }
SystemOrganizer >> removeSystemCategories: categories [

	(self superclassOrderInAll: categories) reverseDo: [ :class | class removeFromSystem].

	categories do: [ :aCategory | self removeCategory: aCategory].

]

{ #category : 'remove' }
SystemOrganizer >> removeSystemCategory: category [
	"remove all the classes associated with the category and its subcategories.
	If category does not exist, it does nothing"

	(self withSubCategoriesOf: category) reverseDo: [ :categoryToRemove |
		(self superclassOrderIn: categoryToRemove) reverseDo: [ :class | class removeFromSystem].
		self removeCategory: categoryToRemove ]
]

{ #category : 'categories' }
SystemOrganizer >> renameSystemCategory: oldName to: newName [

	"If it's a top category, rename subcategories first:"
	(self withSubCategoriesOf: oldName) reverseDo: [ :each |
		self renameCategory: each to: newName, (each allButFirst: oldName size)].

	Smalltalk allBehaviorsDo: [ :aClass | 
		aClass organization renamePackageCategoriesFrom: oldName to: newName ]
]

{ #category : 'categories' }
SystemOrganizer >> rootCategoryOf: aCategory [

	^ (aCategory findTokens: '-') first
]

{ #category : 'categories' }
SystemOrganizer >> subCategoriesOf: aRootCategoryName [

	^self categoriesMatching: (self subCategoriesPatternOf: aRootCategoryName)
	
]

{ #category : 'categories' }
SystemOrganizer >> subCategoriesPatternOf: aRootCategoryName [

	^ aRootCategoryName,'-*'
]

{ #category : 'fileIn/Out' }
SystemOrganizer >> superclassOrderIn: category  [
	"Answer an OrderedCollection containing references to the classes in the 
	category whose name is the argument, category (a string). The classes 
	are ordered with superclasses first so they can be filed in."

	| list |
	list := 
		(self listAtCategoryNamed: category asSymbol) 
			collect: [:title | Smalltalk at: title].
	^Array streamContents: [ :strm |
		Smalltalk hierarchySorted: list do: [ :cls | strm nextPut: cls ]].
]

{ #category : 'categories' }
SystemOrganizer >> superclassOrderInAll: categories [

	| classes |

	classes := OrderedCollection new.
	categories do: [ :aCategory | classes addAll: (self classesAt: aCategory)].

	^Array streamContents: [ :stream | Smalltalk hierarchySorted: classes do: [ :aClass | stream nextPut: aClass ]].
]

{ #category : 'categories' }
SystemOrganizer >> testCaseClassesAt: aCategoryName [

	^(self allClassesAt: aCategoryName) select: [ :aClass | aClass is: #TestCaseClass ]
]

{ #category : 'categories' }
SystemOrganizer >> withSubCategoriesOf: aRootCategoryName [

	"Returns the root category and subcategories if they exist
	The order is important because it is assumed like that in some senders
	If aRootCategoryName does not exist as category, it will return it anyway - Hernan"
	^(OrderedCollection with: aRootCategoryName) 
		addAll: (self subCategoriesOf: aRootCategoryName);
		yourself.
	
	
]
