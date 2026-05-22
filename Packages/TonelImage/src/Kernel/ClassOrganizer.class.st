"
I represent method categorization information for classes.  The handling of class comments has gone through a tortuous evolution.   Grandfathered class comments (before late aug 98) have no time stamps, and historically, fileouts of class comments always substituted the timestamp reflecting the author and date/time at the moment of fileout; and historically any timestamps in a filed out class comment were dropped on the floor, with the author & time prevailing at the moment of filein being substituted.   Such grandfathered comments now go out on fileouts with '<historical>' timestamp; class comments created after the 8/98 changes will have their correct timestamps preserved, though there is not yet a decent ui for reading those stamps other than filing out and looking at the file; nor is there yet any ui for browsing and recovering past versions of such comments.  Everything in good time!
"
Class {
	#name : 'ClassOrganizer',
	#superclass : 'BasicClassOrganizer',
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'class variables' }
ClassOrganizer class >> defaultMethodCategory [
	^ Default
]

{ #category : 'instance creation' }
ClassOrganizer class >> withUnclassifiedSelectors: anArray  [

	^self new unclassifiedSelectors: anArray
]

{ #category : 'accessing' }
ClassOrganizer >> addCategory: catString before: nextCategory [
	| oldCategories |
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super addCategory: catString before: nextCategory].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
]

{ #category : 'accessing' }
ClassOrganizer >> changeFromCategorySpecs: categorySpecs [
	| oldDict oldCategories |
	oldDict := self elementCategoryDict.
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super changeFromCategorySpecs: categorySpecs].
	self notifyOfChangedSelectorsOldDict: oldDict newDict: self elementCategoryDict.
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
]

{ #category : 'accessing' }
ClassOrganizer >> classify: element under: heading suppressIfDefault: aBoolean [
	| oldCat newCat |
	oldCat := self categoryOfElement: element.
	SystemChangeNotifier uniqueInstance doSilently: [
		super classify: element under: heading suppressIfDefault: aBoolean].
	newCat := self categoryOfElement: element.
	self notifyOfChangedSelector: element from: oldCat to: newCat.
]

{ #category : 'accessing' }
ClassOrganizer >> initEmpty [
	SystemChangeNotifier uniqueInstance doSilently: [
		super initEmpty ].
]

{ #category : 'private' }
ClassOrganizer >> notifyOfChangedCategoriesFrom: oldCollectionOrNil to: newCollectionOrNil [
	(self hasSubject and: [oldCollectionOrNil ~= newCollectionOrNil]) 
		ifTrue: [SystemChangeNotifier uniqueInstance classReorganized: self subject].
]

{ #category : 'private' }
ClassOrganizer >> notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil [
	(self hasSubject and: [oldNameOrNil ~= newNameOrNil]) 
		ifTrue: [SystemChangeNotifier uniqueInstance classReorganized: self subject].
]

{ #category : 'private' }
ClassOrganizer >> notifyOfChangedSelector: element from: oldCategory to: newCategory [
	(self hasSubject and: [(oldCategory ~= newCategory)]) ifTrue: [
		SystemChangeNotifier uniqueInstance selectorRecategorized: element from: oldCategory to: newCategory inClass: self subject
	].
]

{ #category : 'private' }
ClassOrganizer >> notifyOfChangedSelectorsOldDict: oldDictionaryOrNil newDict: newDictionaryOrNil [
	(oldDictionaryOrNil isNil and: [newDictionaryOrNil isNil])
		ifTrue: [^ self].
		
	oldDictionaryOrNil ifNil: [
		newDictionaryOrNil keysAndValuesDo: [ :el :cat |
			self notifyOfChangedSelector: el from: nil to: cat].
		^ self.
	].

	newDictionaryOrNil ifNil: [
		oldDictionaryOrNil keysAndValuesDo: [ :el :cat |
			self notifyOfChangedSelector: el from: cat to: nil].
		^ self.
	].
		
	oldDictionaryOrNil keysAndValuesDo: [ :el :cat | | newCat |
		newCat := newDictionaryOrNil at: el.
		self notifyOfChangedSelector: el from: cat to: newCat.
	]
]

{ #category : 'accessing' }
ClassOrganizer >> removeCategory: cat  [
	| oldCategories |
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeCategory: cat].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
]

{ #category : 'accessing' }
ClassOrganizer >> removeElement: element [
	| oldCat |
	oldCat := self categoryOfElement: element.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeElement: element].
	self notifyOfChangedSelector: element from: oldCat to: (self categoryOfElement: element).
]

{ #category : 'accessing' }
ClassOrganizer >> removeEmptyCategories [
	| oldCategories |
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeEmptyCategories].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
]

{ #category : 'renaming' }
ClassOrganizer >> renameCategory: oldCatString to: newCatString [

	| newCategory oldElementsBefore oldElementsAfter |
	
	newCategory := newCatString withBlanksTrimmed.
	oldElementsBefore := self listAtCategoryNamed: oldCatString.
	SystemChangeNotifier uniqueInstance doSilently: [
		super renameCategory: oldCatString to: newCatString].
	
	oldElementsAfter := (self listAtCategoryNamed: oldCatString) asSet.
	oldElementsBefore do: [:each | (oldElementsAfter includes: each)
		ifFalse: [self notifyOfChangedSelector: each from: oldCatString to: newCategory]].
	
	self notifyOfChangedCategoryFrom: oldCatString to: newCategory.
]

{ #category : 'renaming' }
ClassOrganizer >> renamePackageCategoriesFrom: oldPackageName to: newPackageName [

	self categories do: [ :aCategory |
		(self isPackageCategoryExtension: aCategory fromPackage: oldPackageName) ifTrue: [
			self renamePackageCategory: aCategory from: oldPackageName to: newPackageName.
		]
	]
]

{ #category : 'renaming' }
ClassOrganizer >> renamePackageCategory: aCategory from: oldPackageName to: newPackageName [

	| newCategory |
			
	newCategory := aCategory copyReplaceFrom: 2 to: oldPackageName size + 1 with: newPackageName.
	self renameCategory: aCategory to: newCategory
]

{ #category : 'accessing' }
ClassOrganizer >> sortCategories [
	| oldCategories |
	oldCategories := self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super sortCategories].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
]

{ #category : 'accessing' }
ClassOrganizer >> unclassifiedSelectors: anArray [

	SystemChangeNotifier uniqueInstance doSilently: [
		categoryArray := {self class defaultCategory}.
		categoryStops := {anArray size}.
		elementArray := anArray ].
]
