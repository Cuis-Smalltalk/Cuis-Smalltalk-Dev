'From Cuis 4.1 of 12 December 2012 [latest update: #1518] on 21 December 2012 at 11:51:52 am'!

!Categorizer methodsFor: 'testing' stamp: 'jmv 12/21/2012 11:24'!
hasAnyCategoriesSuchThat: aBlock
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ false].
	(categoryArray size = 1 
		and: [categoryArray first = Default & (elementArray size = 0)])
		ifTrue: [^false].
	^categoryArray anySatisfy: aBlock! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 12/21/2012 11:50'!
hasAnyExtensionCategoriesForClass: aClassOrMetaClass
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization hasAnyCategoriesSuchThat: [ :cat |
		self isYourClassExtension: cat ]! !


!CodePackage methodsFor: 'modifying' stamp: 'jmv 12/21/2012 11:36'!
externalBehaviors
	"As Cuis doesn't support Traits, answer just external classes and metaclasses"
	^self externalClasses" , self externalTraits"! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 12/21/2012 11:45'!
extensionClassNamesIn: aSystemCategory

	^ (SystemOrganization listAtCategoryNamed: aSystemCategory) select: [ :className | | cls |
		cls _ Smalltalk at: className.
		(self hasAnyExtensionCategoriesForClass: cls) or: [
			self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 12/21/2012 11:48'!
extensionClasses
	"Classes and metaClasses for which we do define extensions.
	Include a class if we define some instance method.
	Include a metaclass if we define some class method."
	^ self externalBehaviors select: [ :classOrMetaClass |
		self hasAnyExtensionCategoriesForClass: classOrMetaClass ]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 12/21/2012 11:51'!
extensionMethods
	"Include both class and instance methods we define, for classes we don't define."
	^ self externalBehaviors gather: [ :classOrMetaClass |
		self extensionMethodsForClass: classOrMetaClass ]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 12/21/2012 11:43'!
systemCategoriesWithExtensionMethods

	^ SystemOrganization categories select: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) anySatisfy: [ :className | | cls |
			cls _ Smalltalk at: className.
			(self hasAnyExtensionCategoriesForClass: cls) or: [
				self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 12/21/2012 11:50'!
extensionCategoriesForClass: aClassOrMetaClass
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization categories select: [:cat |
		self isYourClassExtension: cat]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 12/21/2012 11:45'!
extensionMethodsForClass: aClassOrMetaClass
	"Pass the class as the argument for instance side methods.
	Pass the metaclass as the argument for class side methods	"
	^ (self extensionCategoriesForClass: aClassOrMetaClass) gather: [ :cat |
		self methodsInCategory: cat ofClass: aClassOrMetaClass ]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 12/21/2012 11:39'!
extensionMethodsFromClasses: classesAndMetaClasses
	"Pass the class as the argument for instance side methods.
	Pass the metaclass as the argument for class side methods	"
	^classesAndMetaClasses
		gather: [ :classOrMetaClass | self extensionMethodsForClass: classOrMetaClass ]! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 12/21/2012 11:31'!
externalClasses
	"Answer Classes and Metaclasses not defined in self."
	| myClassesAndMetaClasses |
	myClassesAndMetaClasses _ self classesAndMetaClasses.
	^ Array streamContents: [ :s |
		ProtoObject withAllSubclassesDo: [ :classOrMetaClass |
			(myClassesAndMetaClasses includes: classOrMetaClass) ifFalse: [
				s nextPut: classOrMetaClass ]]]! !


!Categorizer reorganize!
('accessing' addCategory: addCategory:before: allMethodSelectors categories categories: categoryOfElement: changeFromCategorySpecs: changeFromString: classify:under: classify:under:suppressIfDefault: classifyAll:under: elementCategoryDict isEmptyCategoryNumber: listAtCategoryNamed: listAtCategoryNumber: moveCategoryBottom: moveCategoryDown: moveCategoryTop: moveCategoryUp: numberOfCategoryOfElement: removeCategory: removeElement: removeEmptyCategories renameCategory:toBe: sortCategories)
('printing' printOn:)
('fileIn/Out' scanFrom:)
('private' elementArray firstIndexOfCategoryNumber: lastIndexOfCategoryNumber: setDefaultList:)
('copying' postCopy)
('testing' hasAnyCategoriesSuchThat:)
!

