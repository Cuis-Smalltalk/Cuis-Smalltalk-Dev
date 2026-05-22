Class {
	#name : 'DynamicallyCodeCreationTest',
	#superclass : 'TestCase',
	#instVars : [
		'classCategories',
		'setUpAssertionsPassed'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
DynamicallyCodeCreationTest class >> classCategoryOfTestData [
	
	^ '__Refactoring-TestData__'
]

{ #category : 'testing' }
DynamicallyCodeCreationTest class >> isAbstract [

	^self subclasses notEmpty
]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> allClassCategoriesOfTestData [

	^Array with: self classCategoryOfTestData 
]

{ #category : 'assertions' }
DynamicallyCodeCreationTest >> assertAllClassCategoriesAreExpectedToBeRemoved [

	"I'm using a halt instead of assert or error because I want the programmer to see what class category 
	was not defined in allClassCategoriesOfTestData - Hernan"
	(classCategories difference: self allClassCategoriesOfTestData) notEmpty ifTrue: [ self halt: #allClassCategoriesOfTestData asString, ' is not implemented correctly' ]
	
]

{ #category : 'assertions' }
DynamicallyCodeCreationTest >> assertCanRunTest [
	
	self assertNoTestDataClassCategoryExist.
	
]

{ #category : 'assertions' }
DynamicallyCodeCreationTest >> assertClassCategoryIsEmpty: aClassCategory [
		
	self 
		assert: (SystemOrganization listAtCategoryNamed: aClassCategory) isEmpty 
		description: 'Can not run test because class category ', aClassCategory, ' is not empty'
]

{ #category : 'assertions' }
DynamicallyCodeCreationTest >> assertNoTestDataClassCategoryExist [

	self allClassCategoriesOfTestData do: [ :aClassCategory | self assertClassCategoryIsEmpty: aClassCategory ]
		
]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> classCategoryOfTestData [
	
	"I can not call it testDataClassCategory because it will be taken as test! - Hernan"
	
	^self class classCategoryOfTestData
]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName  [

	^self 
		createClassNamed: aName asSymbol "Just in case it is a string... - Hernan"
		subclassOf: self defaultSuperclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName category: aCategory [

	^self 
		createClassNamed: aName
		subclassOf: self defaultSuperclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName instanceVariableNames: instanceVariables [

	^self 
		createClassNamed: aName
		subclassOf: self defaultSuperclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.


]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName subclassOf: superclass  [

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName subclassOf: superclass category: aCategory [

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables [

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables classVariableNames: classVariables [

	^self createClassNamed: aName 
		subclassOf: superclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: classVariables 
		poolDictionaries: '' 
		category: self classCategoryOfTestData 

]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables classVariableNames: classVariables poolDictionaries: poolDictionaries category: category [

	self assert: (Smalltalk classNamed: aName) isNil description: 'Class ', aName, ' already exists'.
	
	classCategories add: category.
	
	^SystemChangeNotifier uniqueInstance doSilently: [ 
		superclass
			subclass: aName
			instanceVariableNames: instanceVariables 
			classVariableNames: classVariables 
			poolDictionaries: poolDictionaries 
			category: category ]
]

{ #category : 'class factory' }
DynamicallyCodeCreationTest >> defaultSuperclass [

	^ RefactoringClassTestData
]

{ #category : 'setup/teardown' }
DynamicallyCodeCreationTest >> setUp [

	super setUp.
	"If the authorInitials are not set, they will be asked - Hernan"
	UISupervisor whenUIinSafeState: [ Utilities authorInitials ].
	"Because I'm using the same system to create classes and removed them with its category at tearDown, I want to be sure 
	I will not remove something I don't have too - Hernan"
	setUpAssertionsPassed := false.
	self assertCanRunTest.
	setUpAssertionsPassed := true.
	
	classCategories := Set new.
]

{ #category : 'setup/teardown' }
DynamicallyCodeCreationTest >> tearDown [

	setUpAssertionsPassed ifTrue: [
		self assertAllClassCategoriesAreExpectedToBeRemoved.
		
		"I'm doing this instead of SystemOrganization removeSystemCategories: classCategories
		because it is faster - Hernan"
		SystemChangeNotifier uniqueInstance doSilently: [
			classCategories do: [ :aCategory | 
				(SystemOrganization classesAt: aCategory) do: [ :aClass | aClass removeFromSystem ]]]]
]
