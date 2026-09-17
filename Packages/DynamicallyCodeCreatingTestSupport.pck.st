'From Cuis7.9 [latest update: #8205] on 17 September 2026 at 3:21:50 pm'!
'Description '!
!provides: 'DynamicallyCodeCreatingTestSupport' 1 8!
SystemOrganization addCategory: #DynamicallyCodeCreatingTestSupport!


!classDefinition: #DynamicallyCodeCreatingTestData category: #DynamicallyCodeCreatingTestSupport!
Object subclass: #DynamicallyCodeCreatingTestData
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicallyCodeCreatingTestSupport'!
!classDefinition: 'DynamicallyCodeCreatingTestData class' category: #DynamicallyCodeCreatingTestSupport!
DynamicallyCodeCreatingTestData class
	instanceVariableNames: ''!

!classDefinition: #DynamicallyCodeCreatingTest category: #DynamicallyCodeCreatingTestSupport!
TestCase subclass: #DynamicallyCodeCreatingTest
	instanceVariableNames: 'classCategories setUpAssertionsPassed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicallyCodeCreatingTestSupport'!
!classDefinition: 'DynamicallyCodeCreatingTest class' category: #DynamicallyCodeCreatingTestSupport!
DynamicallyCodeCreatingTest class
	instanceVariableNames: ''!


!DynamicallyCodeCreatingTestData class methodsFor: 'compiling' stamp: 'jmv 24/Aug/2026, 1:45:05 pm (UTC)'!
wantsChangeSetLogging 

	^false! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:31:27 pm (UTC)'!
allClassCategoriesOfTestData

	^Array with: self classCategoryOfTestData ! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:31:31 pm (UTC)'!
classCategoryOfTestData
	
	"I can not call it testDataClassCategory because it will be taken as test!! - Hernan"
	
	^self class classCategoryOfTestData! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
createClassNamed: aName 

	^self 
		createClassNamed: aName asSymbol "Just in case it is a string... - Hernan"
		subclassOf: self defaultSuperclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
createClassNamed: aName category: aCategory

	^self 
		createClassNamed: aName
		subclassOf: self defaultSuperclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
createClassNamed: aName instanceVariableNames: instanceVariables

	^self 
		createClassNamed: aName
		subclassOf: self defaultSuperclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:31:49 pm (UTC)'!
createClassNamed: aName subclassOf: superclass 

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:31:53 pm (UTC)'!
createClassNamed: aName subclassOf: superclass category: aCategory

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: '' 
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategory 
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:31:58 pm (UTC)'!
createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables

	^self 
		createClassNamed: aName
		subclassOf: superclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 8/Jul/2024, 1:34:28 pm (UTC)'!
createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables classVariableNames: classVariables

	^self createClassNamed: aName 
		subclassOf: superclass 
		instanceVariableNames: instanceVariables 
		classVariableNames: classVariables 
		poolDictionaries: '' 
		category: self classCategoryOfTestData 
! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 11/Mar/2025, 10:49:47 pm (UTC)'!
createClassNamed: aName subclassOf: superclass instanceVariableNames: instanceVariables classVariableNames: classVariables poolDictionaries: poolDictionaries category: category

	self assert: (Smalltalk classNamed: aName) isNil description: 'Class ', aName, ' already exists'.
	
	classCategories add: category.
	
	^SystemChangeNotifier uniqueInstance doSilently: [ 
		superclass
			subclass: aName
			instanceVariableNames: instanceVariables 
			classVariableNames: classVariables 
			poolDictionaries: poolDictionaries 
			category: category ]! !

!DynamicallyCodeCreatingTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
defaultSuperclass

	^ DynamicallyCodeCreatingTestData! !

!DynamicallyCodeCreatingTest methodsFor: 'assertions' stamp: 'HAW 17/Dec/2019, 2:32:57 pm (UTC)'!
assertAllClassCategoriesAreExpectedToBeRemoved

	"I'm using a halt instead of assert or error because I want the programmer to see what class category 
	was not defined in allClassCategoriesOfTestData - Hernan"
	(classCategories difference: self allClassCategoriesOfTestData) notEmpty ifTrue: [ self halt: #allClassCategoriesOfTestData asString, ' is not implemented correctly' ]
	! !

!DynamicallyCodeCreatingTest methodsFor: 'assertions' stamp: 'HAW 17/Dec/2019, 2:33:05 pm (UTC)'!
assertCanRunTest
	
	self assertNoTestDataClassCategoryExist.
	! !

!DynamicallyCodeCreatingTest methodsFor: 'assertions' stamp: 'HAW 11/Mar/2025, 10:33:45 pm (UTC)'!
assertClassCategoryIsEmpty: aClassCategory
		
	self 
		assert: (SystemOrganization listAtCategoryNamed: aClassCategory) isEmpty 
		description: 'Can not run test because class category ', aClassCategory, ' is not empty'! !

!DynamicallyCodeCreatingTest methodsFor: 'assertions' stamp: 'HAW 11/Mar/2025, 10:30:05 pm (UTC)'!
assertNoTestDataClassCategoryExist

	self allClassCategoriesOfTestData do: [ :aClassCategory | self assertClassCategoryIsEmpty: aClassCategory ]
		! !

!DynamicallyCodeCreatingTest methodsFor: 'setup/teardown' stamp: 'jmv 17/Sep/2026, 6:18:02 pm (UTC)'!
cleanUpGeneratedCode

	classCategories ifNotNil: [
		
		self assertAllClassCategoriesAreExpectedToBeRemoved.
		
		"I'm doing this instead of SystemOrganization removeSystemCategories: classCategories
		because it is faster - Hernan"
		SystemChangeNotifier uniqueInstance doSilently: [
			classCategories do: [ :aCategory | 
				(SystemOrganization classesAt: aCategory) do: [ :aClass | aClass removeFromSystem ]]]].

	Smalltalk cleanOutUndeclared.! !

!DynamicallyCodeCreatingTest methodsFor: 'setup/teardown' stamp: 'HAW 20/Aug/2026, 9:51:44 pm (UTC)'!
cleanUpInstanceVariables

	TestCase instSize+1 
		to: self class instSize 
		do: [ :anInstVarIndex | self instVarAt: anInstVarIndex put: nil ]! !

!DynamicallyCodeCreatingTest methodsFor: 'setup/teardown' stamp: 'jmv 17/Sep/2026, 6:15:16 pm (UTC)'!
setUp

	super setUp.
	UISupervisor whenUIinSafeState: [ Utilities authorInitials ].
	self cleanUpGeneratedCode.
	setUpAssertionsPassed := false.
	self assertCanRunTest.
	setUpAssertionsPassed := true.
	classCategories := Set new.! !

!DynamicallyCodeCreatingTest methodsFor: 'setup/teardown' stamp: 'HAW 20/Aug/2026, 9:53:17 pm (UTC)'!
tearDown

	setUpAssertionsPassed ifTrue: [ self cleanUpGeneratedCode ].
	
	self cleanUpInstanceVariables.
! !

!DynamicallyCodeCreatingTest class methodsFor: 'class factory' stamp: 'HAW 17/Dec/2019, 2:34:07 pm (UTC)'!
classCategoryOfTestData
	
	^ '__Refactoring_TestData__'! !

!DynamicallyCodeCreatingTest class methodsFor: 'testing' stamp: 'jmv 27/Jun/2022, 4:18:52 pm (UTC)'!
isAbstract

	^self subclasses notEmpty! !
