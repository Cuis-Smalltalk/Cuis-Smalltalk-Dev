"
This is a Composite of Tests, either TestCases or other TestSuites. The common protocol is #run: aTestResult and the dependencies protocol
"
Class {
	#name : 'TestSuite',
	#superclass : 'ActiveModel',
	#instVars : [
		'tests',
		'resources',
		'name'
	],
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'instance creation - private' }
TestSuite class >> allTestCaseClassesReferencing: aClass [

	^(aClass allCallsOn 
		collect: [ :aMethodReference | aMethodReference actualClass ]
		thenSelect: [ :aPotentialTestCaseClass | aPotentialTestCaseClass is: #TestCaseClass ]) asSet.
]

{ #category : 'instance creation - private' }
TestSuite class >> allTestsSending: aSelector [
	
	^(Smalltalk allCallsOn: aSelector) select: [:aMethodReference | 
		(aMethodReference actualClass is: #TestCaseClass) and: [aMethodReference compiledMethod isTestMethod ]].
]

{ #category : 'instance creation' }
TestSuite class >> forClass: aClass [

	^(aClass is: #TestCaseClass) 
		ifTrue: [ self forTestCaseClass: aClass ]
		ifFalse: [ self forNoTestCaseClass: aClass ]

]

{ #category : 'instance creation - private' }
TestSuite class >> forClasses: classes named: aName [

	| testMethods suite classTests tests testPrintString |
	
	"I don't want repeated tests. TestCase does not redefine #= so instead of redefining it and use a Set
	I decided to keep the related tests methods in a different set and decide to add it or note base on that
	- Hernan"
	
	testMethods := Set new.
	tests := OrderedCollection new.
	
	classes do: [ :aClass |
		classTests := (self forClass: aClass) tests.
		classTests do: [ :aTest | 
				testPrintString := aTest printString.
				(testMethods includes: testPrintString) ifFalse: [ 
					testMethods add: testPrintString.
					tests add: aTest ]]].
		
	suite := self named: aName.
	suite addTests: tests.
	
	^suite
		

]

{ #category : 'instance creation' }
TestSuite class >> forCompiledMethod: aCompiledMethod [

	^aCompiledMethod isTestMethod 
		ifTrue: [ self forTestMethod: aCompiledMethod ]
		ifFalse: [ self forNoTestMethod: aCompiledMethod ]
	
]

{ #category : 'instance creation' }
TestSuite class >> forMessageCategoryNamed: aMessageCategoryName of: aClass categorizedWith: aClassOrganizer [

	| suite |
	
	suite := self named: aClass name, ' tests categorized under ',aMessageCategoryName.
	(aClassOrganizer listAtCategoryNamed: aMessageCategoryName) do: [ :selector |
		(aClass compiledMethodAt: selector) isTestMethod ifTrue: [ suite addTest: (aClass selector: selector) ]].
	
	^suite
	
]

{ #category : 'instance creation - private' }
TestSuite class >> forNoTestCaseClass: aClass  [

	^aClass 
		withTestCaseClassDo: [ :aTestCaseClass | self forTestCaseClass: aTestCaseClass ]
		ifNone: [ self forReferencesToClass: aClass ]


]

{ #category : 'instance creation - private' }
TestSuite class >> forNoTestMethod: aCompiledMethod  [
	
	| allTestSenders testCaseClassesReferencingClass reducedTestSenders suite |
	
	allTestSenders := self allTestsSending: aCompiledMethod selector.
	testCaseClassesReferencingClass := aCompiledMethod methodClass 
		withTestCaseClassDo:  [:aTestCaseClass | Array with: aTestCaseClass ]
		ifNone: [ self allTestCaseClassesReferencing: aCompiledMethod methodClass ].
		
	reducedTestSenders := allTestSenders select: [ :aMethodReference | testCaseClassesReferencingClass includes: aMethodReference actualClass ].
	reducedTestSenders isEmpty 
		ifTrue: [ suite := self forClass: aCompiledMethod methodClass ]
		ifFalse: [ 
			suite := self named: 'Tests senders of ', aCompiledMethod selector.
			reducedTestSenders do: [ :aMethodReference | suite addTest: (aMethodReference actualClass selector: aMethodReference selector)]].
	
	^suite
]

{ #category : 'instance creation - private' }
TestSuite class >> forReferencesToClass: aClass  [

	| testCaseClasses |
	
	testCaseClasses := self allTestCaseClassesReferencing: aClass.
		
	^testCaseClasses 
		inject: (self named: aClass name, ' all test references')
		into: [ :suite :aTestCaseClass | aTestCaseClass addToSuiteFromSelectors: suite ]
	


]

{ #category : 'instance creation' }
TestSuite class >> forSystemCategoryNamed: aCategoryName  using: aSystemOrganizer [

	| testCaseClasses |

	testCaseClasses := aSystemOrganizer testCaseClassesAt: aCategoryName.

	^ testCaseClasses isEmpty 
		ifTrue: [ self forClasses: (aSystemOrganizer allClassesAt: aCategoryName) named: aCategoryName, ' infered tests' ]
		ifFalse: [ self forTestCaseClasses: testCaseClasses named: aCategoryName, ' tests' ]
		

]

{ #category : 'instance creation' }
TestSuite class >> forSystemCategoryTreeOf: aCategoryName  using: aSystemOrganizer [

	| rootCategory |

	rootCategory := aSystemOrganizer rootCategoryOf: aCategoryName.
	
	^self forSystemCategoryNamed: rootCategory using: aSystemOrganizer 
]

{ #category : 'instance creation - private' }
TestSuite class >> forTestCaseClass: aTestCaseClass  [

	| suite |
	
	suite := aTestCaseClass buildSuite.
	suite name: aTestCaseClass name, ' tests'.
	
	^suite
]

{ #category : 'instance creation - private' }
TestSuite class >> forTestCaseClasses: testCaseClasses named: aName [

	| suite |
	suite := self named: aName.
	testCaseClasses do: [ :aTestCaseClass |
			aTestCaseClass isAbstract
				ifFalse: [ aTestCaseClass addToSuiteFromSelectors: suite ]].
		
	^suite
]

{ #category : 'instance creation - private' }
TestSuite class >> forTestMethod: aCompiledMethod [
	
	| suite |
	
	suite := self named: 'Test'.
	suite addTest: (aCompiledMethod methodClass selector: aCompiledMethod selector).
	
	^suite
		
]

{ #category : 'instance creation' }
TestSuite class >> named: aString [

	^self new
		name: aString;
		yourself
			
]

{ #category : 'accessing' }
TestSuite >> addTest: aTest [
	self tests add: aTest
			
]

{ #category : 'accessing' }
TestSuite >> addTests: aCollection  [
	aCollection do: [:eachTest | self addTest: eachTest]
			
]

{ #category : 'accessing' }
TestSuite >> defaultResources [
	^self tests 
		inject: Set new
		into: [:coll :testCase | 
			coll
				addAll: testCase resources;
				yourself]
			
]

{ #category : 'accessing' }
TestSuite >> name [

	^name
			
]

{ #category : 'accessing' }
TestSuite >> name: aString [

	name := aString
			
]

{ #category : 'dependencies' }
TestSuite >> removeDependentFromHierachy: anObject [
	self removeDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]
			
]

{ #category : 'accessing' }
TestSuite >> resources [
	resources ifNil: [resources := self defaultResources].
	^resources
			
]

{ #category : 'accessing' }
TestSuite >> resources: anObject [
	resources := anObject
			
]

{ #category : 'running' }
TestSuite >> run [

	| result |
 
	result := TestResult new.
	
	[ self setUpResources ] 
		on: result class failure 
		do: [ :aFailure |
			result addAll: tests asFailureBecause: aFailure messageText.
			self tearDownResources.
			^result ].
		
	[self run: result] ensure: [self tearDownResources].
	
	^result
			
]

{ #category : 'running' }
TestSuite >> run: aResult [

	| skipFailures t1 t2 |
	t1 := Time localMillisecondClock.
	skipFailures := self tests size > 1.
	self tests do: [:each | 
		self changed: each.
		each run: aResult skipExpectedFailures: skipFailures.
		self changed: aResult.
		"Allow for some responsivenes of the UI, or any other stuff that may need running"
		t2 := Time localMillisecondClock.
		t2-t1 > 200 ifTrue: [
			Processor yield. 
			t1 := t2 ]]
]

{ #category : 'running - private' }
TestSuite >> setUpResources [
			
	self resources do: [ :res | res isAvailable ifFalse: [^res signalInitializationError]].
	
]

{ #category : 'running - private' }
TestSuite >> tearDownResources [
			
	self resources do: [:each | each reset]
]

{ #category : 'accessing' }
TestSuite >> tests [
	tests ifNil: [tests := OrderedCollection new].
	^tests
			
]
