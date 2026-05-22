Class {
	#name : 'TestSuiteTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Testing',
	#package : 'BaseImageTests',
	#tag : 'Tools-Testing'
}

{ #category : 'assertions' }
TestSuiteTest >> assert: expectedTestSuite hasSameTestsAs: resultTestSuite [

	| expectedTests resultTests |
	
	expectedTests := expectedTestSuite tests.
	resultTests := resultTestSuite tests.
	
	self assert: resultTests size equals: expectedTests size.
	self assert: (expectedTests allSatisfy: [:expectedTest | resultTests anySatisfy: [:resultTest | self is: expectedTest equalTo: resultTest ]]).
]

{ #category : 'assertions' }
TestSuiteTest >> assertIncludesThisTest: aTestSuite  [
	
	| thisTestSelector |
	
	thisTestSelector _ thisContext sender selector.
	^aTestSuite tests anySatisfy: [ :aTestCase | aTestCase class = self class and: [aTestCase selector = thisTestSelector ]]
]

{ #category : 'test support' }
TestSuiteTest >> hardCodedReferenceToSelfClass [

	^TestSuiteTest 
]

{ #category : 'assertions' }
TestSuiteTest >> is: expectedTest equalTo: resultTest [

	^expectedTest class = resultTest class and: [ expectedTest selector =  resultTest selector ]
]

{ #category : 'test support' }
TestSuiteTest >> quickMethodTest [

	"stub for a quick method - Hernan"
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test01SuiteForTestCaseClassHasTheSameTestAsCreatingTheSuiteFromTheTestCaseClass [

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: self class)
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test02SuiteForNoTestCaseClassUsesTestClassWithSameNameEndingWithTest [

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: TestSuite)
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test03SuiteForNoTestCaseClassAndNoTestClassLooksForReferencesInTests [

	| objectTestSuite referencesToObject testCaseClasses |
	
	"I need to be sure that Object has no test class for the test to make sense - Hernan"
	self assert: self class testCaseClass isNil.
	
	objectTestSuite _ TestSuite forClass: self class.
	referencesToObject _ self class allCallsOn collect: [:aMethodReference | aMethodReference actualClass ]. 
	testCaseClasses _ (objectTestSuite tests collect: [:aTestCase | aTestCase class ]) asSet.

	self assert: objectTestSuite tests notEmpty.
	self assert: (testCaseClasses allSatisfy: [:aTestCaseClass | (aTestCaseClass is: #TestCaseClass) and: [ referencesToObject includes: aTestCaseClass ]])
	
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test04SuiteForTestMethodIncludesOnlyTheMethod [

	self assert: (TestSuite new addTest: (self class selector: thisContext selector)) hasSameTestsAs: (TestSuite forCompiledMethod: thisContext method)
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test05SuiteForNoTestMethodIncludesTestCaseSendingTheCompiledMethodSelector [

	| compiledMethodSuite compiledMethodSelector senders |
	
	compiledMethodSelector _ #addTest:.
	compiledMethodSuite _ TestSuite forCompiledMethod: (TestSuite compiledMethodAt: compiledMethodSelector).
	senders _ (Smalltalk allCallsOn: compiledMethodSelector) collect: [:aMethodReference | aMethodReference selector ].
	
	self assert: compiledMethodSuite tests notEmpty.
	self assert: (compiledMethodSuite tests allSatisfy: [:aTestCase | (aTestCase class is: #TestCaseClass) and: [ senders includes: aTestCase selector]])
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test06ForSystemCategoryWithTestCasesIncludesOnlyTestCasesClasses [

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: self class category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite 
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test07ForSystemCategoryWithNoTestCasesIncludesTestClassesTests [

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: TestSuite category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite 
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test08ForMessageCategoryContainingTestCasesIncludesThoseTestCases [

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: thisContext selector) of: self class categorizedWith: classOrganizer.

	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite 
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test09ForMessageCategoryWihoutTestIsEmpty [

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: #assert:hasSameTestsAs:) of: self class categorizedWith: classOrganizer.

	self assert: suite tests isEmpty
]

{ #category : 'tests - instance creation' }
TestSuiteTest >> test10SuiteForClassCategoryTreeIncludesAllTestsInCategoryTree [

	| categoryTreeSuite suiteFromCategoryRoot |
	
	categoryTreeSuite := TestSuite forSystemCategoryTreeOf: self class category using: SystemOrganization.
	suiteFromCategoryRoot := TestSuite forSystemCategoryNamed: (SystemOrganization rootCategoryOf: self class category) using: SystemOrganization.
	
	self 
		assert: (categoryTreeSuite tests collect: [ :test | test methodForTest ]) 
		equals: (suiteFromCategoryRoot tests collect: [ :test | test methodForTest ])
]
