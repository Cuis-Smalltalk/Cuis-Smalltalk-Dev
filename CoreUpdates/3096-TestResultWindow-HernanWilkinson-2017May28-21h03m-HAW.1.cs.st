'From Cuis 5.0 of 7 November 2016 [latest update: #3094] on 3 June 2017 at 8:52:25 pm'!
!classDefinition: #TestResultWindow category: #'Tools-Testing'!
MessageSetWindow subclass: #TestResultWindow
	instanceVariableNames: 'testResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 6/3/2017 20:40:29'!
openTestResultWindow

	TestResultWindow openFor: testResult 
	! !


!TestCase methodsFor: 'Testing' stamp: 'HAW 6/3/2017 20:26:43'!
isSameAs: aTestCase

	^self class = aTestCase class and: [ testSelector = aTestCase selector ]! !


!TestResult methodsFor: 'Accessing' stamp: 'HAW 6/3/2017 20:27:28'!
removeFromDefectsAndAddToPassed: aPassed 

	errors 
		detect: [ :anError | anError isSameAs: aPassed ]
		ifFound: [ :anError | errors remove: anError ]
		ifNone: [ 
			failures 
				detect: [ :aFail | aFail isSameAs: aPassed ]
				ifFound: [ :aFail | failures remove: aFail ]
				ifNone: [ self error: aPassed printString, ' is not an error nor a failure' ]].
	passed add: aPassed
! !


!TestResultWindow methodsFor: 'actions' stamp: 'HAW 6/3/2017 20:51:08'!
debug

	model selection ifNotNil: [ :selection | | test |
		test := selection actualClass selector: selection selector.
		test debug.
		testResult removeFromDefectsAndAddToPassed: test.
		model removeMessageFromBrowserKeepingLabel. 
		self setLabel: testResult printString ]! !

!TestResultWindow methodsFor: 'actions' stamp: 'HAW 6/3/2017 20:51:12'!
runSuite

	| suite |
	 
	suite := TestSuite new.
	suite addTests: testResult tests.
	self delete.
	(ProgessiveTestRunner for: suite) value.
	! !

!TestResultWindow methodsFor: 'button creation' stamp: 'HAW 6/3/2017 20:51:16'!
createDebugButton

	^PluggableButtonMorph 
		model: self
		stateGetter: #isMessageSelected
		action: #debug
		label: 'Debug'.
! !

!TestResultWindow methodsFor: 'button creation' stamp: 'HAW 6/3/2017 20:51:20'!
createReRunButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #runSuite
		label: 'Run Suite'.
! !

!TestResultWindow methodsFor: 'GUI building' stamp: 'HAW 6/3/2017 20:51:24'!
addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10! !

!TestResultWindow methodsFor: 'GUI building' stamp: 'HAW 6/3/2017 20:51:28'!
addButtonsTo: row color: buttonColor

	self addButton: self createDebugButton to: row color: buttonColor.
	self addButton: self createReRunButton to: row color: buttonColor.
	! !

!TestResultWindow methodsFor: 'GUI building' stamp: 'HAW 6/3/2017 20:51:32'!
buildLowerPanes

	| codeAndButtons  |

	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons ! !

!TestResultWindow methodsFor: 'GUI building' stamp: 'HAW 6/3/2017 20:51:36'!
buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	! !

!TestResultWindow methodsFor: 'initialization' stamp: 'HAW 6/3/2017 20:51:40'!
initializeFor: aTestResult 

	testResult := aTestResult ! !

!TestResultWindow methodsFor: 'testing' stamp: 'HAW 6/3/2017 20:51:46'!
isMessageSelected

	^model selection notNil ! !


!TestResultWindow class methodsFor: 'instance creation' stamp: 'HAW 6/3/2017 20:50:29'!
methodReferencesOf: tests

	^tests collect: [:aTest | MethodReference class: aTest class selector: aTest selector].
! !

!TestResultWindow class methodsFor: 'instance creation' stamp: 'HAW 6/3/2017 20:50:25'!
openFor: aTestResult

	| window |

	window := self openMessageList: (self methodReferencesOf: aTestResult defects) label: aTestResult printString.
	window initializeFor: aTestResult.
	
	^window 

! !


!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 6/3/2017 20:40:17'!
showDeffects
	
	 | defects |
			
	defects _ 	testResult defects.
	defects size = 1
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]! !

!methodRemoval: TestResult #forDebuggingAndInspection!
TestResult removeSelector: #forDebuggingAndInspection!
!methodMoveToSomePackage: TestCase #should:raise:withMessageText:!
TestCase removeSelectorIfInBaseSystem: #should:raise:withMessageText:!
!methodRemoval: ProgessiveTestRunner #openTestResultForDebuggingAndInspection!
ProgessiveTestRunner removeSelector: #openTestResultForDebuggingAndInspection!

!TestResultWindow class reorganize!
('instance creation' methodReferencesOf: openFor:)
!


!TestCase reorganize!
('Dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('Accessing' assert: assert:description: assert:description:resumable: assert:equals: defaultFailDescription deny: deny:description: deny:description:resumable: errored: fail failWith: failureString raisedError resources selector should: should:description: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: signalFailure:)
('Running' debug debugAsFailure debugAsFailureIfCanNot: failureLog isLogging logFailure: run run: runCase setUp setUpResources tearDown tearDownResources)
('Private' canNotDebugMethodErrorDescription comparingStringBetween:and: ifCanNotDebugDo: methodForTest openDebuggerOnFailingTestMethod performTest runCaseAsFailure setTestSelector: signalCanNotDebugMethod)
('Printing' printOn:)
('Testing' is: isSameAs:)
('*Refactoring' should:raise:withMessageText:)
!


!TestResult reorganize!
('Accessing' correctCount defects errorCount errors failureCount failures passed passedCount removeFromDefectsAndAddToPassed: runCount tests)
('Testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('Printing' printOn:)
('Running' runCase:)
!

!classRemoval: #TestResultForDebuggingAndInspection!
Smalltalk removeClassNamed: #TestResultForDebuggingAndInspection!
