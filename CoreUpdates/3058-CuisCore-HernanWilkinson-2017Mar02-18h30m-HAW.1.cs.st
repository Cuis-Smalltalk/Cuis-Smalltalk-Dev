'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 3 March 2017 at 7:15:45 pm'!

!TestCase methodsFor: 'Running' stamp: 'HAW 3/3/2017 19:13:13'!
debugAsFailureIfCanNot: handler
	
	| semaphore |
	
	self ifCanNotDebugDo: [ ^handler value].
	
	semaphore := Semaphore new.
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[semaphore wait. 
	self tearDown.
	self resources do: [:each | each reset]] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore.! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 14:13:09'!
canNotDebugMethodErrorDescription

	^self class canNotDebugMethodErrorDescription! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 18:50:08'!
ifCanNotDebugDo: handler

	^self testMethod isQuick ifTrue: handler! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 14:13:37'!
signalCanNotDebugMethod 

	self error: self canNotDebugMethodErrorDescription! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 14:10:20'!
testMethod 

	^self class lookupSelector: self selector! !


!TestCase class methodsFor: 'Instance Creation' stamp: 'HAW 3/3/2017 18:51:38'!
debugAsFailure: aSymbol ifCanNot: handler

	^(self selector: aSymbol) debugAsFailureIfCanNot: handler
	! !

!TestCase class methodsFor: 'Error Descriptions' stamp: 'HAW 3/3/2017 16:33:00'!
canNotDebugMethodErrorDescription

	^'Quick methods can not be debugged'! !


!Browser methodsFor: 'tdd' stamp: 'HAW 3/3/2017 19:12:25'!
debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ifCanNot: [ PopUpMenu inform: TestCase canNotDebugMethodErrorDescription ]]
! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 3/3/2017 19:12:39'!
acceptAndDebugTest
	
	^self acceptAndWithMethodDo: [ :aPotencialTestMethod | 
		aPotencialTestMethod isTestMethod ifTrue: [ 
			aPotencialTestMethod methodClass debugAsFailure: aPotencialTestMethod selector ifCanNot: [ PopUpMenu inform: TestCase canNotDebugMethodErrorDescription ]]]! !


!TestCase methodsFor: 'Running' stamp: 'HAW 3/3/2017 18:49:24'!
debugAsFailure
	
	^self debugAsFailureIfCanNot: [ self signalCanNotDebugMethod ]! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 19:13:40'!
openDebuggerOnFailingTestMethod

	| guineaPig context compiledMethod debugger |

	compiledMethod _ self testMethod.
	guineaPig _ [ self performTest ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ Debugger new
		process: guineaPig
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/3/2017 19:14:27'!
openDebuggerOnFailingTestMethod: semaphore

	| guineaPig context compiledMethod debugger |

	compiledMethod _ self testMethod.
	guineaPig _ [ self performTest ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ TestCaseDebugger new
		process: guineaPig
		context: context.
	debugger doneSemaphore: semaphore.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
! !

!methodRemoval: TestCase class #debugAsFailure:!
TestCase class removeSelector: #debugAsFailure:!
!methodRemoval: TestCase #assertCanDebugMethod!
TestCase removeSelector: #assertCanDebugMethod!
!methodRemoval: TestCase #canNotDebugQuickMethodErrorDescription!
TestCase removeSelector: #canNotDebugQuickMethodErrorDescription!
!methodRemoval: TestCase #signalCanNotDebugQuickMethod!
TestCase removeSelector: #signalCanNotDebugQuickMethod!

!TestCase reorganize!
('Dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('Accessing' assert: assert:description: assert:description:resumable: assert:equals: deny: deny:description: deny:description:resumable: errored: failureString raisedError resources selector should: should:description: should:raise: should:raise:description: should:raise:withExceptionDo: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: signalFailure:)
('Running' debug debugAsFailure debugAsFailureIfCanNot: failureLog isLogging logFailure: run run: runCase setUp tearDown)
('Private' canNotDebugMethodErrorDescription comparingStringBetween:and: executeShould:inScopeOf: executeShould:inScopeOf:withExceptionDo: ifCanNotDebugDo: openDebuggerOnFailingTestMethod openDebuggerOnFailingTestMethod: performTest runCaseAsFailure: setTestSelector: signalCanNotDebugMethod testMethod)
('Printing' printOn:)
('Testing' is:)
!


!TestCase class reorganize!
('Accessing' allTestSelectors resources sunitVersion testSelectors)
('Building Suites' addTestsFor:toSuite: addToSuite:fromMethods: addToSuiteFromSelectors: buildSuite buildSuiteFromAllSelectors buildSuiteFromLocalSelectors buildSuiteFromMethods: buildSuiteFromSelectors suiteClass)
('Instance Creation' debug: debugAsFailure:ifCanNot: run: selector:)
('Testing' is: isAbstract shouldInheritSelectors)
('Error Descriptions' canNotDebugMethodErrorDescription)
!

