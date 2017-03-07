'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 7 March 2017 at 3:18:07 pm'!

!TestCase methodsFor: 'Private' stamp: 'HAW 3/6/2017 14:55:23'!
methodForTest

	"Can not call it testMethod because it will be detected as test - Hernan" 

	^self class lookupSelector: self selector! !


!TestCase methodsFor: 'Private' stamp: 'HAW 3/6/2017 14:55:38'!
ifCanNotDebugDo: handler

	^self methodForTest isQuick ifTrue: handler! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/6/2017 14:55:45'!
openDebuggerOnFailingTestMethod

	| guineaPig context compiledMethod debugger |

	compiledMethod _ self methodForTest.
	guineaPig _ [ self performTest ] newProcess.
	context _ guineaPig suspendedContext.
	debugger _ Debugger new
		process: guineaPig
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod]
		whileFalse: [debugger send].
! !

!TestCase methodsFor: 'Private' stamp: 'HAW 3/6/2017 14:55:50'!
openDebuggerOnFailingTestMethod: semaphore

	| guineaPig context compiledMethod debugger |

	compiledMethod _ self methodForTest.
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

!methodRemoval: TestCase #testMethod!
TestCase removeSelector: #testMethod!
