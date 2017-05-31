'From Cuis 5.0 of 7 November 2016 [latest update: #3083] on 28 May 2017 at 8:07:38 pm'!

!TestCase methodsFor: 'Running' stamp: 'HAW 5/28/2017 20:01:43'!
setUpResources
			
	self resources do: [:res | res isAvailable ifFalse: [^res signalInitializationError]].
! !

!TestCase methodsFor: 'Running' stamp: 'HAW 5/28/2017 20:02:07'!
tearDownResources
	
	self resources do: [:each | each reset]! !

!TestCase methodsFor: 'Private' stamp: 'HAW 5/28/2017 20:03:22'!
runCaseAsFailure

	self setUpResources.
	self setUp.
	
	self openDebuggerOnFailingTestMethod! !


!TestSuite methodsFor: 'Running - Private' stamp: 'HAW 5/28/2017 20:04:46'!
setUpResources
			
	self resources do: [ :res | res isAvailable ifFalse: [^res signalInitializationError]].
	! !

!TestSuite methodsFor: 'Running - Private' stamp: 'HAW 5/28/2017 20:05:08'!
tearDownResources
			
	self resources do: [:each | each reset]! !


!TestCase methodsFor: 'Running' stamp: 'HAW 5/28/2017 20:01:57'!
debug

	self setUpResources.
	
	[(self class selector: testSelector) runCase] ensure: [self tearDownResources]
			! !

!TestCase methodsFor: 'Running' stamp: 'HAW 5/28/2017 20:06:48'!
debugAsFailureIfCanNot: handler
	
	self ifCanNotDebugDo: [ ^handler value].
	
	(self class selector: testSelector) runCaseAsFailure! !

!TestCase methodsFor: 'Private' stamp: 'HAW 5/28/2017 16:11:59'!
executeShould: aBlock inScopeOf: anExceptonHandlingCondition 
	
	^self executeShould: aBlock inScopeOf: anExceptonHandlingCondition withExceptionDo: [:anException | ]
! !

!TestCase methodsFor: 'Private' stamp: 'HAW 5/28/2017 16:12:11'!
executeShould: aBlock inScopeOf: anExceptonHandlingCondition withExceptionDo: assertionsBlock

	^[aBlock value.
 	false] 
		on: anExceptonHandlingCondition
		do: [:exception | 
			assertionsBlock value: exception.
			exception sunitExitWith: true]! !

!TestCase methodsFor: 'Private' stamp: 'HAW 5/28/2017 20:03:03'!
openDebuggerOnFailingTestMethod

	| processToDebug context compiledMethod debugger |

	compiledMethod _ self methodForTest.
	
	processToDebug _ [ [ self performTest ] ensure: [
		self tearDown.
		self tearDownResources]] newProcess.
	context _ processToDebug suspendedContext.
	
	debugger _ Debugger new
		process: processToDebug 
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod] whileFalse: [debugger send].
! !


!TestSuite methodsFor: 'Running' stamp: 'HAW 5/28/2017 20:04:59'!
run

	| result |
 
	result := TestResult new.
	self setUpResources.	
	[self run: result] ensure: [self tearDownResources].
	
	^result
			! !

!methodRemoval: TestCase #openDebuggerOnFailingTestMethod2!
TestCase removeSelector: #openDebuggerOnFailingTestMethod2!
!methodRemoval: TestCase #openDebuggerOnFailingTestMethod:!
TestCase removeSelector: #openDebuggerOnFailingTestMethod:!
!methodRemoval: TestCase #runCaseAsFailure:!
TestCase removeSelector: #runCaseAsFailure:!

!TestSuite reorganize!
('Dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('Accessing' addTest: addTests: defaultResources name name: resources resources: tests)
('Running' run run:)
('Running - Private' setUpResources tearDownResources)
!

!classRemoval: #TestCaseDebugger!
Smalltalk removeClassNamed: #TestCaseDebugger!
