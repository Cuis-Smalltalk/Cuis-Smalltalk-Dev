'From Cuis 6.0 [latest update: #5810] on 25 May 2023 at 8:19:32 pm'!

!TestCase methodsFor: 'debugging' stamp: 'HAW 5/25/2023 20:15:40'!
debugInstance
	^ self class selector: testSelector! !


!TestRunnerWindow methodsFor: 'GUI building' stamp: 'jmv 3/12/2018 15:57:20'!
buildMorphicWindow

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: `460 @ 400`! !


!TestCase methodsFor: 'debugging' stamp: 'HAW 5/25/2023 20:15:40'!
debug
	self setUpResources.
	[self debugInstance runCase] ensure: [self tearDownResources]
			! !

!TestCase methodsFor: 'debugging' stamp: 'HAW 5/25/2023 20:15:40'!
debugAsFailureIfCanNot: handler
	self ifCanNotDebugDo: [^ handler value].
	self debugInstance runCaseAsFailure! !

!methodRemoval: TestCase #copy stamp: 'HAW 5/25/2023 20:15:51'!
TestCase removeSelector: #copy!

!TestCase reorganize!
('accessing' errored: methodForTest raisedError resources selector setTestSelector:)
('assertions' assert: assert:changes: assert:changes:by: assert:changes:from:to: assert:description: assert:description:resumable: assert:doesNotChange: assert:equals: assert:equalsExpected: assert:includes: assert:isCloseTo: assert:isCloseTo:withPrecision: assert:isNotCloseTo: assert:isNotCloseTo:withPrecision: comparingStringBetween:and: defaultPrecision deny: deny:description: deny:description:resumable: is:closeTo:withPrecision: should: should:description: should:notTakeMoreThan: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldFail: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: shouldntFail:)
('debugging' debug debugAsFailure debugAsFailureIfCanNot: debugInstance)
('debugging - private' canNotDebugMethodErrorDescription ifCanNotDebugDo: openDebuggerOnFailingTestMethod signalCanNotDebugMethod)
('dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('failing' defaultFailDescription expectedFailures fail failWith: failureString signalFailure:)
('log' failureLog logFailure:)
('printing' printOn:)
('running' run run:skipExpectedFailures: runCase runCaseAsFailure)
('running - private' performTest)
('setUp/tearDown' setUp setUpResources tearDown tearDownResources)
('testing' is: isLogging isSameAs:)
!

