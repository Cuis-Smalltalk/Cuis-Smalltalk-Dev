'From Cuis 5.0 of 7 November 2016 [latest update: #3598] on 6 February 2019 at 1:38:28 pm'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 2/6/2019 12:17:47'!
should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: anExpectedErrorMessageCreator

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException | self assert: anExpectedErrorMessageCreator value equals: anException messageText ]! !


!TestCase reorganize!
('Dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('Accessing' defaultFailDescription errored: fail failWith: failureString raisedError resources selector signalFailure:)
('Running' debug debugAsFailure debugAsFailureIfCanNot: failureLog isLogging logFailure: run run: runCase setUp setUpResources tearDown tearDownResources)
('Private' canNotDebugMethodErrorDescription comparingStringBetween:and: ifCanNotDebugDo: methodForTest openDebuggerOnFailingTestMethod performTest runCaseAsFailure setTestSelector: signalCanNotDebugMethod)
('Printing' printOn:)
('Testing' is: isSameAs:)
('assertions' assert: assert:description: assert:description:resumable: assert:equals: deny: deny:description: deny:description:resumable: should: should:description: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description:)
!

