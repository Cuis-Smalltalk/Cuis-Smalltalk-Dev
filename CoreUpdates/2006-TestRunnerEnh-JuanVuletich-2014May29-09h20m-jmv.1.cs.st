'From Cuis 4.2 of 25 July 2013 [latest update: #2005] on 29 May 2014 at 9:21:01.716265 am'!

!TestRunner methodsFor: 'updating' stamp: 'jmv 5/27/2014 22:59'!
updateFailures: aTestResult

        self displayFailures: aTestResult failures! !


!TestResult methodsFor: 'Accessing' stamp: 'jmv 5/27/2014 22:57'!
failures
	failures
		ifNil: [failures := OrderedCollection new].
	^failures
			! !

!TestResult methodsFor: 'Running' stamp: 'jmv 5/27/2014 23:01'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed _
		[
			[
				aTestCase runCase.
				true]
					on: self class failure
					do: [ :signal |
						(self failures isEmpty or: [ (failures last == aTestCase) not ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: self class error
							do: [ :signal |
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]! !

