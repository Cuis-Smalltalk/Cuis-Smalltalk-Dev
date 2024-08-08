'From Cuis7.1 [latest update: #6571] on 8 August 2024 at 12:34:10 pm'!

!SUnitNameResolver class methodsFor: 'Camp Smalltalk' stamp: 'jmv 8/8/2024 12:19:22'!
errorObject
	^Error! !


!TestResult methodsFor: 'running' stamp: 'jmv 8/8/2024 12:09:39'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed :=
		[
			[
				self reportAboutToRun: aTestCase.
				aTestCase runCase.
				self reportPassed: aTestCase.
				true]
					on: self class failure
					do: [ :signal |
						self reportFailed: aTestCase because: signal.
						(self failures isEmpty or: [ failures last ~~ aTestCase ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: UnhandledError
							do: [ :signal |
								self reportError: aTestCase because: signal.
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]! !


!TestResult class methodsFor: 'exceptions' stamp: 'jmv 8/8/2024 12:12:24'!
signalErrorWith: aString 
	TestResult exError sunitSignalWith: aString.! !

!methodRemoval: TestResult class #error stamp: 'jmv 8/8/2024 12:33:15'!
TestResult class removeSelector: #error!
