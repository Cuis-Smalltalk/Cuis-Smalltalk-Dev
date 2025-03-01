'From Cuis7.3 [latest update: #7061] on 1 March 2025 at 5:31:30 pm'!

!TestResult methodsFor: 'running' stamp: 'jmv 3/1/2025 17:16:02'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed :=
		[
			[
				"self reportAboutToRun: aTestCase."
				aTestCase runCase.
				"self reportPassed: aTestCase."
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

