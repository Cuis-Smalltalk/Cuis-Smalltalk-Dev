'From Cuis 5.0 [latest update: #3871] on 16 September 2019 at 11:53:10 am'!

!TestResult methodsFor: 'running' stamp: 'FJG 9/16/2019 11:52:06'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed _
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
							on: self class error
							do: [ :signal |
								self reportError: aTestCase because: signal.
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]! !

!TestResult methodsFor: 'logging' stamp: 'FJG 9/16/2019 11:52:20'!
reportAboutToRun: aTestCase

	Transcript show: 'Will run: '; print: aTestCase; newLine! !

!TestResult methodsFor: 'logging' stamp: 'FJG 9/16/2019 11:52:23'!
reportError: aTestCase because: anException

	Transcript print: anException; newLine.! !

!TestResult methodsFor: 'logging' stamp: 'FJG 9/16/2019 11:52:27'!
reportFailed: aTestCase because: anException

	Transcript print: anException; newLine.
	! !

!TestResult methodsFor: 'logging' stamp: 'FJG 9/16/2019 11:52:30'!
reportPassed: aTestCase

	Transcript show: 'finished.'; newLine! !

