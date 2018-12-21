'From Cuis 5.0 of 7 November 2016 [latest update: #3529] on 20 December 2018 at 10:58:38 am'!

!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 12/20/2018 10:52:46'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^aParsingErrorBlock value: nil! !


!TestResult methodsFor: 'Running' stamp: 'jmv 12/20/2018 10:58:20'!
runCase: aTestCase

	| testCasePassed |

	testCasePassed _
		[
			[
				Transcript show: 'Will run: '; print: aTestCase; newLine.
				aTestCase runCase.
				Transcript show: 'finished.'; newLine.
				true]
					on: self class failure
					do: [ :signal |
						Transcript print: signal; newLine.
						(self failures isEmpty or: [ (failures last == aTestCase) not ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: self class error
							do: [ :signal |
								Transcript print: signal; newLine.
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]! !

