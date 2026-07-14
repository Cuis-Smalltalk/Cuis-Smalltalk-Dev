'From Cuis7.9 [latest update: #8056] on 13 July 2026 at 3:29:24 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 13/Jul/2026, 4:43:35 pm (UTC)'!
installOrTestRun

	Installing isNil ifTrue: [ ^false ].
	(Installing beginsWith: 'RunningTest-') ifTrue: [
		^Processor activeProcess name = 'TestRunner' ].
	^true
	
	"
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name = 'TestRunner' ]	-> Test
	(Installing beginsWith: Install-')															-> Install
	Installing isNil 																			-> Normal
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]	-> Normal
	"! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 13/Jul/2026, 4:43:50 pm (UTC)'!
notInstallOrTestRun

	^self installOrTestRun not! !

