'From Cuis 5.0 [latest update: #3994] on 26 December 2019 at 10:03:58 am'!

!UISupervisor class methodsFor: 'gui process' stamp: 'HAW 12/26/2019 10:01:29'!
spawnNewMorphicProcessFor: guiRootObject

	| previousUIProcess |
	
	previousUIProcess := UIProcess.
	UIProcess _ guiRootObject runProcess.
	previousUIProcess ifNotNil: [ previousUIProcess animatedUI: nil ].
	UIProcess resume! !

