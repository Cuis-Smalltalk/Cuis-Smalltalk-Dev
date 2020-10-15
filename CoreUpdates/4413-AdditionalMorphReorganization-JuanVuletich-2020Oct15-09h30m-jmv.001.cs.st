'From Cuis 5.0 [latest update: #4412] on 15 October 2020 at 9:31:49 am'!
!classDefinition: #DropFilesAction category: #'Morphic-Kernel'!
Object subclass: #DropFilesAction
	instanceVariableNames: 'dropFilesEvent selectedFileEntry shouldAskToStop stopHereBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
!classDefinition: #WorldState category: #'Morphic-Kernel'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world'
	classVariableNames: 'DeferredUIMessages'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!classRemoval: #BorderedRectMorph stamp: 'jmv 10/15/2020 09:30:30'!
Smalltalk removeClassNamed: #BorderedRectMorph!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SystemOrganization removeSystemCategory: 'Morphic-Basic'.
SystemOrganization removeSystemCategory: 'Morphic-Worlds'.!


