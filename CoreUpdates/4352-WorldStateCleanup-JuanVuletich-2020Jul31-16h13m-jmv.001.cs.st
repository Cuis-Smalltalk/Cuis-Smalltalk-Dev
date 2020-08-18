'From Cuis 5.0 [latest update: #4351] on 18 August 2020 at 4:23:23 pm'!

!WorldState methodsFor: 'canvas' stamp: 'jmv 8/18/2020 16:13:52'!
setCanvas
	self setCanvas: Display getCanvas.
	damageRecorder _ DamageRecorder new.
	world redrawNeeded! !

!Preferences class methodsFor: 'start up' stamp: 'jmv 8/18/2020 16:22:58'!
checkLostChangesOnStartUp
	^ "self
		valueOfFlag: #checkLostChangesOnStartUp
		ifAbsent: [ true ]." false! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(nil confirm: 'After this update,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates] again.') ifFalse: [ self halt ].
[
	ChangeSet installing: '4352-WorldStateCleanup-JuanVuletich-2020Jul31-16h13m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4352') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 4352-WorldStateCleanup-JuanVuletich-2020Jul31-16h13m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates] again.' print.
] forkAt: 39.
Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true.!
