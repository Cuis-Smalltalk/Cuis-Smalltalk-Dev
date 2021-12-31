'From Cuis 5.0 [latest update: #4351] on 18 August 2020 at 4:23:23 pm'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| activeProc guiRoot |
(nil confirm: 'After this update,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates] again.') ifFalse: [ self halt ].
[
	ChangeSet installing: '5027-RecreateWorld-JuanVuletich-2021Dec30-19h03m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '5027') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 5027-RecreateWorld-JuanVuletich-2021Dec30-19h03m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates] again.' print.
] forkAt: 39.
	activeProc _ Processor activeProcess.
[
	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	guiRoot _ WorldMorph newWorld.
	UISupervisor spawnNewMorphicProcessFor: guiRoot.
	UISupervisor whenUIinSafeState: [
		guiRoot recreateDefaultDesktop; 
			color: Theme current background;
			restoreDisplay.
		Smalltalk garbageCollect ].
] forkAt: 41.!
