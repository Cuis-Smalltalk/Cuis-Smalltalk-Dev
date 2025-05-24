'From Cuis7.3 [latest update: #7129] on 9 April 2025 at 3:16:15 pm'!

!VectorEngineWithPlugin class methodsFor: 'instance creation' stamp: 'jmv 4/8/2025 15:24:27'!
isPluginAvailable
	"
	VectorEngineWithPlugin isPluginAvailable
	Check if services provided by VM plugin match this class requirements.
	"
	| pluginApiVersion |
	pluginApiVersion := self pluginApiVersion.
	^ pluginApiVersion = 7! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| activeProc guiRoot |
(nil inform: 'After this update, #7221,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates].').
[
	ChangeSet installing: '7221-EnableVectorEnginePluginV7-JuanVuletich-2025Apr09-15h11m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '7221') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Installed ChangeSet: 7221-EnableVectorEnginePluginV7-JuanVuletich-2025Apr09-15h11m-jmv.001.cs.st' print.
	'Please do World / Changes... / [Install New Updates].' print.
	Utilities logsUserChanges: true.
] forkAt: 39.
UISupervisor ui setupNewCanvasAndEngine.
UISupervisor newUIProcess.
Processor activeProcess terminate.
!

