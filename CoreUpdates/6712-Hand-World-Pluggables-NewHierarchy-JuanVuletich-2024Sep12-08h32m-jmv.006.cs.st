'From Cuis7.1 [latest update: #6711] on 12 September 2024 at 8:37:55 am'!

!ProgressBarMorph methodsFor: 'as yet unclassified' stamp: 'jmv 9/12/2024 08:36:21'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `0@0`! !


!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 9/12/2024 08:35:36'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `0@0`! !


!WorldMorph methodsFor: 'caching' stamp: 'jmv 9/12/2024 08:37:19'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	canvas ifNotNil: [
		canvas unpinAndFreeMemory ].
	canvas := nil.
	damageRecorder := nil.
	activeHand releaseCachedState.! !

!methodRemoval: WorldMorph #handsReverseDo: stamp: 'jmv 9/12/2024 08:37:15'!
WorldMorph removeSelector: #handsReverseDo:!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| ui oldAuthor |
ui := UISupervisor ui.
oldAuthor := Utilities authorInitialsPerSe.
oldAuthor ifNil: [ Utilities setAuthorName: 'update 6712' initials: 'update 6712' ].
(nil confirm: 'Update 6712: After this update,
we need to restart the User Interface process.
Please do World / Changes... / [Install New Updates] again.

Select "yes" to continue.
')
	ifFalse: [ self halt ].
[
	Delay forSeconds: 0.1 :: wait.
	ChangeSet installing: '6712-Hand-World-Pluggables-NewHierarchy-JuanVuletich-2024Sep12-08h32m-jmv.006.cs.st' do: [
		ClassBuilder beSilentDuring: [
			ColoredBoxMorph subclass: #InnerPluggableMorph
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Morphic-Widgets'.
			BorderedBoxMorph subclass: #PluggableMorph
				instanceVariableNames: 'model menuNshortcutsKey'
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Morphic-Widgets'.
			PluggableMorph allSubInstancesDo: [ :m |
				m instVarNamed: 'padding' put: 0@0 ].
			BoxMorph subclass: #HandMorph
				instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData lastPosition'
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Morphic-Kernel'.
			ColoredBoxMorph subclass: #PasteUpMorph
				instanceVariableNames: 'backgroundImage backgroundImageData'
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Morphic-Kernel'.
			PasteUpMorph subclass: #WorldMorph
				instanceVariableNames: 'activeHand canvas damageRecorder stepList lastCycleTime alarms lastAlarmTime deferredUIMessages drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent taskbar'
				classVariableNames: 'ScaleFactor'
				poolDictionaries: ''
				category: 'Morphic-Kernel' ]].
	[ 'Installed ChangeSet: 6712. Reshaped main Morphic classes.' print.
	  'Please do World / Changes... / [Install New Updates] again.' print ] forkAt: 38.
	oldAuthor ifNil: [ Utilities clearAuthor ].
	UISupervisor spawnNewMorphicProcessFor: ui.
] forkAt: 39.
UISupervisor stopUIProcess.
!
