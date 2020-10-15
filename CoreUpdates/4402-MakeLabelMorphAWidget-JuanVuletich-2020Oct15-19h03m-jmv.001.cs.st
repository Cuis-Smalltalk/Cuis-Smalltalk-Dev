'From Cuis 5.0 [latest update: #4411] on 15 October 2020 at 9:20:32 am'!
"Change Set:		4402-MakeLabelMorphAWidget-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st
Date:			15 October 2020
Author:			Juan Vuletich

Modifying class definition of core to Morphs is tricky. Hence this preamble."
| ui b cs |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'In order to modify instance definition of LabelMorph,
we need to restart the User Interface process.
You''ll need to do [Install New Updates] again, to install later updates.') ifFalse: [ self halt ].
[
	ui _ UISupervisor ui.
	UISupervisor stopUIProcess.
	b _ ClassBuilder isSilent.
	ClassBuilder beSilent: true.
	ClassBuilder new
		superclass: WidgetMorph
		subclass: #LabelMorph
		instanceVariableNames: 'font emphasis contents'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Morphic-Widgets'.
	ClassBuilder new
		superclass: LabelMorph
		subclass: #UpdatingLabelMorph
		instanceVariableNames: 'target getSelector stepTime'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Morphic-Widgets'.
	ClassBuilder beSilent: b.
	UISupervisor spawnNewMorphicProcessFor: ui.
	(Delay forSeconds: 1) wait.
	ChangeSet installing: '4402-MakeLabelMorphAWidget-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4402') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done updating definition of LabelMorph.' print.
	'Installed ChangeSet: 4402-MakeLabelMorphAWidget-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st' print.
	'Please do [Install New Updates] again.' print.
] forkAt: 41!
