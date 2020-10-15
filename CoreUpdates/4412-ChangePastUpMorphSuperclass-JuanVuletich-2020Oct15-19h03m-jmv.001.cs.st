'From Cuis 5.0 [latest update: #4411] on 15 October 2020 at 9:20:32 am'!
"Change Set:		4412-ChangePastUpMorphSuperclass-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st
Date:			15 October 2020
Author:			Juan Vuletich

Modifying class definition of core to Morphs is tricky. Hence this preamble."
| ui b cs |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'In order to modify instance definition of PasteUpMorph,
we need to restart the User Interface process.
You''ll need to do [Install New Updates] again, to install later updates.') ifFalse: [ self halt ].
[
	ui _ UISupervisor ui.
	UISupervisor stopUIProcess.
	b _ ClassBuilder isSilent.
	ClassBuilder beSilent: true.
	ClassBuilder new
		superclass: KernelMorph
		subclass: #PasteUpMorph
		instanceVariableNames: 'worldState backgroundImage backgroundImageData taskbar'
		classVariableNames: 'WindowEventHandler'
		poolDictionaries: ''
		category: 'Morphic-Kernel'.
	ClassBuilder beSilent: b.
	UISupervisor spawnNewMorphicProcessFor: ui.
	(Delay forSeconds: 1) wait.
	ChangeSet installing: '4412-ChangePastUpMorphSuperclass-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4412') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done updating superclass of PasteUpMorph.' print.
	'Installed ChangeSet: 4412-ChangePastUpMorphSuperclass-JuanVuletich-2020Oct15-19h03m-jmv.001.cs.st' print.
	'Please do [Install New Updates] again.' print.
] forkAt: 41!
