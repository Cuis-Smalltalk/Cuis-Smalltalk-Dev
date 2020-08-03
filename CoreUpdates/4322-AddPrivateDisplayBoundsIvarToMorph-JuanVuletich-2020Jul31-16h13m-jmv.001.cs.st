'From Cuis 5.0 [latest update: #4319] on 31 July 2020 at 4:16:04 pm'!
"Change Set:		4322-AddPrivateDisplayBoundsIvarToMorph-JuanVuletich-2020Jul31-16h13m
Date:			31 July 2020
Author:			Juan Vuletich

Adding an instance variable to Morph is tricky. Hence this preamble."
| ui b cs |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'In order to add a new instance variable to Morph,
we need to restart the User Interface process.
You''ll need to re-start the image update to install later updates.') ifFalse: [ self halt ].
[
	ui _ UISupervisor ui.
	UISupervisor stopUIProcess.
	b _ ClassBuilder isSilent.
	ClassBuilder beSilent: true.
	ClassBuilder new
		superclass: Object
		subclass: #Morph
		instanceVariableNames: 'owner submorphs location layoutSpec properties id privateDisplayBounds'
		classVariableNames: 'LastMorphId'
		poolDictionaries: ''
		category: 'Morphic-Kernel'.
	ClassBuilder beSilent: b.
	UISupervisor spawnNewMorphicProcessFor: ui.
	ChangeSet installing: '4322-AddPrivateDisplayBoundsIvarToMorph-JuanVuletich-2020Jul31-16h13m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4322') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done adding ivar ''privateDisplayBounds'' to Morph.' print.
	'Installed ChangeSet: 4322-AddPrivateDisplayBoundsIvarToMorph-JuanVuletich-2020Jul31-16h13m-jmv.001.cs.st' print.
	'Please restart [Install New Updates].' print.
] forkAt: 41!
