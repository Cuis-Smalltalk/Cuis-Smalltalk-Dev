'From Cuis 5.0 [latest update: #4106] on 6 April 2020 at 9:36:16 am'!
"Change Set:		4110-AddIdIvarToMorph-JuanVuletich-2020Apr06-09h34m
Date:			6 April 2020
Author:			Juan Vuletich

Adding an instance variable to Morph is tricky. Hence this preamble."
| ui b cs |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'In order to add a new instance variable to Morph,
we need to restart the User Interface process.
You''ll need to re-start the image update to install the next updates.') ifFalse: [ self halt ].
[
	ui _ UISupervisor ui.
	UISupervisor stopUIProcess.
	b _ ClassBuilder isSilent.
	ClassBuilder beSilent: true.
	ClassBuilder new
		superclass: Object
		subclass: #Morph
		instanceVariableNames: 'owner submorphs location layoutNeeded layoutSpec properties id'
		classVariableNames: 'LastMorphId'
		poolDictionaries: ''
		category: 'Morphic-Kernel'.
	ClassBuilder beSilent: b.
	UISupervisor spawnNewMorphicProcessFor: ui.
	ChangeSet installing: '4110-AddIdIvarToMorph-JuanVuletich-2020Apr06-09h34m-jmv.003.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4110') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done adding ivar ''id'' to Morph.' print.
	'Please restart [Install New Updates].' print.
] forkAt: 41!