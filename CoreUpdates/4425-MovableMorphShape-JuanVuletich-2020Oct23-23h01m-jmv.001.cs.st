'From Cuis 5.0 [latest update: #4424] on 23 October 2020 at 11:02:34 pm'!
"Change Set:		4425-MovableMorphShape-JuanVuletich-2020Oct23-23h01m-jmv.001.cs.st
Date:			23 October 2020
Author:			Juan Vuletich

Modifying class definition of core to Morphs is tricky. Hence this preamble."
| ui b cs d1 d2 |
Utilities authorInitialsPerSe ifNil: [ Utilities setAuthor ].
(nil confirm: 'In order to modify instance definition of Morph and MovableMorph,
we need to restart the User Interface process.
You''ll need to do [Install New Updates] again, to install later updates.') ifFalse: [ self halt ].
[
	ui _ UISupervisor ui.
	UISupervisor stopUIProcess.
	d1 _ IdentityDictionary new.
	MovableMorph allSubInstancesDo: [ :m |
		d2 _ Dictionary new.
		d1 at: m put: d2.
		d2 at: #location put: (m instVarNamed: 'location').
		d2 at: #layoutSpec put: (m instVarNamed: 'layoutSpec').
		d2 at: #properties put: (m instVarNamed: 'properties').
		d2 at: #id put: (m instVarNamed: 'id').
		d2 at: #privateDisplayBounds put: (m instVarNamed: 'privateDisplayBounds'). ].
	b _ ClassBuilder isSilent.
	ClassBuilder beSilent: true.
	[
		ClassBuilder new
			superclass: Object
			subclass: #Morph
			instanceVariableNames: 'owner submorphs properties id privateDisplayBounds'
			classVariableNames: 'LastMorphId'
			poolDictionaries: ''
			category: 'Morphic-Kernel'.
	]
		on: InMidstOfFileinNotification, UndeclaredVariableWarning, PoolDefinitionNotification 
		do: [ :ex | ex resume: true ].
	ClassBuilder new
		superclass: Morph
		subclass: #MovableMorph
		instanceVariableNames: 'location layoutSpec'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Morphic-Kernel'.
	ClassBuilder beSilent: b.	
	MovableMorph allSubInstancesDo: [ :m |
		d2 _ d1 at: m.
		m instVarNamed: 'properties' put: (d2 at: #properties).
		m instVarNamed: 'id' put: (d2 at: #id).
		m instVarNamed: 'privateDisplayBounds' put: (d2 at: #privateDisplayBounds).
		m instVarNamed: 'location' put: (d2 at: #location).
		m instVarNamed: 'layoutSpec' put: (d2 at: #layoutSpec). ].
	d1 _ nil. d2 _ nil.
	UISupervisor spawnNewMorphicProcessFor: ui.
	(Delay forSeconds: 1) wait.
	ChangeSet installing: '4425-MovableMorphShape-JuanVuletich-2020Oct23-23h01m-jmv.001.cs.st' do: [].
	cs _ ChangeSet changeSetForBaseSystem.
	(cs name beginsWith: '4425') ifTrue: [
		ChangeSet removeChangeSet: cs ].
	'Done updating definition of LabelMorph.' print.
	'Installed ChangeSet: 4425-MovableMorphShape-JuanVuletich-2020Oct23-23h01m-jmv.001.cs.st' print.
	'Please do [Install New Updates] again.' print.
] forkAt: 41!
