'From Cuis 4.1 of 12 December 2012 [latest update: #1532] on 3 January 2013 at 11:10:59 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/3/2013 11:09'!
okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSorter allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges and: [ any isEmpty not ]]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	wouldSave ifTrue: [
"		baseCSdirty & dirtyPackages ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'      (would need to be saved on next run), ', String newLineString,
				'and unsaved Changes to Cuis core', String newLineString,
				'      (they would be lost as a separate ChangeSet).', String newLineString,
				'Continue?'  ]."
		baseCSdirty ifTrue: [
			^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
				'If you continue, they would be lost as a separate ChangeSet.', String newLineString,
				'Continue?' ].
"		dirtyPackages ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will need to be saved on next run.', String newLineString,
				'Continue?' ]"
	] ifFalse: [
		baseCSdirty & dirtyPackages ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'and unsaved Changes to Cuis core.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ].
		baseCSdirty ifTrue: [
			^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
				'If you continue, they would be lost.', String newLineString,
				'Continue?' ].
		dirtyPackages ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]
	].

	^true! !

