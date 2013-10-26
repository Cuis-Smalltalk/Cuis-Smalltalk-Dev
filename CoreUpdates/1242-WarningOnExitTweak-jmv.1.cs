'From Cuis 4.0 of 3 April 2012 [latest update: #1241] on 3 April 2012 at 3:24:59 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/3/2012 15:24'!
                          okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSorter allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any isEmpty not ]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	baseCSdirty & dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'      (would need to be saved on next run), ', String newLineString,
				'and unsaved Changes to Cuis core', String newLineString,
				'      (they would be lost as a separate ChangeSet).', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'and unsaved Changes to Cuis core.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core has unsaved changes.', String newLineString,
			'If you continue, they will be lost.', String newLineString,
			'Continue?' ].

	dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will need to be saved on next run.', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	^true! !

