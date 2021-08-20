'From Cuis 5.0 [latest update: #4775] on 20 August 2021 at 11:58:40 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/20/2021 11:56:37'!
askConfirmationOnQuit
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk askConfirmationOnQuit
	"
	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	"dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ]."
	dirtyPackages _ ChangeSet allChangeSets anySatisfy: [ :any | any codePackage notNil and: [ any hasUnsavedChanges ]].
	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, all unsaved changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, their changes will be lost.', String newLineString,
			'Do you really want to exit Cuis without saving the image?' ].
	Preferences askConfirmationOnQuit ifTrue: [
		^self confirm: 'Do you really want to exit Cuis without saving the image?' ].
	^true! !


!Preferences class methodsFor: 'personalization' stamp: 'jmv 8/20/2021 11:57:15'!
askConfirmationOnQuit
	^ self
		valueOfFlag: #askConfirmationOnQuit
		ifAbsent: [ true ].! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 8/20/2021 11:57:53'!
quitSession
	Smalltalk askConfirmationOnQuit ifFalse: [ ^ self ].
	Smalltalk quit! !

!methodRemoval: SystemDictionary #okayToDiscardUnsavedCode stamp: 'jmv 8/20/2021 11:58:03'!
SystemDictionary removeSelector: #okayToDiscardUnsavedCode!
!methodRemoval: SystemDictionary #askConfirmationOnQuitDiscardingUnsavedCode stamp: 'jmv 8/20/2021 11:57:58'!
SystemDictionary removeSelector: #askConfirmationOnQuitDiscardingUnsavedCode!
