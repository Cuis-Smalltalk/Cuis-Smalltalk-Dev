'From Cuis 4.1 of 12 December 2012 [latest update: #1736] on 22 June 2013 at 9:20:14.404988 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:13'!
okayToDiscardUnsavedCode
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCode
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges and: [ any isEmpty not ]]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

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
			'Continue?' ].

	^true! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 6/22/2013 21:07'!
changeSetForBaseSystem

	| csName numberToUse |
	self notInstallOrTestRun
		ifTrue: [
			numberToUse _ self currentBaseCSNumber.
			AllChangeSets
				detect: [ :any | any name initialIntegerOrNil = numberToUse ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName _ (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifFalse: [
			csName _ 'Affects-BaseSystem--', Installing.
			^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ]! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 6/22/2013 21:07'!
allChangeSetsWithClass: class selector: selector

	class ifNil: [^ #()].
	^ AllChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none]! !

!ChangeSet class methodsFor: 'removing' stamp: 'jmv 6/22/2013 21:19'!
zapAllChangeSets
	"Keep those change sets that are recording the unsaved changes of some package. Also kee the change set currently capturing changes to base system: it should be filed out and destroyed by the user."
	AllChangeSets copy do: [ :changeSet |
		(changeSet isForBaseSystem or: [
			changeSet codePackage notNil and: [changeSet isEmpty not]])
		ifFalse: [
			self removeChangeSet: changeSet ]]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:12'!
okayToSave
	"Answer true unless the user cancels saving because of some warning given."

	| wasCog isCog |
	isCog _ Smalltalk isRunningCog.
	[ wasCog _ self imageFormatVersionFromFile allMask: 1 ]
		on: Error
		do: [ :ignore |
			"probably save-as to non-existing file"
			^ true ].

	(isCog and: [wasCog not]) ifTrue: [
		(self confirm: 'You''re running with a Cog VM.', String newLineString,
			'Non-Cog VMs might not be able to open images saved under Cog!!', String newLineString,
			'(If you choose "YES", you might only use this image under Cog VMs.)', String newLineString,
			'(If you choose "NO", you might save your work in some other way, and later exit Cuis without saving).', String newLineString,
			'Really save?')
				ifFalse: [ ^false ]].
		
	^ true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 21:14'!
snapshot: save andQuit: quit
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSet zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				self okayToDiscardUnsavedCode ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false! !

!methodRemoval: SystemDictionary #okayToDiscardUnsavedCodeSaving:!
SystemDictionary removeSelector: #okayToDiscardUnsavedCodeSaving:!
