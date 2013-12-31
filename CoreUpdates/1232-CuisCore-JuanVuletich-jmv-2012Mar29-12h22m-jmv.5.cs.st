'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 3:27:29 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/29/2012 13:47'!
                  okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCs baseCSdirty dirtyPackages |
	baseCs _ ChangeSet changeSetForBaseSystem.
	baseCSdirty _ baseCs isEmpty not.
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
		^self confirm: 'The current ChangeSet for Cuis core:', String newLineString,
			baseCs name, String newLineString, 
			'has unsaved stuff. If you continue, it will be lost.', String newLineString,
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


!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/29/2012 13:01'!
    isForBaseSystem
	^ ((name beginsWith: 'UnsavedChangesTo-')
		or: [ (name beginsWith: 'Install-')
			or: [ name beginsWith: 'Affects-' ]]) not! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/29/2012 13:50'!
                             okayToSave
	"Answer true unless the user cancels saving because of some warning given."

	| wasCog isCog |
	isCog _ Smalltalk isRunningCog.
	[ wasCog _ self imageFormatVersionFromFile allMask: 1 ]
		on: Error
		do: [ :ignore |
			"probably save-as to non-existing file"
			^ self okayToDiscardUnsavedCodeSaving: true ].

	(isCog and: [wasCog not]) ifTrue: [
		(self confirm: 'You''re running with a Cog VM.', String newLineString,
			'Non-Cog VMs might not be able to open images saved under Cog!!', String newLineString,
			'(If you choose "YES", you might only use this image under Cog VMs.)', String newLineString,
			'(If you choose "NO", you might save your work in some other way, and later exit Cuis without saving).', String newLineString,
			'Really save?')
				ifFalse: [ ^false ]].
		
	^ self okayToDiscardUnsavedCodeSaving: true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 3/29/2012 13:54'!
   snapshot: save andQuit: quit
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSorter zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				(self okayToDiscardUnsavedCodeSaving: false) ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 3/29/2012 12:27'!
                      snapshot: save andQuit: quit embedded: embeddedFlag
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	| resuming msg |
	ActiveModel flushEventSystem.
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: (FileDirectory default localNameFor: self imageName);
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ].
	self processShutDownList: quit.
	Cursor write show.
	save
		ifTrue: [
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].
	Cursor normal show.
	self setGCParameters.
	resuming == true ifTrue: [ self clearExternalObjects ].
	self processStartUpList: resuming == true.
	resuming == true ifTrue: [
		self setPlatformPreferences.
		self readDocumentFile ].
	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].
	^ resuming! !

