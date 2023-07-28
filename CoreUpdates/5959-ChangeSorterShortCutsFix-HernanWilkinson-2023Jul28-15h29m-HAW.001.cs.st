'From Cuis 6.0 [latest update: #5958] on 28 July 2023 at 3:31:34 pm'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'HAW 7/28/2023 15:30:22'!
renameWith: newName

	"I do not use renameTo: because it is defined in Obejct with a different semantic - Hernan"
	
	(newName = myChangeSet name or: [newName isEmpty])
		ifTrue: [Smalltalk beep]
		ifFalse:
			[(ChangeSet changeSetNamed: newName) notNil
				ifTrue: [self inform: 'Sorry that name is already used']
				ifFalse:
					[myChangeSet name: newName.
					self update.
					self changed: #mainButtonName.
					self changed: #relabel]]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'HAW 7/28/2023 15:29:54'!
fileOutAndRemove
	"File out the current change set."

	myChangeSet isForBaseSystem ifTrue: [ 
		myChangeSet fileOut.
		self removePrompting: false.

		self showChangeSet: ChangeSet changeSetForBaseSystem.
		self update ]! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'HAW 7/28/2023 15:30:05'!
remove
	"Completely destroy my change set.  Check if it's OK first"

	myChangeSet isForBaseSystem ifTrue: [ 
		self removePrompting: true.
		self showChangeSet: ChangeSet changeSetForBaseSystem.
		self update ]! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'HAW 7/28/2023 15:30:14'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	myChangeSet isForBaseSystem ifTrue: [ 
		self 
			request: 'New name for this change set' 
			initialAnswer: myChangeSet name 
			do: [:newName| self renameWith: newName]]! !


!ChangeSorter reorganize!
('access' changeSet currentCanHavePreambleAndPostscript currentHasPostscript currentHasPreamble label labelString methodInfoFromRemoval: myChangeSet myChangeSet: showChangeSet: showChangeSetNamed:)
('changeSet menu' addPreamble changeSetList currentChangeSetName editPostscript fileOutAndKeep fileOutAndRemove mainButtonName methodConflicts remove removePostscript removePreamble removePrompting: rename renameWith: trimHistory update updateIfNeeded viewAffectedClassCategories)
('class list' classList currentClassName currentClassName: forgetClass selectedClass selectedClassOrMetaClass)
('message list' currentSelector currentSelector: forget messageList removeMessage selectedMessageName)
('code pane' acceptedStringOrText contents:notifying: toggleDiffing)
('annotation' addPriorVersionsCountForSelector:ofClass:to:)
('shout styling' shouldStyle:with:)
!

