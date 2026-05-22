"
I display a ChangeSet.
"
Class {
	#name : 'ChangeSorter',
	#superclass : 'CodeProvider',
	#instVars : [
		'myChangeSet',
		'currentClassName',
		'currentSelector',
		'priorChangeSetList',
		'priorDirtyFlags'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'ui' }
ChangeSorter class >> open [
	^ ChangeSorterWindow 
		open: self new
		label: nil
]

{ #category : 'code pane' }
ChangeSorter >> acceptedStringOrText [
	"return the source code that shows in the bottom pane"

	| sel class changeType answer |
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class := self selectedClassOrMetaClass.
	(sel := currentSelector)
		ifNotNil: [
			changeType := (myChangeSet atSelector: (sel := sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!'].
			answer :=  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer := (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer := (self diffFromPriorSourceFor: answer) ].
			^ answer ]
		ifNil: [
			^ String streamContents: [ :strm |
				(myChangeSet classChangeAt: currentClassName) do: [ :each |
					each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
					each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
					each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
					each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
					each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
					each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
					each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
					each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
				]]].
]

{ #category : 'changeSet menu' }
ChangeSorter >> addPreamble [
	myChangeSet assurePreambleExists.
	currentClassName := nil.
	currentSelector := nil.
	self showChangeSet: myChangeSet
]

{ #category : 'annotation' }
ChangeSorter >> addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream [
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior version is pointed to by the lost-method pointer in the change held on to by the changeset"

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].
	aStream nextPutAll:
		((myChangeSet methodInfoFromRemoval: {aClass name. aSelector})
			ifNil:
				['no prior versions']
			ifNotNil:
				['version(s) retrievable here'])
]

{ #category : 'shout styling' }
ChangeSorter >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	^self currentSelector notNil
]

{ #category : 'access' }
ChangeSorter >> changeSet [
	^ myChangeSet
]

{ #category : 'changeSet menu' }
ChangeSorter >> changeSetList [
	"Answer a list of ChangeSet names to be shown in the change sorter."

	^ (ChangeSet allChangeSets collect: [ :each |
		(((each isForBaseSystem or: [ each codePackage notNil ]) and: [ each hasUnsavedChanges ]) ifTrue: ['*'] ifFalse: ['']), each name ]) reversed
]

{ #category : 'class list' }
ChangeSorter >> classList [
	"Computed.  View should try to preserve selections, even though index changes"

	^ myChangeSet ifNotNil: [myChangeSet changedClassNames] ifNil: [OrderedCollection new]

]

{ #category : 'code pane' }
ChangeSorter >> contents: input notifying: aRequestor  [
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	(class := self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: codeString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: codeString.
		self currentSelector: nil.  "forces update with no 'unsaved chages' feedback"
		^ true].
	oldSelector := self selectedMessageName.
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true
]

{ #category : 'access' }
ChangeSorter >> csIsForBase [
	^ myChangeSet isForBaseSystem
]

{ #category : 'access' }
ChangeSorter >> currentCanHavePreambleAndPostscript [
	^ myChangeSet canHavePreambleAndPostscript
]

{ #category : 'changeSet menu' }
ChangeSorter >> currentChangeSetName [
	^ (((myChangeSet isForBaseSystem or: [ myChangeSet codePackage notNil ]) and: [ myChangeSet hasUnsavedChanges ]) ifTrue: ['*'] ifFalse: ['']), myChangeSet name
]

{ #category : 'class list' }
ChangeSorter >> currentClassName [

	^ currentClassName
]

{ #category : 'class list' }
ChangeSorter >> currentClassName: aString [

	currentClassName := aString.
	currentSelector := nil.	"fix by wod"
	self changed: #currentClassName.
	self changed: #messageList.
	self acceptedContentsChanged.
]

{ #category : 'access' }
ChangeSorter >> currentHasPostscript [
	^ myChangeSet hasPostscript
]

{ #category : 'access' }
ChangeSorter >> currentHasPreamble [
	^ myChangeSet hasPreamble
]

{ #category : 'message list' }
ChangeSorter >> currentSelector [

	^ currentSelector
]

{ #category : 'message list' }
ChangeSorter >> currentSelector: messageName [

	currentSelector := messageName.
	self changed: #currentSelector.
	self acceptedContentsChanged.
]

{ #category : 'changeSet menu' }
ChangeSorter >> editPostscript [
	"Allow the user to edit the receiver's change-set's postscript -- in a separate window"

	myChangeSet editPostscript
]

{ #category : 'changeSet menu' }
ChangeSorter >> fileOutAndKeep [
	"File out the current change set."

	myChangeSet fileOut.
	self update
]

{ #category : 'changeSet menu' }
ChangeSorter >> fileOutAndRemove [
	"File out the current change set."

	myChangeSet fileOut.
	self removePrompting: false.

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update
]

{ #category : 'message list' }
ChangeSorter >> forget [
	"Drop this method from the changeSet"

	currentSelector ifNotNil: [
		myChangeSet removeSelectorChanges: self selectedMessageName 
			class: self selectedClassOrMetaClass.
		currentSelector := nil.
		self showChangeSet: myChangeSet]
]

{ #category : 'class list' }
ChangeSorter >> forgetClass [
	"Remove all mention of this class from the changeSet"

	currentClassName ifNotNil: [
		myChangeSet removeClassChanges: currentClassName.
		currentClassName := nil.
		currentSelector := nil.
		self showChangeSet: myChangeSet].

]

{ #category : 'access' }
ChangeSorter >> label [
	^ self labelString
]

{ #category : 'access' }
ChangeSorter >> labelString [
	"The label for my entire window.  The large button that displays my name is gotten via mainButtonName"

	^ 'ChangeSet: ', myChangeSet name
]

{ #category : 'changeSet menu' }
ChangeSorter >> mainButtonName [

	^ myChangeSet name
]

{ #category : 'message list' }
ChangeSorter >> messageList  [

	| probe newSelectors |
	currentClassName ifNil: [^ #()].
	probe := (currentClassName endsWith: ' class')
		ifTrue: [currentClassName]
		ifFalse: [currentClassName asSymbol].
	newSelectors := myChangeSet selectorsInClass: probe.
	(newSelectors includes: currentSelector) ifFalse: [currentSelector := nil].
	^ newSelectors asArray sort

]

{ #category : 'changeSet menu' }
ChangeSorter >> methodConflicts [
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |
	aList := myChangeSet 
		messageListForChangesWhich: [ :aClass :aSelector |
			(ChangeSet allChangeSetsWithClass: aClass selector: aSelector) size > 1 ]
		ifNone: [ #() ].
	^aList
]

{ #category : 'access' }
ChangeSorter >> methodInfoFromRemoval: classAndSelector [
	^myChangeSet methodInfoFromRemoval: classAndSelector
]

{ #category : 'access' }
ChangeSorter >> myChangeSet [
	^myChangeSet
]

{ #category : 'access' }
ChangeSorter >> myChangeSet: anObject [
	myChangeSet := anObject
]

{ #category : 'changeSet menu' }
ChangeSorter >> remove [
	"Completely destroy my change set.  Check if it's OK first"

	myChangeSet isForBaseSystem ifTrue: [ 
		self removePrompting: true.
		self showChangeSet: ChangeSet changeSetForBaseSystem.
		self update ]
]

{ #category : 'message list' }
ChangeSorter >> removeMessage [
	"Remove the selected msg from the system. Real work done by the 
	parent, a ChangeSorter"
	| confirmation sel |
	currentSelector
		ifNotNil: [
			confirmation := Smalltalk
				confirmRemovalOf: (sel := self selectedMessageName)
				on: self selectedClassOrMetaClass.
			confirmation = 3
				ifTrue: [^ self].
			self selectedClassOrMetaClass removeSelector: sel.
			self update.
			confirmation = 2
				ifTrue: [Smalltalk browseAllCallsOn: sel]]
]

{ #category : 'changeSet menu' }
ChangeSorter >> removePostscript [

	myChangeSet removePostscript.
	self showChangeSet: myChangeSet
]

{ #category : 'changeSet menu' }
ChangeSorter >> removePreamble [
	myChangeSet removePreamble.
	self showChangeSet: myChangeSet
]

{ #category : 'changeSet menu' }
ChangeSorter >> removePrompting: doPrompt [
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName msg |
	aName := myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message := 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg := myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	ChangeSet removeChangeSet: myChangeSet.
]

{ #category : 'changeSet menu' }
ChangeSorter >> rename [
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	myChangeSet isForBaseSystem ifTrue: [ 
		self 
			request: 'New name for this change set' 
			initialAnswer: myChangeSet name 
			do: [:newName| self renameWith: newName]]
]

{ #category : 'changeSet menu' }
ChangeSorter >> renameWith: newName [

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
					self changed: #relabel]]
]

{ #category : 'class list' }
ChangeSorter >> selectedClass [
	"Answer the currently-selected class.  If there is no selection, or if the selection refers to a class no longer extant, return nil"
	| c |
	^ currentClassName ifNotNil: [(c := self selectedClassOrMetaClass)
		ifNotNil: [c theNonMetaClass]]
]

{ #category : 'class list' }
ChangeSorter >> selectedClassOrMetaClass [
	"Careful, the class may have been removed!"

	| cName |
	currentClassName ifNil: [^ nil].
	(currentClassName endsWith: ' class')
		ifTrue: [
			cName := (currentClassName copyFrom: 1 to: currentClassName size-6) asSymbol.
			^ (Smalltalk at: cName ifAbsent: [^nil]) class]
		ifFalse: [
			cName := currentClassName asSymbol.
			^ Smalltalk at: cName ifAbsent: nil]
]

{ #category : 'message list' }
ChangeSorter >> selectedMessageName [

	currentSelector ifNil: [^ nil].
	^ currentSelector asSymbol
]

{ #category : 'message list' }
ChangeSorter >> selector: oldSelector refactoredTo: newSelector [

	
]

{ #category : 'access' }
ChangeSorter >> showChangeSet: chgSet [

	myChangeSet == chgSet ifFalse: [
		myChangeSet := chgSet.
		currentClassName := nil.
		currentSelector := nil].
	self changed: #relabel.
	self changed: #currentChangeSetName.	"new -- list of sets"
	self changed: #mainButtonName.	"old, button"
	self changed: #classList.
	self changed: #messageList.
	self acceptedContentsChanged.
]

{ #category : 'access' }
ChangeSorter >> showChangeSetNamed: aString [
	| name |
	name := (aString beginsWith: '*') ifTrue: [aString allButFirst] ifFalse: [aString].
	self showChangeSet: (ChangeSet changeSetNamed: name) 
]

{ #category : 'changeSet menu' }
ChangeSorter >> trimHistory [
	"Drop non-essential history (rename, reorg, method removals) from newly-added classes."

	myChangeSet trimHistory.
	
	self showChangeSet: myChangeSet


]

{ #category : 'changeSet menu' }
ChangeSorter >> update [
	"Recompute all of my lists."

	self updateIfNeeded
]

{ #category : 'changeSet menu' }
ChangeSorter >> updateIfNeeded [
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList := self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList := newList.
			self changed: #changeSetList ].

	self acceptedContentsChanged
]

{ #category : 'changeSet menu' }
ChangeSorter >> viewAffectedClassCategories [

	myChangeSet changedClassCategories explore
]
