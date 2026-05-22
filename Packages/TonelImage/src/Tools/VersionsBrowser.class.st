"
VersionsBrowser shows all the versions of a particular method, and lets you compare them, revert to selected versions, and so on.
"
Class {
	#name : 'VersionsBrowser',
	#superclass : 'ChangeList',
	#instVars : [
		'classOfMethod',
		'selectorOfMethod',
		'addedChangeRecord'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'as yet unclassified' }
VersionsBrowser class >> timeStampFor: aSelector class: aClass reverseOrdinal: anInteger [
	"Answer the time stamp corresponding to some version of the given method, nil if none.  The reverseOrdinal parameter is interpreted as:  1 = current version; 2 = last-but-one version, etc."
	
	| method aChangeList |
	method := aClass compiledMethodAt: aSelector ifAbsent: [^ nil].
	aChangeList := self new
			scanVersionsOf: method class: aClass meta: aClass isMeta
			category: nil selector: aSelector.
	^ aChangeList ifNil: [nil] ifNotNil:
		[aChangeList list size >= anInteger
			ifTrue:
				[(aChangeList changeList at: anInteger) stamp]
			ifFalse:
				[nil]]
]

{ #category : 'as yet unclassified' }
VersionsBrowser class >> versionCountForSelector: aSelector class: aClass [
	"Answer the number of versions known to the system for the given class and method, including the current version.  A result of greater than one means that there is at least one superseded version.  Answer zero if no logged version can be obtained."
	
	| method aChangeList |
	method := aClass compiledMethodAt: aSelector ifAbsent: [^ 0].
	aChangeList := self new
			scanVersionsOf: method class: aClass meta: aClass isMeta
			category: nil selector: aSelector.
	^ aChangeList ifNil: [0] ifNotNil: [aChangeList list size]
]

{ #category : 'misc' }
VersionsBrowser >> addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream [
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior versions are seen in a versions browser -- in this case, the inherited version of this method will not work."

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].

	aStream nextPutAll: 
		((changeList notEmpty
			ifTrue:
				[changeList size = 1
					ifTrue:
						['deleted - one prior version']
					ifFalse:
						['deleted - ', changeList size printString, ' prior versions']]
			ifFalse:
				['surprisingly, no prior versions']))
]

{ #category : 'init & update' }
VersionsBrowser >> addedChangeRecord: aChangeRecord [
	addedChangeRecord := aChangeRecord.
	self reformulateList.
]

{ #category : 'misc' }
VersionsBrowser >> classCommentIndicated [
	"Answer whether the receiver is pointed at a class comment"

	^ false
]

{ #category : 'menu' }
VersionsBrowser >> compareToOtherVersion [
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| base case baseString caseString differDesc diffWords targetChanges labels label1 |
	base := changeList at: listIndex ifAbsent: [ ^self ].
	label1 := list at: listIndex.

	labels := list reject: [ :any | any = label1 ].
	targetChanges := changeList reject: [ :any | any = base ].
	case := ((SelectionMenu
				labels: labels
				selections: targetChanges) startUpMenu) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp := change1. change1 := change2. change2 := temp ]."

	baseString := base string.
	caseString := case string.
	baseString = caseString
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords := self shouldDiffWords.
	differDesc := diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: baseString to: caseString
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { base stamp. case stamp })
]

{ #category : 'menu' }
VersionsBrowser >> fileInSelections  [
	super fileInSelections.
	self reformulateList
]

{ #category : 'menu' }
VersionsBrowser >> offerVersionsHelp [
	(TextModel new contents: self versionsHelpString)
		openLabel: 'Versions Browsers'
]

{ #category : 'init & update' }
VersionsBrowser >> reformulateList [
	| aMethod |
	"Some uncertainty about how to deal with lost methods here"
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	
	self scanVersionsOf: aMethod class: classOfMethod theNonMetaClass meta: classOfMethod isMeta category: (classOfMethod whichCategoryIncludesSelector: selectorOfMethod) selector: selectorOfMethod.
	self changed: #list. "for benefit of mvc"
	listIndex := 1.
	self changed: #listIndex.
	self acceptedContentsChanged

]

{ #category : 'init & update' }
VersionsBrowser >> scanVersionsOf: method class: class meta: meta category: category selector: selector [
	| position stamp priorMethodRef prevPos prevFileIndex libraryName preamble tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod stampFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ].
				libraryName := CompiledMethod libraryNameFrom: tokens ].
 		self addItem:
				(ChangeRecord new
					file: file position: position type: #method
					class: class name category: category meta: meta stamp: stamp 
					prior: priorMethodRef libraryName: libraryName)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections
]

{ #category : 'misc' }
VersionsBrowser >> selectedClass [
	"Answer the class currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod theNonMetaClass
]

{ #category : 'misc' }
VersionsBrowser >> selectedClassOrMetaClass [
	"Answer the class or metaclass currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod
]

{ #category : 'misc' }
VersionsBrowser >> selectedMessageName [
	"Answer the message name currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ selectorOfMethod
]

{ #category : 'misc' }
VersionsBrowser >> showsVersions [
	^ true
]

{ #category : 'init & update' }
VersionsBrowser >> updateIfNeeded [
	| aMethod |
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true

]

{ #category : 'menu' }
VersionsBrowser >> versionsHelpString [
	^ 'Each entry in the list pane represents a version of the source code for the same method; the topmost entry is the current version, the next entry is the next most recent, etc.

To revert to an earlier version, select it (in the list pane) and then do any of the following:
  *  Choose "revert to this version" from the list pane menu.
  *  Hit the "revert" button,
  *  Type Return in the code pane
  *  Type cmd-s (alt-s) in the code pane.

The code pane shows the source for the selected version.  If "diffing" is in effect, then differences betwen the selected version and the version before it are pointed out in the pane.  Turn diffing on and off by choosing "toggle diffing" from the list pane menu, or hitting the "diffs" button, or hitting cmd-D when the cursor is over the list pane.

To get a comparison between the selected version and the current version, choose "compare to current" from the list pane menu or hit the "compare to current" button.  (This is meaningless if the current version is selected, and is unnecessary if you''re interested in diffs from between the current version and the next-most-recent version, since the standard in-pane "diff" feature will give you that.)

You can also compare the selected version with any other version using the "compare to version..." menu choice.

If further versions of the method in question have been saved elsewhere since you launched a particular Versions Browser, it will still stay nicely up-to-date if you''re in Morphic and have asked that smart updating be maintained; if you''re in mvc or in morphic but with smart-updating turned off, a versions browser is only brought up to date when you activate its window (and when you issue "revert" from within it, of course,) and you can also use the "update list" command to make certain the versions list is up to date.

Hit the "remove from changes" button, or choose the corresponding command in the list pane menu, to have the method in question deleted from the current change set.  This is useful if you''ve put debugging code into a method, and now want to strip it out and cleanse your current change set of all memory of the excursion.

Note:  the annotation pane in versions browsers shows information about the *current* version of the method in the image, not about the selected version.'
]
