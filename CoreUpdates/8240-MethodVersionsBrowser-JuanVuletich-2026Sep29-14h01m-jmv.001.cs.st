'From Cuis7.9 [latest update: #8239] on 29 September 2026 at 2:04:26 pm'!
!classDefinition: #MethodVersionsBrowser category: #'Tools-Changes'!
CodeProvider subclass: #MethodVersionsBrowser
	instanceVariableNames: 'changeList list listIndex classOfMethod selectorOfMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodVersionsBrowser commentStamp: 'jmv 28/Sep/2026, 6:33:03 pm (UTC)' prior: 0!
A ChangeList represents a list of changed methods that reside on a file in fileOut format.  The classes and methods in my list are not necessarily in this image!!  Used as the model for both Version Lists and Changed Methods.

It holds three lists:
	changeList - a list of ChangeListElements
	list - a list of one-line printable headers
	listSelections - a list of Booleans (true = selected, false = not selected) multiple OK.
	listIndex 
Items that are removed (removeDoits, remove an item) are removed from all three lists.
Most recently clicked item is the one showing in the bottom pane.
----
MethodVersionsBrowser shows all the versions of a particular method, and lets you compare them, revert to selected versions, and so on.!

!classDefinition: #MethodVersionsBrowserWindow category: #'Morphic-Tool Windows'!
CodeWindow subclass: #MethodVersionsBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!MethodVersionsBrowserWindow commentStamp: '<historical>' prior: 0!
Morphic view for VersionsBrowser models. See category 'GUI building'.!


!MethodVersionsBrowser methodsFor: 'initialization-release' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
addItem: item text: text

	changeList addLast: item.
	list addLast: (text collect: [ :x | x isLineSeparator ifTrue: [$/] ifFalse: [x]])! !

!MethodVersionsBrowser methodsFor: 'initialization-release' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
initialize
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol := (Preferences at: #diffsInChangeList)
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.
	super initialize! !

!MethodVersionsBrowser methodsFor: 'menu actions' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| base class baseString caseString differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	base := changeList at: listIndex.
	((class := base changeClass) notNil
			and: [class includesSelector: base methodSelector])
		ifTrue: [
			baseString := base string.
			caseString := (class sourceCodeAt: base methodSelector) asPlainString.
			baseString = caseString
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords := self shouldDiffWords.
			differDesc := diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: baseString to: caseString
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: (Preferences at: #standardCodeFont)))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !

!MethodVersionsBrowser methodsFor: 'menu actions' stamp: 'jmv 28/Sep/2026, 5:53:33 pm (UTC)'!
fileInSelections 
		
	listIndex = 0 ifTrue: [
		self inform: 'nothing selected, so nothing done'
		^self ].

	(changeList at: listIndex) fileIn.
	self reformulateList.! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:55:53 pm (UTC)'!
acceptedStringOrText
	"Answer the contents string, obeying diffing directives if needed"

	^ self showingAnyKindOfDiffs
		ifFalse: [
			self undiffedContents]
		ifTrue: [
			self diffedVersionContents]! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 5:51:11 pm (UTC)'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change ann1 ann2 |
	change := self currentChange.
	
	change isNil ifTrue: [
		^ 'Total items: ', list size printString ].

	change isClassDefinition ifTrue: [
		ann1 := change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 := (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ ''].

	^ change methodSelector notNil
		ifFalse: [
			super annotation]
		ifTrue: [
			'current version: ', super annotation]! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change := changeList at: listIndex.
	later := change text.
	class := change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier := (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
list
	^ list! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
listIndex
	^ listIndex! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:52:40 pm (UTC)'!
selectedClass
	"Answer the class currently selected in the browser.  In the case of a MethodVersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod theNonMetaClass! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:52:45 pm (UTC)'!
selectedClassOrMetaClass
	"Answer the class or metaclass currently selected in the browser.  In the case of a MethodVersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:52:51 pm (UTC)'!
selectedMessageName
	"Answer the message name currently selected in the browser.  In the case of a MethodVersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ selectorOfMethod! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 5:53:51 pm (UTC)'!
toggleListIndex: newListIndex

	listIndex := newListIndex.
	self changed: #listIndex.
	self acceptedContentsChanged.! !

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
undiffedContents
	^ listIndex = 0
		ifTrue: ['']
		ifFalse: [(changeList at: listIndex) text]! !

!MethodVersionsBrowser methodsFor: 'accessing' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
currentChange
	"return the current change being viewed, or nil if none"

	^ listIndex = 0
		ifFalse: [ changeList at: listIndex ]! !

!MethodVersionsBrowser methodsFor: 'accessing' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
currentCompiledMethod
	"Actually, the CompiledMethod installed in the system that correspond to current selection"
	| selector |
	selector := self selectedMessageName.
	(selector isNil or: [ MethodSet isPseudoSelector: selector ]) ifTrue: [
		^nil ].
	^self selectedClassOrMetaClass compiledMethodAt: selector! !

!MethodVersionsBrowser methodsFor: 'shout styling' stamp: 'jmv 28/Sep/2026, 4:49:02 pm (UTC)'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) isMethodDefinition ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				^true ]]].
	^false! !

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 24/Nov/2010, 5:21:00 pm (UTC)'!
reformulateList
	| aMethod |
	"Some uncertainty about how to deal with lost methods here"
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	
	self scanVersionsOf: aMethod class: classOfMethod theNonMetaClass meta: classOfMethod isMeta category: (classOfMethod whichCategoryIncludesSelector: selectorOfMethod) selector: selectorOfMethod.
	self changed: #list. "for benefit of mvc"
	listIndex := 1.
	self changed: #listIndex.
	self acceptedContentsChanged
! !

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 28/Sep/2026, 6:34:51 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex preamble methodProperties libNameOrNil tokens sourceFilesCopy sourceFile |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.



	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	sourceFile := sourceFilesCopy at: method fileIndex.
	[position notNil & sourceFile notNil] whileTrue: [
		"Skip back filler"
		sourceFile position: position.
		sourceFile backChunk. "Skip back blank space."
		"Get preamble"
		preamble := sourceFile backChunk.
		methodProperties := nil.
		libNameOrNil := nil.
		(preamble beginsWith: 'methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: (preamble copyFrom: 'methodProperties:' size+1 to: preamble size).
			methodProperties do: [ :assoc |
				assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]].
			sourceFile backChunk. "Skip back blank space."
			preamble := sourceFile backChunk ].
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod authorAndStampLocalTZFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					priorMethodRef = 0 ifFalse: [
						priorMethodRef := priorMethodRef + 16rFC000000 - 16r1000000 ].
					prevFileIndex := sourceFilesCopy fileIndexFromHistoryPointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromHistoryPointer: priorMethodRef ]].
		
		
		
 		self addItem:
				(MethodDefinitionChange new
					file: sourceFile position: position
					class: class name category: category meta: meta stamp: stamp
					properties: methodProperties
					prior: priorMethodRef
					selector: selectorOfMethod)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
					", (libNameOrNil ifNil: [''] ifNotNil: [' - [libraryName: ', libNameOrNil, ']'])."
					, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] ']).
		
		
		
		position := prevPos.
		prevPos notNil ifTrue: [
			sourceFile := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].! !

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 28/Sep/2026, 5:42:18 pm (UTC)'!
updateIfNeeded
	| aMethod |
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true
! !

!MethodVersionsBrowser methodsFor: 'menu' stamp: 'jmv 24/Jul/2025, 2:00:03 pm (UTC)'!
compareToOtherVersion
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
					format: { base stamp. case stamp })! !

!MethodVersionsBrowser methodsFor: 'menu' stamp: 'jmv 25/Nov/2010, 5:52:00 pm (UTC)'!
offerVersionsHelp
	(TextModel new contents: self versionsHelpString)
		openLabel: 'Versions Browsers'! !

!MethodVersionsBrowser methodsFor: 'menu' stamp: 'jmv 27/Mar/2024, 1:50:30 pm (UTC)'!
versionsHelpString
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

Note:  the annotation pane in versions browsers shows information about the *current* version of the method in the image, not about the selected version.'! !


!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 13/Oct/2025, 1:48:23 pm (UTC)'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorph
		withModel: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 28/Sep/2026, 4:49:22 pm (UTC)'!
createCodePaneMorph

	^(TextModelMorph
		textProvider: model
		textGetter: #acceptedContents) emptyTextDisplayMessage: 'Selection detail (no change selected?)'! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/Jul/2016, 12:28:00 pm (UTC)'!
lineDiffButtonHelp
	^'Show code differences from the previous version, line by line.'! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 28/Sep/2026, 5:30:00 pm (UTC)'!
optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph 
					model: model
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	buttons add:  self prettyLineDiffButton.
	widths add: 21.
	buttons add:  self prettyWordDiffButton.
	widths add: 23.
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 19/Mar/2012, 7:25:00 pm (UTC)'!
optionalModelButtonTuples

	^#(
		(25
		'compare to current'
		compareToCurrentVersion
		'opens a separate window which shows the text differences between the selected version and the current version')

		(10
		'revert'
		fileInSelections
		'reverts the method to the version selected')

		(7
		'help'
		offerVersionsHelp
		'further explanation about use of Versions browsers')
	)! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/Jul/2016, 1:08:00 am (UTC)'!
prettyLineDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, line by line.'! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/Jul/2016, 1:09:00 am (UTC)'!
prettyWordDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, word by word.'! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 15/Apr/2011, 5:52:00 pm (UTC)'!
windowColor
	^ Theme current versionsBrowser! !

!MethodVersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/Jul/2016, 12:29:00 pm (UTC)'!
wordDiffButtonHelp
	^'Show code differences from the previous version, word by word.'! !

!MethodVersionsBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 28/Sep/2026, 5:17:25 pm (UTC)'!
listMenuSpec
	"Spec for the menu and shortcut keys for a method version in the method versions list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
				#(0 		'Versions' 									true)
	).
	model listIndex > 0 ifTrue: [
		(model list size > 1 ) ifTrue: [
			answer := answer, #(
				#(10		'Compare to current' 									nil 	model 		compareToCurrentVersion 		nil
							'Compare selected version to the current version')
				#(20		'Compare to version...' 								nil 	model 		compareToOtherVersion 		nil
							'Compare selected version to another selected version'))].
		answer := answer, #(
				#(30		'Revert to selected version (z)' 						$z 	model 		fileInSelections 				nil
							'Resubmit the selected version, so that it becomes the current version'))].
	answer := answer, #(
				#(40		'Edit current method (O)' 								$O 	window 	openSingleMessageBrowser 	nil
							'Open a single-message browser on the current version of this method')
				#(50 		'-----')
				#(60		'Update List' 											nil 	model 		reformulateList 				nil
							'Reformulate the list of versions, in case it somehow got out of synch with reality')
				#(70 		'-----')
				#(80		'Senders (n)' 										$n 	window 	browseSenders 				nil
							'Browse all Senders of this selector')
				#(90		'Implementors (m)' 									$m 	window 	browseImplementors 			nil
							'Browse all Implementors of this selector')
				#(100 		'-----')
				#(110		'Help...' 												nil 	model 		offerVersionsHelp 				nil
							'provide an explanation of the use of this tool')
	).
	^answer! !

!MethodVersionsBrowserWindow methodsFor: 'updating' stamp: 'jmv 25/Aug/2021, 12:13:05 pm (UTC)'!
updateListsAndCodeNow
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded! !


!MethodVersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 28/Sep/2026, 5:40:57 pm (UTC)'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector
	| browser |
	browser := MethodVersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	browser toggleListIndex: 1.
	MethodVersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 28/Sep/2026, 5:00:14 pm (UTC)'!
recordedChangesForSelector: selector
	"Return a list of ChangeListElementss for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	| aList |
	aList := MethodVersionsBrowser new
			scanVersionsOf: (self compiledMethodAt: selector ifAbsent: [^ nil])
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector.
	^ aList ifNotNil: [aList changeList]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 28/Sep/2026, 5:02:28 pm (UTC)'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	model selectedMessageName isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	method := model currentCompiledMethod.
	"because maybe the method has been removed --- unhelpful"
	method isNil ifTrue: [^self].
	class := method methodClass.
	^ MethodVersionsBrowserWindow 
		browseVersionsOf: method
		class: class theNonMetaClass 
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector! !


!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 28/Sep/2026, 5:02:36 pm (UTC)'!
browseVersions
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	| class selector |
	(selector := model selectedMessageName) ifNotNil: [
		class := model selectedClassOrMetaClass.
		(class exists and: [class realClass includesSelector: selector]) ifTrue: [
			MethodVersionsBrowserWindow
				browseVersionsOf: (class realClass compiledMethodAt: selector)
				class: class realClass theNonMetaClass
				meta: class realClass isMeta
				category: model selectedMessageCategoryName
				selector: selector]]! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 29/Sep/2026, 5:03:06 pm (UTC)'!
browseVersions

	| method class browser |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		model selectedClass ifNotNil: [ :selectedClass |
			^ VersionsBrowserWindow
				browseCommentOf: selectedClass ]].
	class := method methodClass.
	browser := MethodVersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector.
	^browser! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 28/Sep/2026, 5:01:56 pm (UTC)'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChange: aChange
	| browser |
	browser := VersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	aChange ifNotNil: [browser extraChange: aChange ].
	browser toggleListIndex: 1.
	MethodVersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser! !


!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'jmv 28/Sep/2026, 5:02:51 pm (UTC)'!
browseVersions
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	| class selector method category pair |

	(selector := model selectedMessageName) ifNil: [^ self].
	class := model selectedClassOrMetaClass.
	(class includesSelector: selector)
		ifTrue: [
			method := class compiledMethodAt: selector.
			category := class whichCategoryIncludesSelector: selector ]
		ifFalse: [
			pair := model methodInfoFromRemoval: {class name. selector}.
			pair ifNil: [^ nil].
			method := CompiledMethod toReturnSelf historyPointer: pair first.
			category := pair last ].
	MethodVersionsBrowserWindow
		browseVersionsOf: method
		class: model selectedClass meta: class isMeta
		category: category selector: selector.! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 28/Sep/2026, 5:02:15 pm (UTC)'!
browseVersionsForClass: aClass selector: aSelector
	MethodVersionsBrowserWindow
		browseVersionsOf: (aClass compiledMethodAt: aSelector)
		class: aClass
		meta: aClass isMeta
		category: (aClass organization categoryOfElement: aSelector)
		selector: aSelector! !

