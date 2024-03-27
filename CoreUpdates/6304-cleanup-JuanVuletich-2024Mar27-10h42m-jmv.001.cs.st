'From Cuis6.3 [latest update: #6302] on 27 March 2024 at 10:53:51 am'!

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 3/27/2024 10:51:29'!
dateCommentLastSaved
	"Answer a Date object indicating when my class comment was last saved.  If there is no date stamp, or one of the old-time <historical>  guys, return nil"
	"RecentMessageSet organization dateCommentLastSaved"

	| aStamp tokens |
	(aStamp := self commentStamp) isEmptyOrNil ifTrue: [^ nil].
	tokens := aStamp findBetweenSubStrs: ' 
'.  "space is expected delimiter, but cr is sometimes seen, though of mysterious provenance"
	^ tokens size > 1
		ifTrue: [
			[tokens second asDate] ifError: nil]! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 3/27/2024 10:51:49'!
dateMethodLastSaved
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSaved) dateMethodLastSaved
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]! !


!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/27/2024 10:48:57'!
toggleDecompile
	"Toggle the setting of the showingDecompile flag, unless there are unsaved edits that the user declines to discard"

	self showDecompile: self showingDecompile not! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/27/2024 10:49:02'!
toggleShowDocumentation
	"Toggle the setting of the showingDocumentation flag, unless there are unsaved edits that the user declines to discard"

	self showDocumentation: self showingDocumentation not! !


!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/27/2024 10:49:30'!
reformulateListNoting: newSelector
	"A method has possibly been saved for the receiver with newSelector as its selector.
	If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	^ self reformulateList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/27/2024 10:50:02'!
sortByDate
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs := messageList collect: [ :aRef |
		aDate := aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSaved]
			ifFalse: [
				aCompiledMethod := aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateCommentLastSaved]].
		aRef -> (aDate ifNil: [Date fromString: '01/01/1996'])].  "The dawn of Squeak history"
	inOrder := assocs asArray sort: [ :a :b | a value < b value].

	inOrder do: [ :each | each key prefixStringVersionWith: each value yyyymmdd ].
	messageList := inOrder collect: [ :assoc | assoc key ].
	self changed: #messageList! !


!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 3/27/2024 10:50:30'!
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


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/27/2024 10:50:49'!
contents: input notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	(class := self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [codeString isEmpty]) ifTrue: [ ^ false].
		(codeString count: [:char | char = $"]) odd 
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
	^ true! !


!TextModelMorph methodsFor: 'drawing' stamp: 'jmv 3/27/2024 10:52:52'!
drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw := Preferences at: #focusIndicatorWidth.
	bc := nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be saved in this state"
				bc := Color tan ]
			ifFalse: [
				scroller hasEditingConflicts
					ifTrue: [
						bw := 3.
						bc := Color red ]
					ifFalse: [
						scroller hasUnacceptedEdits ifTrue: [ bc := Color red ]]]].
	(drawKeyboardFocusIndicator and: [ scroller hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc := Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc := bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 3/27/2024 10:46:13'!
cleanUpWorld
	(SelectionMenu confirm:
'This will remove all windows except those containing unsaved text.
It will also remove all non-window morphs on the desktop.
Are you sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonWindowRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/27/2024 10:53:07'!
windowsMenuOptions

	^`{
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsaved changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#submenuOf  ->  					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Tile open windows'.
				#object          			-> 		TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		40.
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		50.
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		60.
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#submenuOf  -> 					TheWorldMenu windowsLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and all non-window morphs on the world.'
			} asDictionary.
		}`! !

!methodRemoval: CompiledMethod #dateMethodLastSubmitted stamp: 'jmv 3/27/2024 10:52:00'!
CompiledMethod removeSelector: #dateMethodLastSubmitted!
!methodRemoval: BasicClassOrganizer #dateCommentLastSubmitted stamp: 'jmv 3/27/2024 10:50:06'!
BasicClassOrganizer removeSelector: #dateCommentLastSubmitted!
