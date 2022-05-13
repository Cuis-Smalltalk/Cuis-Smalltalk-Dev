'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 12:06:56 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'hlsf 5/12/2022 11:52:14'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), 
		(PreferenceNG at: #userChangesFileNameExtension)! !

!SystemDictionary methodsFor: 'ui' stamp: 'hlsf 5/12/2022 12:06:12'!
beep
	"
	Smalltalk beep
	"
	(PreferenceNG at: #soundsEnabled) ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !


!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 12:05:02'!
prefProgramming
	^ #(
		#(allowBlockArgumentAssignment false )
		#(alternativeBrowseIt false )
		#(assignmentGlyphSelector useLeftArrow #(useLeftArrow useAlwaysLeftArrow) assignmentGlyph: )
		#(backgroundColorFillsAllBackground true )
		#(browseWithPrettyPrint false )
		#(classAnnotations #(instanceMethodsCount classMethodsCount linesOfCode) )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugShowDamage false )
		#(decorateBrowserButtons true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(extraDebuggerButtons true )
		#(fullPrintItInWorkspaces false )
		#(highlightBlockNesting true )
		#(italicsInShout true)
		#(leftArrowAssignmentsInGeneratedCode false )
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(methodAnnotations #(timeStamp linesOfCode messageCategory implementorsCount sendersCount  packages changeSets) )	
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount linesOfCode) )
		#(syntaxHighlightingAsYouType true )
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 11:50:23'!
prefSystem
	^ #( #(allowNonLocalReturnsInExceptionHandlers true )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges false )
		#(caseSensitiveFinds false )
		#(changeSetVersionNumbers true )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #roots #(root image vm current) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
	)! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 5/12/2022 11:53:18'!
setDefaultFont: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	PreferenceNG
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#standardListFont. (PreferenceNG at: #standardListFont) pointSize.}.
				{#standardMenuFont. (PreferenceNG at: #standardMenuFont) pointSize.}.
				{#windowTitleFont. (PreferenceNG at: #windowTitleFont) pointSize.}.
				{#standardCodeFont. (PreferenceNG at: #standardCodeFont) pointSize.}.
				{#standardButtonFont. (PreferenceNG at: #standardButtonFont) pointSize.}.
			}.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !


!FontFamily class methodsFor: 'ui' stamp: 'hlsf 5/12/2022 11:57:49'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| selectedNameOrDirectory fontFamily |
	selectedNameOrDirectory _ self promptUserFolders.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^PreferenceNG setDefaultFont: it familyName ].
	(Feature require: 'VectorGraphics') ifTrue: [
		(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
			Feature require: 'VectorEngineInSmalltalk' ].
		UISupervisor whenUIinSafeState: [
			fontFamily _ FontFamily availableFamilies values
				detect: [ :any | any folderName = selectedNameOrDirectory name ]
				ifNone: [
					FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ].
			PreferenceNG setDefaultFont: fontFamily familyName ]].! !


!WorldMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 11:58:44'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	(PreferenceNG at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(PreferenceNG at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'hlsf 5/12/2022 12:00:41'!
open: model label: aString message: messageString
	
	((PreferenceNG at: #usePreDebugWindow) or: [messageString notNil])
		ifTrue: [self forceOpen: model label: aString message: messageString]
		ifFalse: [model openFullMorphicLabel: aString ]! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'hlsf 5/12/2022 12:02:28'!
dispatchWith: aMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph ].

	"Try to get out quickly"
	(aMorph fullIncludesPixel: position)
		ifFalse: [ ^ #rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			(eachChild dispatchEvent: self) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ (aMorph rejectsEvent: self) not and: [ aMorph fullIncludesPixel: position ] ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sendEventTo: aMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: ((PreferenceNG at: #clickGrabsMorphs) ifFalse: [#dragEvent:localPosition:])
						clkSel: ((PreferenceNG at: #clickGrabsMorphs) ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					(PreferenceNG at: #clickGrabsMorphs) ifFalse: [
						self shiftPressed
							ifTrue: [ aMorph is: #SystemWindow :: ifTrue: [ aMorph sendToBack ] ]
							ifFalse: [ aMorph activateWindow ] ].
					self wasHandled: true.
					^ self ]]].

	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected! !

!methodRemoval: PreferenceNG class #import stamp: 'hlsf 5/12/2022 11:52:39'!
PreferenceNG class removeSelector: #import!
