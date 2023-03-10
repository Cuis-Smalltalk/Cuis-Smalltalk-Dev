'From Cuis 6.0 [latest update: #5681] on 10 March 2023 at 11:09:29 am'!
"Change Set:		5682-CuisCore-AuthorName-2023Mar10-10h55m
Date:			10 March 2023
Author:			Juan Vuletich"!


!SHTextStylerST80 commentStamp: 'jmv 3/10/2023 11:08:08' prior: 0!
I style Smalltalk methods and expressions.

My 'styleTable' class instance var holds an array ofArrays which control how each token is styled/coloured. See my defaultStyleTable class method for its structure.
My styleTable can be changed by either modifying the defaultStyleTable class method and then executing SHTextStylerST80 initialize ; or by giving me a new styleTable through my #styleTable: class method.

My 'textAttributesByPixelSize' class instance var contains a dictionary of dictionaries.
	The key is a pixelSize and the value a Dictionary from token type Symbol to TextAttribute array.
	It is created/maintained automatically.

I also install these 2 preferences when my class initialize method is executed....
	#syntaxHighlightingAsYouType  - controls whether methods are styled in browsers
	#showAssignmentAsLeftArrow - controls whether assignments are shown in the Smalltalk-80 style

I reimplement #unstyledTextFrom: so that TextActions are preserved in the unstyled text 
!


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 3/10/2023 11:07:38'!
machineDefault
	^ #(
		#(drawKeyboardFocusIndicator true )
		#(balloonHelpEnabled true )
		#(biggerCursors false )
		#(browseWithPrettyPrint false )
		#(caseSensitiveFinds false )
		#(checkForSlips true )
		#(cmdDotEnabled true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(menuKeyboardControl true )
		#(optionalButtons true )
		#(extraDebuggerButtons true )
		#(subPixelRenderFonts true )
		#(thoroughSenders true )
		#(cheapWindowReframe false )
		#(syntaxHighlightingAsYouType true )
		#(tapAndHoldEmulatesButton2 true )
		#(clickGrabsMorphs false )
		#(showAssignmentAsLeftArrow true )
		#(wantsMenuIcons true )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 3/10/2023 11:07:45'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
		#(backgroundColorFillsAllBackground true )
		#(browseWithPrettyPrint false )
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
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(classAnnotations #(instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(messageCategoryAnnotations #(messagesCount messageSendsCount) )
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount  packages changeSets) )	
		#(syntaxHighlightingAsYouType true )
		#(showAssignmentAsLeftArrow true )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/10/2023 11:08:15'!
privateFormatAndConvert

	"Turn := into an arrow, only if requested"
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn $_ and := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString.
			self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString ]
		ifFalse: [
			"Turn $_ into :="
			self replaceStringForRangesWithType: #assignment with: ':='  ].! !


!SHTextStylerST80 class methodsFor: 'class initialization' stamp: 'jmv 3/10/2023 10:57:27'!
initialize  
	"Clear styleTable and textAttributesByPixelSize cache so that they will 
	reinitialize.	 

		SHTextStylerST80 initialize
	" 
	
	styleTable := nil.
	textAttributes := nil.	! !

!methodRemoval: SHTextStylerST80 class #ansiAssignmentPreferenceChanged stamp: 'jmv 3/10/2023 10:57:08'!
SHTextStylerST80 class removeSelector: #ansiAssignmentPreferenceChanged!
!methodRemoval: SHTextStylerST80 class #leftArrowAssignmentPreferenceChanged stamp: 'jmv 3/10/2023 10:58:38'!
SHTextStylerST80 class removeSelector: #leftArrowAssignmentPreferenceChanged!
SHTextStylerST80 initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
(Preferences allPreferences keys select: [ :s | (Smalltalk allCallsOn: s) isEmpty ]) do: [ :s | Preferences allPreferences removeKey: s ].
Preferences name: #showAssignmentAsLeftArrow category: #programming value: true.!

