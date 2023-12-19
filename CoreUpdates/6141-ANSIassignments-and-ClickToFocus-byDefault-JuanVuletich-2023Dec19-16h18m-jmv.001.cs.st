'From Cuis6.0 [latest update: #6140] on 19 December 2023 at 4:19:37 pm'!

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 12/19/2023 16:12:42'!
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
		#(showAssignmentAsLeftArrow false )
		#(wantsMenuIcons true )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 12/19/2023 16:12:25'!
prefGui
	^ #( 
		#(balloonHelpEnabled true )
		#(biggerCursors false )		
		#(cacheDisplayContentWhenMovingMorphs true )
		#(cheapWindowReframe false )		
		#(clickGrabsMorphs false )	
		#(ctrlArrowsScrollHorizontally false ) 
		#(drawKeyboardFocusIndicator true )
		#(focusFollowsMouse false )
		#(focusIndicatorWidth 1)
		#(fullScreenLeavesDeskMargins true )
		#(haloEnclosesFullBounds true )
		#(halosShowCoordinateSystem true )
		#(menuKeyboardControl true )
		#(optionalButtons true )		
		#(selectiveHalos true )
		#(tapAndHoldEmulatesButton2 true )
		#(tileResizerInWindowMenu true )
		#(wantsMenuIcons true )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 12/19/2023 16:12:50'!
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
		#(showAssignmentAsLeftArrow false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #focusFollowsMouse category: #gui value: false.
Preferences name: #showAssignmentAsLeftArrow category: #programming value: false.!

