'From Cuis7.9 [latest update: #7994] on 10 June 2026 at 9:39:48 am'!

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/Jun/2026 09:37:01'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
		#(browseWithPrettyPrint false )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugPreDebugger false )
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
		#(workspacesStyleByParagraphs true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(classAnnotations #(instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(messageCategoryAnnotations #(messagesCount messageSendsCount) )
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount packages library changeSets) )	
		#(syntaxHighlightingAsYouType true )
		#(showAssignmentAsLeftArrow false )
		#(replaceUnicodeArrowsWithSmalltalkArrows true)
		#(useWideMinusAndCenteredAsteriskGlyphs true )
		#(clearPackagePathsOnImageMove true)
	)! !


!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 10/Jun/2026 09:39:29'!
replaceUnicodeArrowsWithSmalltalkArrows
	"Useful, for instance, to paste code from TheCuisBook.pdf.
	Default is true to help newcomers.
	Experienced users will most likely turn this preference off."

	^Preferences at: #replaceUnicodeArrowsWithSmalltalkArrows! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #replaceUnicodeArrowsWithSmalltalkArrows category: #programming value: true.
Preferences saveToDisk: #replaceUnicodeArrowsWithSmalltalkArrows.!

