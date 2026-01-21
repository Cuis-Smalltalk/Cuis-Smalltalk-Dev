'From Cuis7.7 [latest update: #7803] on 21 January 2026 at 2:27:14 pm'!

!Workspace methodsFor: 'shout styling' stamp: 'jmv 21/Jan/2026 14:26:16'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^Preferences at: #workspacesStyleByParagraphs! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 21/Jan/2026 14:25:07'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
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
		#(usePreDebugWindow false)
		#(clearPackagePathsOnImageMove true)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #workspacesStyleByParagraphs category: #programming value: true.
Preferences saveToDisk: #workspacesStyleByParagraphs.!

