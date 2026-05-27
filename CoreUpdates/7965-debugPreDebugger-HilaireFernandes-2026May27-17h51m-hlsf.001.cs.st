'From Cuis7.7 [latest update: #7964] on 27 May 2026 at 5:53:12 pm'!

!DebuggerWindow class methodsFor: 'as yet unclassified' stamp: 'hlsf 16/May/2026 10:13:58'!
open: model label: aString
	| debuggerWindow |
	debuggerWindow := super open: model label: aString.
	debuggerWindow morphPosition: Display extent - debuggerWindow morphExtent // 2.
	^ debuggerWindow! !


!Debugger class methodsFor: 'opening' stamp: 'hlsf 16/May/2026 10:30:03'!
openProcess: process context: context label: labelString
	"Open a Debugger on an existing process."

	| debugger logFile label |
	<primitive: 19> "Simulation guard"
	label := labelString,
					' - Process: ', process name,
					' - Priority: ', process priority printString,
					' (', process hash printString, ')'.
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		logFile := 'CuisDebug'.
		(labelString beginsWith: 'Space is low')
			ifTrue: [logFile := 'LowSpaceDebug'].
		Smalltalk logError: label inContext: context to: logFile ].

	debugger := self new.
	debugger process: process context: context.
	debugger 
		openWindowLabel: label 
		usePreDebugWindow: (Preferences at: #debugPreDebugger) 
		preDebugMessage: nil.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'hlsf 16/May/2026 10:34:58'!
initialExtent
	^ `600 @ 300` * (Preferences at: #standardCodeFont) lineSpacing / 18! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'hlsf 16/May/2026 10:10:27'!
open: model label: aString message: otherString
	|  window |

	window := self new.
	window
		model: model;
		messageText: otherString;
		buildMorphicWindow.
	aString ifNotNil: [ window setLabel: aString ].
	window openInWorld.
	window morphPosition: Display extent - window morphExtent // 2.
	^window! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 27/May/2026 17:43:55'!
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
		#(clearPackagePathsOnImageMove true)
	)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #debugPreDebugger category: #programming value: false.
Preferences saveToDisk: #debugPreDebugger.
Preferences removeKey: #usePreDebugWindow.!

