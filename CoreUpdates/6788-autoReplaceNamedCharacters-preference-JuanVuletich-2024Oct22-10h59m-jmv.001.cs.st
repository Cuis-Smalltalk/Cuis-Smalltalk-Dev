'From Cuis7.1 [latest update: #6787] on 22 October 2024 at 11:00:15 am'!

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/22/2024 10:59:56'!
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
		#(autoReplaceNamedCharacters true)
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/22/2024 10:44:36'!
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
		#(clearPackagePathsOnImageMove true)
	)! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 10/22/2024 10:45:53'!
installMiscPreferences
" Preferences with value defined as a closure. We could insert in a dynamic array...
  Preferences with an event handler.
  Preferences with default value of anything but simple literals."
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];

		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];

		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;

		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];

		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch;

		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];

		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];

		name: #worldMenu
		category: #system
		value: TheWorldMenu;

		name: #browserWindowClass
		category: #programming
		value: BrowserWindow.! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/22/2024 10:56:14'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |

	"To enable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: true.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"To disable:
	Preferences name: #autoReplaceNamedCharacters category: #gui value: false.
	Preferences saveToDisk: #autoReplaceNamedCharacters.
	PreferenceSet saveUserPrefs.
	"
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	(Preferences at: #autoReplaceNamedCharacters) ifTrue: [
		aKeyboardEvent keyCharacter isLetter ifFalse: [
			string := self privateCurrentString.
			"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
			self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
				(string at: i) = $\ ifTrue: [
					key := string copyFrom: i + 1 to: self pointIndex - 1.
					Character namedCharactersMap at: key ifPresent: [ :value |
						self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value) ].
					^ super normalCharacter: aKeyboardEvent]]]].

	^ super normalCharacter: aKeyboardEvent! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences
		name: #autoReplaceNamedCharacters
		category: #gui
		value: true.
Preferences saveToDisk: #autoReplaceNamedCharacters.
PreferenceSet saveUserPrefs.
!

