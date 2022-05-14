'From Cuis 6.0 [latest update: #5160] on 13 May 2022 at 5:25:53 pm'!

!Parser methodsFor: 'scanning' stamp: 'jmv 5/13/2022 17:24:55'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a series of consecutive vertical bars, up arrows, colons and regular binary selector characters.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain.
	Another special case is binary selectors ending in $-, like in
	1@-2
	This could be interpreted as `1 @ -2` or `1 @- 2`. Preference #atMinusDigitMeaning tells what to do."

	| toMakeBinary |
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	"Note: Also include #binary, to allow stuff like #+| where a special character comes after after a regular binary operator character. "
	toMakeBinary _ #(binary verticalBar upArrow colon).
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here _ here asPlainString.
	hereType _ #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			(token == #- and: [source peekBack isDigit])
				ifTrue: [
					(PreferenceNG at: #atMinusDigitMeaning) == #disabled
						"Disallow `1@-2`. Insists on some whitespace to disambiguate."
						ifTrue: [ ^self expected: 'A space character after selector' ].
					(PreferenceNG at: #atMinusDigitMeaning) == #st80
						ifTrue: [ ^self ].				"interpret `1@-2` like `1 @ -2`"
					"Assume (PreferenceNG at: #atMinusDigitMeaning) == #ansiSmalltalk
					interpret `1@-2` like `1 @- 2`, i.e. consider #@- a valid binary selector"
					].
			here _ here , token asPlainString.
			hereEnd _ hereEnd + 1.
			self scanToken ].! !


!PreferenceNG class methodsFor: 'data' stamp: 'jmv 5/13/2022 17:15:11'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceNG name: #atMinusDigitMeaning description: 'Specifies behavior for code such as `@-2`.' category: #programming type: #(st80 ansiSmalltalk disabled) value: #st80!

