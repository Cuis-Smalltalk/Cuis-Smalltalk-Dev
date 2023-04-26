'From Cuis 6.0 [latest update: #5779] on 26 April 2023 at 2:47:29 pm'!

!Browser methodsFor: 'class functions' stamp: 'jmv 4/26/2023 14:42:28'!
createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage := aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ].
			(aClass canUnderstand: (setter := aName , ':') asSymbol) ifFalse: [
				newMessage := setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' := anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]! !


!Message methodsFor: 'stub creation' stamp: 'jmv 4/26/2023 14:42:51'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: self arguments first argumentName ! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 4/26/2023 14:42:59'!
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
		#(showAssignmentAsLeftArrow true )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'jmv 4/26/2023 14:43:11'!
preferredAssignmentOperator

	^ ':='! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences allPreferences removeKey: #leftArrowAssignmentsInGeneratedCode!

