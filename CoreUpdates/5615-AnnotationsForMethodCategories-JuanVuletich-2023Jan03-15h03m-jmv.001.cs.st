'From Cuis 6.0 [latest update: #5614] on 3 January 2023 at 3:15:04 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 1/3/2023 15:14:00'!
annotationForMessageCategory: aCategory ofClass: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Message Category: ';
			nextPutAll: aCategory.
		(Preferences at: #messageCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#messagesCount] -> [
					strm
						print: (aClass messagesCountInCategory: aCategory);
						nextPutAll: ' total messages' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass messageSendsCountInCategory: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 1/3/2023 15:06:34'!
selectorsInCategory: aSymbol 
	"Answer a list of the method selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl _ Set withAll: (aSymbol == ClassOrganizer allCategory
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/3/2023 15:11:56'!
messageSendsCountInCategory: aCategory
	"Number of message sends in code for methods in a method category.
	Object messageSendsCountInCategory: 'accessing'
	"

	| lines |
	lines _ 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) messageSendsCount ].
	^lines! !

!ClassDescription methodsFor: 'source code management' stamp: 'jmv 1/3/2023 15:13:45'!
messagesCountInCategory: aCategory
	"Number of methods in a method category.
	Object messagesCountInCategory: 'accessing'
	"

	| lines |
	lines _ 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines _ lines + 1 ].
	^lines! !


!Browser methodsFor: 'annotation' stamp: 'jmv 1/2/2023 17:34:05'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategory ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [
			self selectedMessageCategoryName ifNotNil: [ :messageCategory |
				^self annotationForMessageCategory: messageCategory ofClass: aClass ].
			^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 1/3/2023 15:12:59'!
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
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !

!methodRemoval: ClassDescription #methodsInCategory: stamp: 'jmv 1/3/2023 15:07:20'!
ClassDescription removeSelector: #methodsInCategory:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
PreferenceSet initProgramming!

