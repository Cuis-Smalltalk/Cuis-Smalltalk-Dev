'From Cuis7.9 [latest update: #8128] on 4 August 2026 at 5:17:13 pm'!

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:11:41 pm (UTC)'!
allBaseSystemChangeSetsOf: aClass selector: aSelector printOn: strm
	"Print the #allBaseSystemChangeSets method annotation."

	| aList |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	aList := (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
	aList notEmpty
		ifTrue: [ aList size = 1
				ifTrue: [strm nextPutAll: 'only in base system change set']
				ifFalse: [strm nextPutAll: 'in base system change sets:'].
			aList
				do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
				separatedBy: [ strm nextPut: $, ]]
		ifFalse: [strm nextPutAll: 'in no base system change set'].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:11:36 pm (UTC)'!
changeSetsOf: aClass selector: aSelector printOn: strm
	"Print the #changeSets method annotation."

	| aList |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
	aList notEmpty
		ifTrue: [ aList size = 1
				ifTrue: [strm nextPutAll: 'only in change set']
				ifFalse: [strm nextPutAll: 'in change sets:'].
			aList
				do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
				separatedBy: [ strm nextPut: $, ]]
		ifFalse: [strm nextPutAll: 'in no change set'].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:11:15 pm (UTC)'!
documentationOf: aClass selector: aSelector printOn: strm
	"Print the #documentation method annotation."

	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']).

! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:11:04 pm (UTC)'!
firstCommentOf: aClass selector: aSelector printOn: strm
	"Print the #firstComment method annotation."

	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	strm nextPutAll: (aClass firstCommentAt: aSelector).! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:58 pm (UTC)'!
implementorsCountOf: aClass selector: aSelector printOn: strm
	"Print the #implementorsCount method annotation."

	| implementorsCount |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	implementorsCount := Smalltalk numberOfImplementorsOf: aSelector.
	implementorsCount := implementorsCount = 1
			ifTrue: ['1 implementor']
			ifFalse: [implementorsCount printString , ' implementors'].
	strm nextPutAll: implementorsCount.
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:49 pm (UTC)'!
libraryOf: aClass selector: aSelector printOn: strm
	"Print the #library method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
		(method propertyValueAt: #libraryName)
			ifNil: [ strm nextPutAll: 'in no library' ]
			ifNotNil: [ :libraryName |
				strm nextPutAll: 'in library '; nextPutAll: libraryName ]].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:37 pm (UTC)'!
linesOfCodeOf: aClass selector: aSelector printOn: strm
	"Print the #linesOfCode method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
		strm
			print: method linesOfCode;
			nextPutAll: ' lines of code' ].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:29 pm (UTC)'!
masterCommentOf: aClass selector: aSelector printOn: strm
	"Print the #masterComment method annotation."

	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']).
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:21 pm (UTC)'!
messageCategoryOf: aClass selector: aSelector printOn: strm
	"Print the #messageCategory method annotation."

	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) .
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:13 pm (UTC)'!
messageSendsCountOf: aClass selector: aSelector printOn: strm
	"Print the #messageSendsCount method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
		strm
			print: method messageSendsCount;
			nextPutAll: ' message sends' ].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:10:06 pm (UTC)'!
packagesOf: aClass selector: aSelector printOn: strm
	"Print the #packages method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
		(CodePackage packageOfMethod: method methodReference ifNone: nil)
			ifNil: [ strm nextPutAll: 'in no package' ]
			ifNotNil: [ :codePackage |
				strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:13:11 pm (UTC)'!
priorTimeStampOf: aClass selector: aSelector printOn: strm
	"Print the #priorTimeStamp method annotation."

	| stamp |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	stamp := VersionsBrowser
			timeStampFor: aSelector
			class: aClass
			reverseOrdinal: 2.
	strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']).
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:09:41 pm (UTC)'!
priorVersionsCountOf: aClass selector: aSelector printOn: strm
	"Print the #implementorsCount method annotation."

	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm.
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:09:28 pm (UTC)'!
propertiesOf: aClass selector: aSelector printOn: strm
	"Print the #properties method annotation."

	(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :method |
		method propertiesAsArray ifNotEmpty: [ :it |
			strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
			it printOn: strm ]].
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:06:43 pm (UTC)'!
sendersCountOf: aClass selector: aSelector printOn: strm
	"Print the #sendersCount method annotation."

	| sendersCount |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	sendersCount := Smalltalk numberOfSendersOf: aSelector.
	sendersCount := sendersCount = 1
			ifTrue: ['1 sender']
			ifFalse: [sendersCount printString , ' senders'].
	strm nextPutAll: sendersCount.
! !

!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 4/Aug/2026, 8:06:39 pm (UTC)'!
timeStampOf: aClass selector: aSelector printOn: strm
	"Print the #timeStamp method annotation."

	| stamp |
	strm atStart ifFalse: [ strm nextPutAll: self annotationSeparator ].
	stamp := self timeStamp.
	strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp']).
! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 4/Aug/2026, 8:05:01 pm (UTC)'!
annotationForSelector: aSelector ofClass: aClass
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	| method |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	method := aClass compiledMethodAt: aSelector ifAbsent: nil.
	^ String streamContents: [ :strm |
		(Preferences at: #methodAnnotations)
			do: [ :each |
				self perform: (each, 'Of:selector:printOn:') asSymbol withArguments: {aClass. aSelector. strm} ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]].! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 4/Aug/2026, 8:16:31 pm (UTC)'!
machineSlow
	^ #(
		(drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
		(wantsMenuIcons false )
		(methodAnnotations #(timeStamp messageCategory packages changeSets properties) )
		(classAnnotations  #(instanceMethodsCount classMethodsCount) )
		(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount) )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 4/Aug/2026, 8:16:24 pm (UTC)'!
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
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount packages library changeSets properties) )	
		#(syntaxHighlightingAsYouType true )
		#(showAssignmentAsLeftArrow false )
		#(replaceUnicodeArrowsWithSmalltalkArrows true)
		#(useWideMinusAndCenteredAsteriskGlyphs true )
		#(clearPackagePathsOnImageMove true)
	)! !

!methodRemoval: CodeProvider #allChangeSetsOf:selector:printOn: stamp: 'jmv 4/Aug/2026, 7:54:09 pm (UTC)'!
CodeProvider removeSelector: #allChangeSetsOf:selector:printOn:!

!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForMessageCategory:ofClass: annotationForSelector:ofClass: annotationForSystemCategory: annotationSeparator)
('annotation parts' allBaseSystemChangeSetsOf:selector:printOn: changeSetsOf:selector:printOn: documentationOf:selector:printOn: firstCommentOf:selector:printOn: implementorsCountOf:selector:printOn: libraryOf:selector:printOn: linesOfCodeOf:selector:printOn: masterCommentOf:selector:printOn: messageCategoryOf:selector:printOn: messageSendsCountOf:selector:printOn: packagesOf:selector:printOn: priorTimeStampOf:selector:printOn: priorVersionsCountOf:selector:printOn: propertiesOf:selector:printOn: sendersCountOf:selector:printOn: timeStampOf:selector:printOn:)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText contentsSymbol contentsSymbol: currentMethodRefactored methodNodeOf:ifErrorsParsing:)
('commands' removeClass)
('controls' whatToShowSubmenuSpec)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleLineDiffing togglePrettyLineDiffing togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' showByteCodes showDecompile showLineDiffs showPrettyLineDiffs showPrettyPrint showPrettyWordDiffs showSource showWordDiffs showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingPrettyPrint showingPrettyPrintString showingSource showingSourceString)
('message list' decompiledSource messageList selectedBytecodes selectedMessage selector:refactoredTo: sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion timeStamp unusedMethods)
('testing' is:)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext doItReceiver)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty: isModeStyleable styleByParagraphs)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('private' labelForClass:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences at: #methodAnnotations put: (Preferences at: #methodAnnotations), #(properties)!

