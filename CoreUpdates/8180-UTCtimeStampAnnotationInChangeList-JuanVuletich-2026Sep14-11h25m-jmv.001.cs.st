'From Cuis7.9 [latest update: #8179] on 14 September 2026 at 11:26:48 am'!

!ChangeList methodsFor: 'annotation parts' stamp: 'jmv 14/Sep/2026, 2:24:10 pm (UTC)'!
timeStampString
	"Answer the time stamp for the chosen class and method, if any, else an empty string.
	Difference with other CodeProviders: Show timestamp in UTC and not in local time.
	Reason is that on the Changes / Versions list timestams are shown in UTC. Do the same in annotations
	for current version, to ease comparison."

	self selectedMessageName ifNotNil: [ :selector |
		self selectedClassOrMetaClass ifNotNil: [ :selectedClassOrMetaClass |
			^(selectedClassOrMetaClass
				compiledMethodAt: selector
				ifAbsent: [ ^'' ])
					authorAndStampUTC ]].
	^''! !


!CodeProvider methodsFor: 'annotation parts' stamp: 'jmv 14/Sep/2026, 2:19:17 pm (UTC)'!
timeStampString
	"Answer the time stamp for the chosen class and method, if any, else an empty string"

	self selectedMessageName ifNotNil: [ :selector |
		self selectedClassOrMetaClass ifNotNil: [ :selectedClassOrMetaClass |
			^ selectedClassOrMetaClass stampAt: selector]].
	^''! !


!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForMessageCategory:ofClass: annotationForSelector:ofClass: annotationForSystemCategory: annotationSeparator)
('annotation parts' allBaseSystemChangeSetsOf:selector:printOn: changeSetsOf:selector:printOn: documentationOf:selector:printOn: firstCommentOf:selector:printOn: implementorsCountOf:selector:printOn: libraryOf:selector:printOn: linesOfCodeOf:selector:printOn: masterCommentOf:selector:printOn: messageCategoryOf:selector:printOn: messageSendsCountOf:selector:printOn: packagesOf:selector:printOn: priorTimeStampOf:selector:printOn: priorVersionsCountOf:selector:printOn: propertiesOf:selector:printOn: sendersCountOf:selector:printOn: timeStampOf:selector:printOn: timeStampString)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText contentsSymbol contentsSymbol: currentMethodRefactored methodNodeOf:ifErrorsParsing:)
('commands' removeClass)
('controls' whatToShowSubmenuSpec)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleLineDiffing togglePrettyLineDiffing togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState)
('self-updating' didCodeChangeElsewhere updateIfNeeded)
('what to show' showByteCodes showDecompile showLineDiffs showPrettyLineDiffs showPrettyPrint showPrettyWordDiffs showProperties showSource showWordDiffs showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingPrettyPrint showingPrettyPrintString showingProperties showingPropertiesString showingSource showingSourceString)
('message list' decompiledSource messageList selectedBytecodes selectedMessage selector:refactoredTo: sourceStringPrettifiedAndDiffed)
('message list menu' copySelector fileOutMessage inspectCompiledMethod revertToPreviousVersion unusedMethods)
('testing' is:)
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext doItReceiver)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty: isModeStyleable styleByParagraphs)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('private' labelForClass:)
!

