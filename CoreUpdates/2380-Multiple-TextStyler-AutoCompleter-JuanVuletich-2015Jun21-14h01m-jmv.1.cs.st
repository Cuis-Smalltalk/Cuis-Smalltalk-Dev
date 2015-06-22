'From Cuis 4.2 of 25 July 2013 [latest update: #2378] on 21 June 2015 at 2:08:59.910028 pm'!

!Object methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:52'!
autoCompleterClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil! !

!Object methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:52'!
editorClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^TextEditor! !

!Object methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:52'!
textStylerClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil! !


!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
autoCompleterClassFor: textGetter
	^nil! !

!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
editorClassFor: textGetter
	^TextEditor! !

!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
textStylerClassFor: textGetter
	^nil! !


!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
autoCompleterClassFor: textGetter
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass autoCompleterClass ].
	^SmalltalkCompleter! !

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
editorClassFor: textGetter
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor! !

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:48'!
textStylerClassFor: textGetter
	textGetter = #acceptedContents ifFalse: [
		^super textStylerClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass textStylerClass ].
	^SHTextStylerST80! !


!FileList methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:05'!
autoCompleterClassFor: textGetter
	^SmalltalkCompleter! !

!FileList methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:05'!
editorClassFor: textGetter
	^SmalltalkEditor! !


!Inspector methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:06'!
autoCompleterClassFor: textGetter
	^SmalltalkCompleter! !

!Inspector methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:06'!
editorClassFor: textGetter
	^SmalltalkEditor! !


!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
autoCompleterClass
	^textProvider autoCompleterClassFor: textGetter! !

!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
editorClass
	^textProvider editorClassFor: textGetter! !

!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 13:44'!
textStylerClass
	^textProvider textStylerClassFor: textGetter! !

!methodRemoval: TextModelMorph #autoCompleter:!
TextModelMorph removeSelector: #autoCompleter:!
!methodRemoval: TextModelMorph #styler:!
TextModelMorph removeSelector: #styler:!

!TextModelMorph reorganize!
('accessing' disableEdition editor textMorph wrapFlag:)
('dependents access' canDiscardEdits)
('drawing' drawOn: wantsFrameAdornments)
('editor access' scrollSelectionIntoView selectAll)
('events' doubleClick:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseEnter:)
('event handling' mouseButton2Activity)
('focus handling' focusText)
('geometry' innerHeight: scrollDeltaHeight)
('initialization' acceptOnCR: defaultColor innerMorphClass model:)
('interactive error protocol' correctFrom:to:with: nextTokenFrom:direction: notify:at:in: selectFrom:to: selectInvisiblyFrom:to: selectionInterval)
('model access' setSelection: setTextColor: text)
('notifications' possiblyChanged)
('scrolling' mightNeedHorizontalScrollBar)
('unaccepted edits' askBeforeDiscardingEdits: hasUnacceptedEdits:)
('updating' update:)
!

!methodRemoval: Inspector #autoCompleterClass!
Inspector removeSelector: #autoCompleterClass!
!methodRemoval: Inspector #editorClass!
Inspector removeSelector: #editorClass!

!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass timeOfLastListUpdate update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('updating' updateListsAndCodeIn:)
('object fileIn' convertToCurrentVersion:refStream:)
('private' printStringErrorText)
('stepping' stepAt:in:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor:)
!

!methodRemoval: FileList #autoCompleterClass!
FileList removeSelector: #autoCompleterClass!
!methodRemoval: FileList #editorClass!
FileList removeSelector: #editorClass!

!FileList reorganize!
('file list' fileList fileListIndex fileListIndex: fileName readOnlyStream)
('user interface support' autoCompleterClassFor: editorClassFor:)
('file list menu' itemsForAnyFile1 itemsForAnyFile2 itemsForFile: itemsForNoFile)
('file menu action' addNew:byEvaluating: addNewDirectory addNewFile deleteFile get getHex renameFile sortByDate sortByName sortBySize)
('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices initialDirectoryList initialize labelString universalButtonServices updateDirectory)
('menu messages' copyName)
('own services' serviceAddNewDirectory serviceAddNewFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace viewContentsInWorkspace)
('updating' update:)
('volume list and pattern' deleteDirectory directory doesPattern:allow: fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: listForSelectingPatterns:rejectingPatterns: pattern pattern:)
('private' acceptedStringOrText currentDirectorySelected defaultContents directoryNamesFor: fileNameFromFormattedItem: folderString fullName postOpen put: readContentsBrief: readContentsHex: resort: setSelectedDirectoryTo: sortBlock sortingByDate sortingByName sortingBySize updateFileList)
('accessing' brevityState)
!

!methodRemoval: CodeProvider #autoCompleterClass!
CodeProvider removeSelector: #autoCompleterClass!
!methodRemoval: CodeProvider #editorClass!
CodeProvider removeSelector: #editorClass!
!methodRemoval: CodeProvider #textStylerClass!
CodeProvider removeSelector: #textStylerClass!
!methodRemoval: TextProvider #autoCompleterClass!
TextProvider removeSelector: #autoCompleterClass!
!methodRemoval: TextProvider #editorClass!
TextProvider removeSelector: #editorClass!
!methodRemoval: TextProvider #textStylerClass!
TextProvider removeSelector: #textStylerClass!
!methodRemoval: Object #autoCompleterClass!
Object removeSelector: #autoCompleterClass!
!methodRemoval: Object #editorClass!
Object removeSelector: #editorClass!
