'From Cuis7.1 [latest update: #6620] on 23 August 2024 at 10:01:10 am'!

!TextProvider methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:35:11'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling.
	Not even asked if text stylyng makes no sense for this model.
	True by default."

	^true! !


!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:14'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer false if showing difs, to veto the styling."

	^self showingAnyKindOfDiffs not! !


!Browser methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:21'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	| type |
	self isModeStyleable ifFalse: [^false].
	type := self editSelection.
	^ #(newMessage editMessage editClass newClass) includes: type.! !


!CodeFileBrowser methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:25'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!MethodSet methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:28'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!ChangeList methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:31'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				^true ]]].
	^false! !


!ChangeSorter methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:34'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	^self currentSelector notNil! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:34:37'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!Inspector methodsFor: 'as yet unclassified' stamp: 'jmv 8/23/2024 09:36:15'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling.
	Not even asked if text stylyng makes no sense for this model.
	True by default."

	selector = #acceptedContents ifTrue: [
		^hasUnsavedEdits ].

	^true! !


!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:32:36'!
allowsStylingFor: selector isTextDirty: hasUnsavedEdits
	"Answer true to allow styling to proceed, or false to veto the styling"

	^true! !


!Compiler class methodsFor: 'user interface support' stamp: 'jmv 8/23/2024 09:42:54'!
textStylerClass

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80! !


!PluggableTextModel methodsFor: 'shout styling' stamp: 'jmv 8/23/2024 09:33:29'!
stylesText
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType) and: [
		self textStylerClass notNil and: [
			textProvider allowsStylingFor: textGetter isTextDirty: self isDirty ]].! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 8/23/2024 09:42:35'!
textStylerClass

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80! !


!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 8/23/2024 09:42:11'!
textStylerClassFor: textGetter

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	textGetter = #acceptedContents ifFalse: [
		^super textStylerClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass textStylerClass ].
	^SHTextStylerST80! !


!Inspector methodsFor: 'user interface support' stamp: 'jmv 8/23/2024 09:42:18'!
textStylerClassFor: textGetter

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80! !


!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 8/23/2024 09:42:21'!
textStylerClassFor: textGetter

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/23/2024 09:43:12'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e := model editorClass new morph: self.
	e model: model.
	tc := TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor := e.
		textComposition := tc.
		tc
			setModel: model;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.
	self possiblyChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass: model textStylerClass.
	self autoCompleterClass: model autoCompleterClass.! !

!methodRemoval: ObjectExplorer #allowsStyling stamp: 'jmv 8/23/2024 09:59:19'!
ObjectExplorer removeSelector: #allowsStyling!
!methodRemoval: Debugger #allowsStyling stamp: 'jmv 8/23/2024 09:59:18'!
Debugger removeSelector: #allowsStyling!
!methodRemoval: ChangeSorter #allowsStyling stamp: 'jmv 8/23/2024 09:59:18'!
ChangeSorter removeSelector: #allowsStyling!
!methodRemoval: ChangeList #allowsStyling stamp: 'jmv 8/23/2024 09:59:18'!
ChangeList removeSelector: #allowsStyling!
!methodRemoval: MethodSet #allowsStyling stamp: 'jmv 8/23/2024 09:59:17'!
MethodSet removeSelector: #allowsStyling!
!methodRemoval: CodeFileBrowser #allowsStyling stamp: 'jmv 8/23/2024 09:59:17'!
CodeFileBrowser removeSelector: #allowsStyling!
!methodRemoval: Browser #allowsStyling stamp: 'jmv 8/23/2024 09:59:17'!
Browser removeSelector: #allowsStyling!
!methodRemoval: CodeProvider #allowsStyling stamp: 'jmv 8/23/2024 09:59:17'!
CodeProvider removeSelector: #allowsStyling!
!methodRemoval: TextProvider #allowsStyling stamp: 'jmv 8/23/2024 09:59:16'!
TextProvider removeSelector: #allowsStyling!
