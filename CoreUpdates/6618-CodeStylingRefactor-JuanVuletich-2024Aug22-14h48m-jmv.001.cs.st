'From Cuis7.1 [latest update: #6617] on 22 August 2024 at 2:54:54 pm'!

!TextModel methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:18:19'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."! !

!TextModel methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 12:25:44'!
stylesText
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType)
		and: [ self textStylerClass notNil ].! !


!PluggableTextModel methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:18:31'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	textProvider aboutToStyleWith: anSHTextStyler.! !

!PluggableTextModel methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 12:26:24'!
stylesText
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType) and: [
		self textStylerClass notNil and: [
			textProvider allowsStyling ]].! !


!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:18:36'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler 
		classOrMetaClass: nil;
		workspace: self.! !

!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 12:26:38'!
stylesText
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType) and: [
		self textStylerClass notNil and: [
			self isTextStylingEnabled ]].! !


!TextProvider methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:43'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling.
	Not even asked if text stylyng makes no sense for this model.
	True by default."

	^true! !


!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:49:58'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.! !

!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:46'!
allowsStyling
	"Answer false if showing difs, to veto the styling."

	^self showingAnyKindOfDiffs not! !


!Browser methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:57:53'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler classOrMetaClass:
		((#(editClass newClass) includes: self editSelection)  ifFalse:[
			self selectedClassOrMetaClass ]). "nil for class definitions"! !

!Browser methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:49'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	| type |
	self isModeStyleable ifFalse: [^false].
	type := self editSelection.
	^ #(newMessage editMessage editClass newClass) includes: type.! !


!CodeFileBrowser methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:52'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!MethodSet methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:55'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!ChangeList methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:46:57'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				^true ]]].
	^false! !


!ChangeSorter methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:47:00'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	^self currentSelector notNil! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:19:02'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler 
		classOrMetaClass: 
			(self selectedContext selector = Scanner doItSelector ifFalse: [
				self selectedClassOrMetaClass]); "nil for DoIts"
		workspace: self.! !

!Debugger methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:47:03'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable! !


!Inspector methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:19:05'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler workspace: self.! !


!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:19:52'!
aboutToStyleWith: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler workspace: self.! !

!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 11:47:06'!
allowsStyling
	"Answer true to allow styling to proceed, or false to veto the styling"

	^true! !


!Compiler class methodsFor: 'user interface support' stamp: 'jmv 8/22/2024 12:28:16'!
textStylerClass

	^SHTextStylerST80! !


!TextModel methodsFor: 'user interface support' stamp: 'jmv 8/22/2024 12:22:18'!
convertAndStyleIfNeededWith: anSHTextStyler

	self aboutToStyleWith: anSHTextStyler.
	self styleByParagraphs
		ifTrue: [ self convertAndStyleIByParagraphsWith: anSHTextStyler ]
		ifFalse: [ 	self convertAndStyleAllWith: anSHTextStyler ]	.! !

!TextModel methodsFor: 'user interface support' stamp: 'jmv 8/22/2024 12:27:57'!
textStylerClass

	^nil! !


!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 8/22/2024 12:27:48'!
textStylerClass

	^textProvider textStylerClassFor: textGetter! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 8/22/2024 12:28:08'!
textStylerClass

	^SHTextStylerST80! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/22/2024 12:23:55'!
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
	self stylerClass: (model stylesText ifTrue: [model textStylerClass]).
	self autoCompleterClass: model autoCompleterClass.! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 8/22/2024 12:22:08'!
convertAndStyleIfNeeded
	"Apply both converting (changes to the characters in the text, such as 	preferred assignment operators),
	and styling (TextAttributes to make Smalltalk code easier to understand)"

	| previousSize delta isAtEnd |
	previousSize := model actualContents size.
	isAtEnd := editor stopIndex > previousSize.
	model stylesText ifTrue: [
		model convertAndStyleIfNeededWith: styler ].
	delta := model actualContents size - previousSize.
	(isAtEnd and: [ delta < 0 ]) ifTrue: [
		delta := 0].
	delta = 0 ifFalse: [
		editor moveCursor: [ :p | p+delta] forward: delta>0 event: nil ].! !

!methodRemoval: ObjectExplorer #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:38'!
ObjectExplorer removeSelector: #shouldStyle:with:!
!methodRemoval: Inspector #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:38'!
Inspector removeSelector: #shouldStyle:with:!
!methodRemoval: Debugger #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:37'!
Debugger removeSelector: #shouldStyle:with:!
!methodRemoval: ChangeSorter #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:37'!
ChangeSorter removeSelector: #shouldStyle:with:!
!methodRemoval: ChangeList #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:37'!
ChangeList removeSelector: #shouldStyle:with:!
!methodRemoval: MethodSet #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:37'!
MethodSet removeSelector: #shouldStyle:with:!
!methodRemoval: CodeFileBrowser #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:36'!
CodeFileBrowser removeSelector: #shouldStyle:with:!
!methodRemoval: Browser #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:36'!
Browser removeSelector: #shouldStyle:with:!
!methodRemoval: CodeProvider #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:36'!
CodeProvider removeSelector: #shouldStyle:with:!
!methodRemoval: Workspace #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:36'!
Workspace removeSelector: #shouldStyle:with:!
!methodRemoval: PluggableTextModel #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:30'!
PluggableTextModel removeSelector: #shouldStyle:with:!
!methodRemoval: TextModel #shouldStyle:with: stamp: 'jmv 8/22/2024 14:51:35'!
TextModel removeSelector: #shouldStyle:with:!
