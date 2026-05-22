"
A TextModel whose contents are stored elsewhere (ivar textProvider)
"
Class {
	#name : 'PluggableTextModel',
	#superclass : 'TextModel',
	#instVars : [
		'textProvider',
		'textGetter',
		'textSetter',
		'selectionGetter'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'instance creation' }
PluggableTextModel class >> on: aTextProvider [
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."
	^self basicNew initWith: aTextProvider
]

{ #category : 'shout styling' }
PluggableTextModel >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	textProvider aboutToStyleWith: anSHTextStyler.
]

{ #category : 'commands' }
PluggableTextModel >> acceptContentsFrom: aTextModelMorph [
	textSetter ifNil: [ ^ true ].
	^ textSetter numArgs = 2
		ifTrue: [
			textProvider
				perform: textSetter
				with: actualContents
				with: aTextModelMorph ]
		ifFalse: [
			textProvider
				perform: textSetter
				with: actualContents ].
]

{ #category : 'accessing' }
PluggableTextModel >> actualContents [
	actualContents ifNil: [ self refetch ].
	^actualContents
]

{ #category : 'user interface support' }
PluggableTextModel >> autoCompleterClass [
	^textProvider autoCompleterClassFor: textGetter
]

{ #category : 'accessing' }
PluggableTextModel >> basicReplaceFrom: start to: stop with: replacement [
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	"If we style text (as usual for Smalltalk code), but text is huge, assume plain String is OK.
	This was needed to accomodate class definition with thousands of instance variables,
	meaning a class definition with a single word-wrapped line of several hundred thousand character.
	It makes no sense to shout that, and plain String is composed much faster."
	(self stylesText and: [actualContents size + replacement size > self textStylerClass maxSizeForStyling ]) ifTrue: [
		actualContents := actualContents asString copyReplaceFrom: start to: stop with: replacement asString.
		actualContents := actualContents asText.
		^self ].

	^super basicReplaceFrom: start to: stop with: replacement
]

{ #category : 'binding' }
PluggableTextModel >> bindingOf: aString [

	^textProvider bindingOf: aString
]

{ #category : 'auto complete' }
PluggableTextModel >> classOfThisContext [

	^ textProvider classOfThisContext 
]

{ #category : 'auto complete' }
PluggableTextModel >> classOfWorkspaceVarNamed: aName [

	^ textProvider classOfWorkspaceVarNamed: aName 
]

{ #category : 'auto complete' }
PluggableTextModel >> computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName   [

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName 
]

{ #category : 'auto complete' }
PluggableTextModel >> computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName   [

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName 
]

{ #category : 'auto complete' }
PluggableTextModel >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	textProvider computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName 
]

{ #category : 'auto complete' }
PluggableTextModel >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	textProvider computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
]

{ #category : 'user interface support' }
PluggableTextModel >> editorClass [
	^textProvider editorClassFor: textGetter
]

{ #category : 'misc' }
PluggableTextModel >> getSelection [
	"Answer the model's selection interval."

	^selectionGetter ifNotNil: [
		textProvider perform: selectionGetter ]
]

{ #category : 'initialization' }
PluggableTextModel >> initWith: aTextProvider [
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."

	textProvider := aTextProvider.
	undoRedoCommands := ReadWriteStream on: #().
	lastEditTimeStamp := nil
]

{ #category : 'testing' }
PluggableTextModel >> is: aSymbol [
	^ aSymbol == #hasTextProvider or: [ super is: aSymbol ]
]

{ #category : 'misc' }
PluggableTextModel >> refetch [
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ]]
		ifNotNil: [
			self actualContents: (Text
				initialFont: (Preferences at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched ]
]

{ #category : 'testing' }
PluggableTextModel >> refusesToAccept [
	^textProvider is: #refusingToAccept
]

{ #category : 'misc' }
PluggableTextModel >> releaseCachedState [
	textProvider releaseCachedState
]

{ #category : 'user interface support' }
PluggableTextModel >> shouldAutoHighlight [
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^textProvider shouldAutoHighlight
]

{ #category : 'shout styling' }
PluggableTextModel >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^textProvider styleByParagraphs
]

{ #category : 'shout styling' }
PluggableTextModel >> stylesText [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType) and: [
		self textStylerClass notNil and: [
			textProvider allowsStylingFor: textGetter isTextDirty: self isDirty ]].
]

{ #category : 'accessing' }
PluggableTextModel >> textGetter: symbol1 textSetter: symbol2 selectionGetter: symbol3 [
	textGetter := symbol1.
	textSetter := symbol2.
	selectionGetter := symbol3
]

{ #category : 'accessing' }
PluggableTextModel >> textProvider [
	^textProvider
]

{ #category : 'user interface support' }
PluggableTextModel >> textStylerClass [

	^textProvider textStylerClassFor: textGetter
]

{ #category : 'updating' }
PluggableTextModel >> update: aSymbol [
	"We are being notified of a change in our provider.
	Notify our dependents"
	"
	self changed: aSymbol
	"
	aSymbol == textGetter ifTrue: [ ^self changed: #acceptedContents ].
	aSymbol == selectionGetter ifTrue: [ ^self changed: #initialSelection ].
	self changed: aSymbol
]

{ #category : 'testing' }
PluggableTextModel >> wantsFrameAdornments [

	^textSetter notNil
]
