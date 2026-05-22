"
A superclass for text providing models. To be used together with a PluggableTextModel.
"
Class {
	#name : 'TextProvider',
	#superclass : 'ActiveModel',
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'contents' }
TextProvider >> acceptedContents [
	^ Text
		initialFont: (Preferences at: #standardCodeFont)
		stringOrText: self acceptedStringOrText
]

{ #category : 'contents' }
TextProvider >> acceptedContentsChanged [

	self changed: #acceptedContents
]

{ #category : 'contents' }
TextProvider >> acceptedStringOrText [
	^self subclassResponsibility
]

{ #category : 'shout styling' }
TextProvider >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling.
	Not even asked if text stylyng makes no sense for this model.
	True by default."

	^true
]

{ #category : 'user interface support' }
TextProvider >> autoCompleterClassFor: textGetter [
	^nil
]

{ #category : 'auto complete' }
TextProvider >> classOfThisContext [

	^ MethodContext 
]

{ #category : 'auto complete' }
TextProvider >> classOfWorkspaceVarNamed: aName [

	^ nil
]

{ #category : 'auto complete' }
TextProvider >> computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextProvider >> computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextProvider >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextProvider >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'accessing' }
TextProvider >> contentsSelection [
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^ nil  "null selection"
]

{ #category : 'accessing' }
TextProvider >> currentCompiledMethod [
	"Answer the CompiledMethod installed in the system forthe currently selected message.
	Answer nil if no such concept exists."
	^ nil
]

{ #category : 'user interface support' }
TextProvider >> editorClassFor: textGetter [
	^TextEditor
]

{ #category : 'testing' }
TextProvider >> isEditingExistingMethod [

	^self isEditingMethod
]

{ #category : 'testing' }
TextProvider >> isEditingMethod [

	^false
]

{ #category : 'accessing' }
TextProvider >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock

]

{ #category : 'accessing' }
TextProvider >> selectedClass [
	"Answer the class that is currently selected.
	Answer nil if no concept of selectedClass exists."
	^ nil
]

{ #category : 'accessing' }
TextProvider >> selectedClassOrMetaClass [
	"Answer the selected class or metaclass.
	Answer nil if no concept of selectedClassOrMetaClass exists."
	^ nil
]

{ #category : 'accessing' }
TextProvider >> selectedMessageName [
	"Answer the message selector of the currently selected message.
	Answer nil if no concept of selectedMessageName exists."
	^ nil
]

{ #category : 'user interface support' }
TextProvider >> shouldAutoHighlight [
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false
]

{ #category : 'shout styling' }
TextProvider >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true
]

{ #category : 'user interface support' }
TextProvider >> textStylerClassFor: textGetter [
	^nil
]
