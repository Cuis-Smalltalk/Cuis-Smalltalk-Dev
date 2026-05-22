"
A Workspace is a text area plus a lot of support for executable code.  It is a great place to execute top-level commands to compute something useful, and it is a great place to develop bits of a program before those bits get put into class methods.

To open a new workspace, execute:

	Workspace openLabel: 'a workspace'


A workspace can have its own variables, called "workspace variables", to hold intermediate results.  For example, if you type into a workspace "x := 5" and do-it, then later you could type in "y := x * 2" and y would become 10.

Additionally, in Morphic, a workspace can gain access to morphs that are on the screen.  If acceptDroppedMorphss is turned on, then whenever a morph is dropped on the workspace, a variable will be created which references that morph.  This functionality is toggled with the window-wide menu of a workspace.
"
Class {
	#name : 'Workspace',
	#superclass : 'TextModel',
	#instVars : [
		'bindings',
		'mustDeclareVariables',
		'shouldStyle',
		'fullPrintIt'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'ui' }
Workspace class >> open [
	^self new
		contents: '';
		openLabel: 'Workspace'
]

{ #category : 'window color' }
Workspace class >> windowColor [
	^ Theme current workspace
]

{ #category : 'menu-world' }
Workspace class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		20.
			#label 			-> 		'Workspace'.
			#object 		-> 		Workspace.
			#selector 		-> 		#open.
			#icon 			-> 		#terminalIcon.
			#balloonText 	-> 		'A window for evaluating Smalltalk expressions'.
		} asDictionary}`
]

{ #category : 'shout styling' }
Workspace >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler 
		classOrMetaClass: nil;
		workspace: self.
]

{ #category : 'user interface support' }
Workspace >> allowStylingWithEmphasis [
	"Disabled by default for faster styling of large contents, as text metrics are not affected by styling."

	^ Preferences at: #stylingWithEmphasisInWorkspaces
]

{ #category : 'user interface support' }
Workspace >> autoCompleterClass [
	^SmalltalkCompleter
]

{ #category : 'binding' }
Workspace >> bindingNamesDo: aBlock [

	bindings keysDo: aBlock
]

{ #category : 'binding' }
Workspace >> bindingOf: aString [
	mustDeclareVariables ifTrue: [^ nil].
	(bindings includesKey: aString) ifFalse: [
		aString first isUppercase
			ifTrue: [^nil]
			ifFalse: [bindings at: aString put: nil]].
	^bindings associationAt: aString
]

{ #category : 'testing' }
Workspace >> canBindVariables [
	^ true
]

{ #category : 'auto complete' }
Workspace >> classOfBindingOf: aName [
												
	^ (self bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] 
]

{ #category : 'auto complete' }
Workspace >> classOfWorkspaceVarNamed: aName [
												
	^ self classOfBindingOf: aName 
]

{ #category : 'auto complete' }
Workspace >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForClassOrNil: (self classOfBindingOf: aName) 
]

{ #category : 'accessing' }
Workspace >> contentsForFileSaving [
	"See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Clipboard >> #storeObject:
	See also CodeProvider class >> #normalizeCodeForSaving:in:	"
	| normalized |
	normalized := self actualContents asPlainString.
	normalized := normalized copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	normalized := normalized copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	normalized := normalized copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	^normalized.
]

{ #category : 'user interface support' }
Workspace >> editorClass [
	^SmalltalkEditor
]

{ #category : 'user interface support' }
Workspace >> fullPrintIt [

	^fullPrintIt ifNil: [ Preferences at: #fullPrintItInWorkspaces]
]

{ #category : 'binding' }
Workspace >> hasBindingOf: aString [
	^bindings includesKey: aString
]

{ #category : 'binding' }
Workspace >> hasBindingThatBeginsWith: aString  [

	bindings keysDo: [ :each |
		(each beginsWith: aString) ifTrue: [ ^true ] ].
	^false
]

{ #category : 'initialization' }
Workspace >> initialize [
	
	super initialize.
	self initializeBindings.
	mustDeclareVariables := false.
	fullPrintIt := false.
]

{ #category : 'binding' }
Workspace >> initializeBindings [
	
	bindings := Dictionary new.
	self changed: #actualContents.

]

{ #category : 'testing' }
Workspace >> is: aSymbol [
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
]

{ #category : 'shout styling' }
Workspace >> isTextStylingEnabled [

	^shouldStyle ifNil: [ Preferences at: #shoutInWorkspaces]
]

{ #category : 'accessing' }
Workspace >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock

]

{ #category : 'variable declarations' }
Workspace >> mustDeclareVariableWording [
	
	^ mustDeclareVariables not
		ifTrue: ['<yes> automatically create variable declaration']
		ifFalse: ['<no> automatically create variable declaration']
]

{ #category : 'variable declarations' }
Workspace >> nameForObject: object [
	"Answer a name suitable for a Workspace variable"
	^ (object class name, object identityHash printString) asIdentifier: false
]

{ #category : 'gui' }
Workspace >> openLabel: aString  [
	"Create a standard system view of the model, me, and open it."
	| window |
	window := WorkspaceWindow editText: self label: aString wrap: true.
	self changed: #actualContents.
	^window
]

{ #category : 'shout styling' }
Workspace >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true
]

{ #category : 'shout styling' }
Workspace >> stylesText [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType) and: [
		self textStylerClass notNil and: [
			self isTextStylingEnabled ]].
]

{ #category : 'user interface support' }
Workspace >> textStylerClass [

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80
]

{ #category : 'user interface support' }
Workspace >> toggleFullPrintIt [

	fullPrintIt := self fullPrintIt not.
]

{ #category : 'user interface support' }
Workspace >> toggleFullPrintItLabel [

	^self fullPrintIt 
		ifTrue: [ '<on> full printIt' ]
		ifFalse: [ '<off> full printIt' ]
]

{ #category : 'shout styling' }
Workspace >> toggleStyling [

	shouldStyle := self isTextStylingEnabled not.
	actualContents := actualContents asPlainString asText.
	self changed: #actualContents
]

{ #category : 'shout styling' }
Workspace >> toggleStylingLabel [

	^self isTextStylingEnabled 
		ifTrue: [ '<on> syntax highlighting' ]
		ifFalse: [ '<off> syntax highlighting' ]
]

{ #category : 'variable declarations' }
Workspace >> toggleVariableDeclarationMode [

	mustDeclareVariables := mustDeclareVariables not
]
