"
ObjectExplorer provides a hierarchical alternative to #inspect. Simply evaluate an expression like:

self runningWorld explore

and enjoy.
"
Class {
	#name : 'ObjectExplorer',
	#superclass : 'AbstractHierarchicalList',
	#instVars : [
		'rootObject',
		'monitorList'
	],
	#category : 'Tools-Explorer',
	#package : 'Tools',
	#tag : 'Explorer'
}

{ #category : 'shout styling' }
ObjectExplorer >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler workspace: self.
]

{ #category : 'shout styling' }
ObjectExplorer >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^true
]

{ #category : 'user interface support' }
ObjectExplorer >> autoCompleterClassFor: textGetter [
	^SmalltalkCompleter
]

{ #category : 'shout styling' }
ObjectExplorer >> bindingNamesDo: aBlock [
	self doItReceiver class allInstVarNames do: aBlock
]

{ #category : 'accessing' }
ObjectExplorer >> doItContext [
	"Answer the context in which a text selection can be evaluated."

	^nil
]

{ #category : 'accessing' }
ObjectExplorer >> doItReceiver [
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	currentSelection ifNil: [^rootObject].
	^currentSelection withoutListWrapper

]

{ #category : 'user interface support' }
ObjectExplorer >> editorClassFor: textGetter [
	^SmalltalkEditor
]

{ #category : 'accessing' }
ObjectExplorer >> getList [

	^Array with: (ObjectExplorerWrapper with: rootObject name: 'root' model: self parent: nil)

]

{ #category : 'shout styling' }
ObjectExplorer >> hasBindingOf: aString [
	^ self doItReceiver class allInstVarNames includes: aString
]

{ #category : 'user commands' }
ObjectExplorer >> inspectSelection [
	self object inspect
]

{ #category : 'testing' }
ObjectExplorer >> is: aSymbol [
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
]

{ #category : 'testing' }
ObjectExplorer >> isEditingExistingMethod [

	^self isEditingMethod
]

{ #category : 'testing' }
ObjectExplorer >> isEditingMethod [

	^false
]

{ #category : 'monitoring' }
ObjectExplorer >> isMonitoring [
	^monitorList isEmptyOrNil not
]

{ #category : 'user interface support' }
ObjectExplorer >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^aParsingErrorBlock value: nil
]

{ #category : 'monitoring' }
ObjectExplorer >> monitorList [
	^monitorList ifNil: [ monitorList := EphemeronIdentityDictionary new ].
]

{ #category : 'accessing' }
ObjectExplorer >> object [
	^currentSelection ifNotNil: [ :cs | cs withoutListWrapper ]
]

{ #category : 'accessing' }
ObjectExplorer >> parentObject [
	currentSelection ifNil: [ ^nil ].
	currentSelection parent ifNil: [ ^rootObject ].
	^currentSelection parent withoutListWrapper
]

{ #category : 'accessing' }
ObjectExplorer >> rootObject [
	^rootObject
]

{ #category : 'accessing' }
ObjectExplorer >> rootObject: anObject [
	rootObject := anObject
]

{ #category : 'menus' }
ObjectExplorer >> selectedClass [
	"Answer the class of the receiver's current selection"

	^self doItReceiver class

]

{ #category : 'accessing' }
ObjectExplorer >> selector [
	^currentSelection ifNotNil: [ :cs | cs selector ]
]

{ #category : 'user interface support' }
ObjectExplorer >> shouldAutoHighlight [
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false
]

{ #category : 'monitoring' }
ObjectExplorer >> stepAt: millisecondSinceLast [

	"If there's anything in my monitor list, see if the strings have changed."
	| changes |
	changes := false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [ | string |
			k refresh.
			(string := k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes := true ]]].
	changes ifTrue: [ | sel |
		sel := currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]
]

{ #category : 'monitoring' }
ObjectExplorer >> stopMonitoring [
	monitorList := nil
]

{ #category : 'shout styling' }
ObjectExplorer >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true
]

{ #category : 'user interface support' }
ObjectExplorer >> textStylerClassFor: textGetter [

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80
]
