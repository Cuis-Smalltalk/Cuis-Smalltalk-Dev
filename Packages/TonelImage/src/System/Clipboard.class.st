"
The Clipboard class implements a basic buffering scheme for text. The currently selected text is also exported to the OS so that text can be copied from and to other applications. Commonly only a single instance is used (the default clipboard) but applications are free to use other than the default clipboard if necessary.
"
Class {
	#name : 'Clipboard',
	#superclass : 'Object',
	#instVars : [
		'contents',
		'recent',
		'contentsOriginalObjectWeakly'
	],
	#classVars : [
		'Default'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
Clipboard class >> chooseRecentClipping  "Clipboard chooseRecentClipping" [
	"Choose by menu from among the recent clippings"
	^self default chooseRecentClipping
]

{ #category : 'default clipboard' }
Clipboard class >> contentsOriginalObject [
	"If not nil, the original object (not a copy!) of what was stored in the clipboard. See #storeObject: Use with care"

	^ self default contentsOriginalObject
]

{ #category : 'accessing' }
Clipboard class >> default [
	^Default ifNil:[Default := self new].
]

{ #category : 'accessing' }
Clipboard class >> default: aClipboard [
	"So that clients can switch between different default clipboards"
	Default := aClipboard.
]

{ #category : 'cached state access' }
Clipboard class >> releaseClassCachedState [
	Default := nil
]

{ #category : 'default clipboard' }
Clipboard class >> retrieveMorph [
	"Answer a Morph"
	^self default retrieveMorph
]

{ #category : 'default clipboard' }
Clipboard class >> retrieveObject [
	"Answer whatever was last stored in the clipboard"
	^self default retrieveObject
]

{ #category : 'default clipboard' }
Clipboard class >> retrieveStringOrText [
	"Answer a Text or a String. Appropriate for a Text Editor."
	^self default retrieveStringOrText
]

{ #category : 'default clipboard' }
Clipboard class >> storeObject: aTextOrStringOrFormOrObject [
	"Set text currently on the clipboard.  Also export to OS"
	^self default storeObject: aTextOrStringOrFormOrObject
]

{ #category : 'accessing' }
Clipboard >> chooseRecentClipping [
	"
	Clipboard chooseRecentClipping
	"
	"Choose by menu from among the recent clippings"
	recent isEmpty ifTrue: [ ^ nil ].
	^ (SelectionMenu
		labelList:
			(recent collect: [ :txt |
				((txt asString contractTo: 50)
					withLineEndings: '\') withBlanksCondensed ])
		selections: recent) startUpMenu
]

{ #category : 'accessing' }
Clipboard >> contentsOriginalObject [
	"If not nil, the original object (not a copy!) of what was stored in the clipboard. See #storeObject: Use with care"

	^ contentsOriginalObjectWeakly at: 1
]

{ #category : 'private' }
Clipboard >> extendedClipboardInterface [
	"Answer a subinstance of ExtendedClipboardInterface, if present and operational.
	Clipboard default extendedClipboardInterface
	"
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		| interface |
		interface := clipboardInterface current.
		interface isOperational ifTrue: [
			^ interface ]].
	"No operational extended clipboard."
	^nil
]

{ #category : 'private' }
Clipboard >> idFor: anObject [
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	^{anObject class. anObject hash} printString
]

{ #category : 'initialization' }
Clipboard >> initialize [
	contents := nil.
	contentsOriginalObjectWeakly := WeakArray new: 1.
	recent := OrderedCollection new.
]

{ #category : 'private' }
Clipboard >> noteRecentClipping: text [
	"Keep most recent clippings in a queue for pasteRecent (paste... command)"
	(text isString or: [text is: #Text]) ifFalse: [^self ].
	text isEmpty ifTrue: [^ self].
	text size > 50000 ifTrue: [^ self].
	(recent includes: text) ifTrue: [^ self].
	recent addFirst: text.
	[recent size > 5] whileTrue: [recent removeLast].

]

{ #category : 'primitives' }
Clipboard >> primitiveClipboardString [
	"Get the current clipboard string. Return the empty string if the primitive fails."
	<primitive: 141>
	^ ''
]

{ #category : 'primitives' }
Clipboard >> primitiveClipboardString: aString [
	"Set the current clipboard to the given string."

	<primitive: 141>
	"don't fail if the primitive is not implemented"
]

{ #category : 'private' }
Clipboard >> retrieveIdOrStringFromOS [
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| string |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	string := self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	string isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	string := string fixLineEndings.

	"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
	contents are invalid! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	(String isAscii: string) ifFalse: [
		ByteArray adoptInstance: string.
		string := UnicodeString fromUtf8Bytes: string ].

	^ string
]

{ #category : 'accessing' }
Clipboard >> retrieveMorph [
	"Answer a Morph or nil."
	| answer |
	
	answer := self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph newWith: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil.
]

{ #category : 'accessing' }
Clipboard >> retrieveObject [
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil := self retrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents or: [(contents is: #Text) and: [stringOrNil = contents string]]])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]
]

{ #category : 'accessing' }
Clipboard >> retrieveStringOrText [
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |

	answer := self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: (UnicodeString with: $*) attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ UnicodeString new ] ifNotNil: [ answer printString ]
]

{ #category : 'accessing' }
Clipboard >> storeObject: anObject [
	"Set new contents on the clipboard. Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present.
	Warning: This has not yet been adapted to UTF-8 Unicode"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id := self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat := self stringOrIdFor: contents.
	"See also CodeProvider class >> #normalizeCodeForSaving:in:	
	See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Workspace >> #contentsForFileSaving"
	primitiveFormat := primitiveFormat copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	primitiveFormat := primitiveFormat asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat.
]

{ #category : 'private' }
Clipboard >> stringOrIdFor: anObject [
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isString ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject
]
