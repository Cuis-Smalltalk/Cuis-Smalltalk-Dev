'From Cuis 6.0 [latest update: #5617] on 3 January 2023 at 3:31:34 pm'!

!Clipboard methodsFor: 'accessing' stamp: 'jmv 1/3/2023 14:34:52'!
storeObject: anObject
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
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present.
	Warning: This has not yet been adapted to UTF-8 Unicode"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
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
	primitiveFormat _ self stringOrIdFor: contents.
	primitiveFormat _ primitiveFormat copyReplaceAll: Character smalltalkLeftArrow asString with: '_'.
	primitiveFormat _ primitiveFormat copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	primitiveFormat _ primitiveFormat asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !

