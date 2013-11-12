'From Cuis 4.2 of 25 July 2013 [latest update: #1865] on 12 November 2013 at 9:00:34.214412 am'!

!Clipboard methodsFor: 'private' stamp: 'jmv 11/10/2013 09:56'!
extendedClipboardInterface
	"Answer a subinstance of ExtendedClipboardInterface, if present and operational.
	Clipboard default extendedClipboardInterface
	"
	| interface |
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		interface _ clipboardInterface current.
		interface isOperational ifTrue: [
			^ interface ]].
	"No operational extended clipboard."
	^nil! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/10/2013 10:01'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil _ self retrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents asString])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 11/10/2013 10:02'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times."
	contents _ (anObject isString or: [ anObject is: #Text])
		ifTrue: [ anObject withCuisLineEndings ]
		ifFalse: [ anObject copyForClipboard ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
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
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'private' stamp: 'jmv 11/10/2013 10:00'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| primitiveFormat |

	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].
		
	primitiveFormat _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard.
	We prefer nil"
	primitiveFormat isEmpty ifTrue: [ ^nil ].

	"The VM uses UTF-8 for clipboard"
	^(String fromUtf8: primitiveFormat hex: false trimLastNull: true) withCuisLineEndings! !

