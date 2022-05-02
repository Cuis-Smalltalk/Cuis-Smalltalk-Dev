'From Cuis 6.0 [latest update: #5121] on 29 April 2022 at 5:08:17 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 4/29/2022 11:22:00'!
isStringLike
	"Overridden to return true in String, and StringUtf8."
	^ false! !


!String methodsFor: 'testing' stamp: 'jmv 4/29/2022 11:22:03'!
isStringLike
	^ true! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 11:25:37'!
retrieveObject: stringClass
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil _ self retrieveIdOrStringFromOS: stringClass.
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
	^stringOrNil ifNil: [ contents copyForClipboard ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 11:35:19'!
retrieveStringOrText: stringClass
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject: stringClass.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isStringLike or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: (stringClass with: $*) attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ stringClass new ] ifNotNil: [ answer printString ]! !

!Clipboard methodsFor: 'private' stamp: 'jmv 4/29/2022 11:20:20'!
retrieveIdOrStringFromOS: stringClass
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes classForUtf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray or StringUtf8."
	classForUtf8Bytes _ stringClass == String ifTrue: [ ByteArray ] ifFalse: [ stringClass ].
	utf8Bytes class format = classForUtf8Bytes format  												"True unless image is in 32bit V3 format"
		ifTrue: [ classForUtf8Bytes adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes _ classForUtf8Bytes withAll: utf8Bytes ].
	^ (stringClass fromUtf8: utf8Bytes) fixLineEndings.! !


!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 4/29/2022 10:51:54'!
retrieveStringOrText: stringClass
	"Answer a Text or a String. Appropriate for a Text Editor."
	^self default retrieveStringOrText: stringClass! !


!String class methodsFor: 'instance creation' stamp: 'jmv 4/29/2022 11:09:47'!
fromUtf8: aByteArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm | | bytes codePoint |
		bytes _ aByteArray readStream.
		[ bytes atEnd ] whileFalse: [
			codePoint _ (Character nextUnicodeCodePointFromUtf8: bytes).
			codePoint ifNotNil: [
				"Add just one char if possible, or the apropriate sequence for the NCR"
				self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ]].
		(doTrimLastNullChar and: [ codePoint = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 4/29/2022 11:04:30'!
clipboardStringOrText

	| clipContents |
	clipContents _ Clipboard retrieveStringOrText: self privateCurrentString class.
	^ ((clipContents is: #Text) and: [ clipContents respondsTo: #asNonStyledText ])
		ifTrue: [ clipContents asNonStyledText ]
		ifFalse: [ clipContents ]! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 11:01:29'!
retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer _ self retrieveObject: String.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph new image: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 17:06:14'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	^self retrieveObject: String! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 17:07:21'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	^self retrieveStringOrText: String! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 4/29/2022 12:18:43'!
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
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isStringLike or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject withCuisLineEndings.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
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

!Clipboard methodsFor: 'private' stamp: 'jmv 4/29/2022 11:38:58'!
stringOrIdFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isStringLike ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject! !


!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 1/18/2011 23:40'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	^self default retrieveObject! !

!Clipboard class methodsFor: 'default clipboard' stamp: 'jmv 4/29/2022 10:52:56'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor.
	By default, use String class for Strings"

	^self retrieveStringOrText: String! !

!methodRemoval: Clipboard #retrieveIdOrStringFromOS stamp: 'jmv 4/29/2022 17:07:38'!
Clipboard removeSelector: #retrieveIdOrStringFromOS!
