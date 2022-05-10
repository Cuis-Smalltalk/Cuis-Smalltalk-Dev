'From Cuis 6.0 [latest update: #5143] on 10 May 2022 at 3:48:54 pm'!

!Clipboard methodsFor: 'private' stamp: 'jmv 5/10/2022 15:48:42'!
retrieveIdOrStringFromOS: stringClass
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	utf8Bytes _ utf8Bytes fixLineEndings.

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray."
	utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
		ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
	^ stringClass fromUtf8: utf8Bytes.! !

