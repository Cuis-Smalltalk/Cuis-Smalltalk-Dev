'From Cuis6.3 [latest update: #6362] on 13 May 2024 at 9:48:22 am'!

!Clipboard methodsFor: 'private' stamp: 'jmv 5/13/2024 09:45:34'!
retrieveIdOrStringFromOS
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
	contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
	(String isAscii: string) ifFalse: [
		ByteArray adoptInstance: string.
		string := UnicodeString fromUtf8Bytes: string ].

	^ string! !


!DropFilesEvent methodsFor: 'files' stamp: 'jmv 5/13/2024 09:47:29'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | string |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :utf8BytesInAStringInstance |
			"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
			contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
			string := utf8BytesInAStringInstance.
			(String isAscii: string) ifFalse: [
				ByteArray adoptInstance: string.
				string := UnicodeString fromUtf8Bytes: string ].
			aBlock value: string ]].! !

