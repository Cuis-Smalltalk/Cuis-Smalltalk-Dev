'From Cuis7.3 [latest update: #6906] on 9 December 2024 at 5:42:11 pm'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/9/2024 17:41:40'!
getCurrentWorkingDirectory
	"Answer the path from which Cuis was started.
	Smalltalk getCurrentWorkingDirectory
	(Note: Windows VM used to give a bogus answer. This was fixed in 2024.)
	"

	(self primGetCurrentWorkingDirectoryUnix ifNil: [ self primGetCurrentWorkingDirectoryWindows ]) ifNotNil:
		[ :utf8BytesInAStringInstance | | answer |
			"It seems on Mac, when dropping image on VM, we get '/', that is not really meaningful"
			(utf8BytesInAStringInstance = '/' and: [ self platformName = 'Mac OS' ]) ifFalse: [
				
				"Primitive answers an instance of String containing the UTF-8 bytes. If not pure ASCII,
				contents are invalid!! Convert it ASAP into a reasonable ByteArray + UnicodeString."
				answer := utf8BytesInAStringInstance.
				(String isAscii: answer) ifFalse: [
					ByteArray adoptInstance: answer.
					answer := UnicodeString fromUtf8Bytes: answer ].
				
				^ answer ]].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !

