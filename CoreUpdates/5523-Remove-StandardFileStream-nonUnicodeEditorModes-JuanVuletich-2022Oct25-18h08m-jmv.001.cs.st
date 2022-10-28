'From Cuis 6.0 [latest update: #5513] on 25 October 2022 at 6:33:26 pm'!
!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp '
	classVariableNames: 'UnicodeContentsHandling '
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #FileExistsException category: 'Exceptions-Kernel'!
FileStreamException subclass: #FileExistsException
	instanceVariableNames: 'fileClass '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!
!classDefinition: #FileIOAccessor category: 'System-File System'!
Object subclass: #FileIOAccessor
	instanceVariableNames: 'slash drives '
	classVariableNames: 'ConcreteFileStreamClass Default '
	poolDictionaries: ''
	category: 'System-File System'!

!LabelMorph commentStamp: 'jmv 9/22/2022 15:50:49' prior: 0!
LabelMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph or TextParagraphMorph.

Structure:
instance var    	Type              Description 
font 			Font 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!


!TextModel methodsFor: 'accessing' stamp: 'jmv 9/22/2022 15:38:37'!
basicActualContents: aTextOrString
	"Do not throw events."
	| prevContents |
	prevContents _ actualContents.
	actualContents _ aTextOrString asText asUtf8StringOrText.
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp _ nil ]! !


!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/25/2022 18:18:18'!
arrowStringForAssignment
	"Some examples to try.
		2190 - 	Leftwards Arrow
		21D0 - 	Leftwards Double Arrow
		21E0	 - Leftwards Dashed Arrow
		21E6 - Leftwards White Arrow
		21FD	 - Leftwards Open-Headed Arrow
		27F5 - Long Leftwards Arrow
		2B05	 - Leftwards Black Arrow	"
	^ `(UnicodeCodePoint codePoint: 16r2190) asString`! !

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/25/2022 18:18:44'!
arrowStringForReturn
	"Some examples to try.
		2191 - 	Upwards Arrow
		21D1 - 	Upwards Double Arrow
		21E1	 - Upwards Dashed Arrow
		21E7 - Upwards White Arrow
		2B06	 - Upwards Black Arrow	"
	^ `(UnicodeCodePoint codePoint: 16r2191) asString`! !


!FileDoesNotExistException methodsFor: 'exceptionDescription' stamp: 'jmv 9/22/2022 15:29:06'!
defaultAction
	"The default action taken if the exception is signaled."


	^ readOnly
		ifTrue: [ UniFileStream readOnlyFileDoesNotExistUserHandling: fileName ]
		ifFalse: [ UniFileStream fileDoesNotExistUserHandling: fileName ]! !


!FileExistsException methodsFor: 'exceptionDescription' stamp: 'jmv 9/22/2022 15:29:52'!
defaultAction
	"The default action taken if the exception is signaled."

	^ UniFileStream fileExistsUserHandling: fileName! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 9/22/2022 15:32:00'!
try: execBlock forFileNamed: fullName

	"If fail, return nil"

	^ (UniFileStream retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 9/22/2022 15:29:16'!
fileSize: fileEntry

	| pathName f size |
	pathName _ fileEntry pathName.
	"At least on Linux 64 Cog, opening a directory as a stream and asking #size answers some absurd number: 9223372036854775807"
	(self basicDirectoryExists: pathName) ifTrue: [^ nil ].
	f _ UniFileStream new open: pathName forWrite: false.
	f ifNil: [^ nil].
	size _ f size.
	f close.
	^ size! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 9/22/2022 15:31:03'!
privateNewFile: aFileEntry
	"Create a new file with the given full pathName."

	^aFileEntry exists
		ifTrue: [
			(FileExistsException fileName: aFileEntry pathName) signal]
		ifFalse: [
			self privateWriteableFile: aFileEntry ]! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 9/22/2022 15:31:48'!
privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	(self basicDirectoryExists: pathName) ifTrue: [
		"If it is a directory, the it is not a file, and the requested file does not exist."
		^ ((FileDoesNotExistException fileName: pathName) readOnly: true) signal ].
	^(UniFileStream new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 9/22/2022 15:31:54'!
privateWriteableFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName _ aFileEntry pathName.
	^ (UniFileStream new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ]! !

!methodRemoval: FileIOAccessor class #useUtf8Files stamp: 'jmv 10/25/2022 18:21:54'!
FileIOAccessor class removeSelector: #useUtf8Files!
!methodRemoval: FileIOAccessor class #concreteStreamClass: stamp: 'jmv 10/25/2022 18:22:17'!
FileIOAccessor class removeSelector: #concreteStreamClass:!
!methodRemoval: FileIOAccessor class #useExtendedAsciiFiles stamp: 'jmv 10/25/2022 18:20:43'!
FileIOAccessor class removeSelector: #useExtendedAsciiFiles!
!methodRemoval: FileIOAccessor #concreteStreamClass stamp: 'jmv 10/25/2022 18:29:02'!
FileIOAccessor removeSelector: #concreteStreamClass!
!methodRemoval: FileExistsException class #fileName:fileClass: stamp: 'jmv 10/25/2022 18:27:34'!
FileExistsException class removeSelector: #fileName:fileClass:!
!methodRemoval: FileExistsException #fileClass: stamp: 'jmv 10/25/2022 18:27:48'!
FileExistsException removeSelector: #fileClass:!
!methodRemoval: TextModel class #useUtf8Editors stamp: 'jmv 10/25/2022 18:30:51'!
TextModel class removeSelector: #useUtf8Editors!
!methodRemoval: TextModel class #useExtendedAsciiEditors stamp: 'jmv 10/25/2022 18:30:09'!
TextModel class removeSelector: #useExtendedAsciiEditors!
!methodRemoval: TextModel class #useNonConvertingEditors stamp: 'jmv 10/25/2022 18:30:31'!
TextModel class removeSelector: #useNonConvertingEditors!
!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #FileExistsException category: 'Exceptions-Kernel'!
FileStreamException subclass: #FileExistsException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!

!FileExistsException reorganize!
('exceptionDescription' defaultAction)
!


!FileExistsException class reorganize!
('as yet unclassified')
!

!classDefinition: #FileIOAccessor category: 'System-File System'!
Object subclass: #FileIOAccessor
	instanceVariableNames: 'slash drives'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-File System'!
!classRemoval: #FileStream stamp: 'jmv 10/25/2022 18:32:39'!
Smalltalk removeClassNamed: #FileStream!

!classRemoval: #StandardFileStream stamp: 'jmv 10/25/2022 18:32:32'!
Smalltalk removeClassNamed: #StandardFileStream!

