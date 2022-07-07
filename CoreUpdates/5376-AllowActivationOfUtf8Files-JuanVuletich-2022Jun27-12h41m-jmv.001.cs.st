'From Cuis 6.0 [latest update: #5323] on 27 June 2022 at 1:02:10 pm'!
!classDefinition: #FileIOAccessor category: 'System-File System'!
Object subclass: #FileIOAccessor
	instanceVariableNames: 'slash drives '
	classVariableNames: 'Default ConcreteFileStreamClass '
	poolDictionaries: ''
	category: 'System-File System'!

!TextModel methodsFor: 'as yet unclassified' stamp: 'jmv 6/27/2022 12:45:17'!
saveOn: stream
	"Saves the model to the given stream"
	stream binary.
	stream nextPutAll: self actualContents asPlainString.! !


!FileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 6/27/2022 12:54:23'!
concreteStreamClass: aFileStreamClass
	"
	FileIOAccessor concreteStreamClass: StandardFileStream
	FileIOAccessor concreteStreamClass: UniFileStream
	"
	ConcreteFileStreamClass _ aFileStreamClass.
	Smalltalk closeSourceFiles; openSourceFiles.! !

!FileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 6/27/2022 12:59:47'!
useExtendedAsciiFiles
	"
	Use Extended Ascii (ISO 8859-15 plus a few Greek and Math symbols) for files.
	Only support instances of String.
	This is the traditional, pre-Unicode way for Cuis.

	FileIOAccessor useExtendedAsciiFiles
	"
	FileIOAccessor concreteStreamClass: StandardFileStream! !

!FileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 6/27/2022 13:00:27'!
useUtf8Files
	"
	Use UTF-8 encoded Unicode files.
	Supports both regular String and Unicode Utf8String.
	The use of multi-byte Unicode:
		- Will save files that require an Unicode enabled Cuis to be read.
		- For tools to work properly, the use of a TrueType fonts is required.

	FileIOAccessor useUtf8Files
	"
	FileIOAccessor concreteStreamClass: UniFileStream! !


!FileDoesNotExistException methodsFor: 'exceptionDescription' stamp: 'jmv 6/27/2022 12:41:31'!
defaultAction
	"The default action taken if the exception is signaled."


	^ readOnly
		ifTrue: [ FileIOAccessor default concreteStreamClass readOnlyFileDoesNotExistUserHandling: fileName ]
		ifFalse: [ FileIOAccessor default concreteStreamClass  fileDoesNotExistUserHandling: fileName ]! !


!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 6/27/2022 12:51:19'!
concreteStreamClass
	"
	FileIOAccessor default concreteStreamClass
	"
	ConcreteFileStreamClass ifNil: [
		ConcreteFileStreamClass _ Smalltalk
			at: #StandardFileStream
			ifAbsent: [ Smalltalk at: #UniFileStream ]].
	^ConcreteFileStreamClass! !


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 6/27/2022 12:45:31'!
saveContentsTo: fileName
	"Saves the contents to the given filename"
	self hasSaveAs ifFalse: [^self].
	fileName asFileEntry writeStreamDo: [ :stream |
		model saveOn: stream ].
	self notifyUserWith: 'Contents saved'.! !

!methodRemoval: FileIOAccessor #concreteStreamClass: stamp: 'jmv 6/27/2022 12:54:41'!
FileIOAccessor removeSelector: #concreteStreamClass:!
!methodRemoval: FileStream class #concreteStream stamp: 'jmv 6/27/2022 12:49:48'!
FileStream class removeSelector: #concreteStream!
!methodRemoval: TextModel #saveOn:as: stamp: 'jmv 6/27/2022 12:45:36'!
TextModel removeSelector: #saveOn:as:!
!classDefinition: #FileIOAccessor category: 'System-File System'!
Object subclass: #FileIOAccessor
	instanceVariableNames: 'slash drives'
	classVariableNames: 'ConcreteFileStreamClass Default'
	poolDictionaries: ''
	category: 'System-File System'!
