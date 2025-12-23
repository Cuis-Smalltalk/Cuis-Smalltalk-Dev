'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:34:07 pm'!
'Description '!
!provides: 'ExtendedClipboard' 1 18!
!requires: 'FFI' 1 nil nil!
!requires: 'Graphics-Files-Additional' 1 nil nil!
SystemOrganization addCategory: #ExtendedClipboard!
SystemOrganization addCategory: #'ExtendedClipboard-Win32'!


!classDefinition: #ExtendedClipboardInterface category: #ExtendedClipboard!
Object subclass: #ExtendedClipboardInterface
	instanceVariableNames: 'clipboard'
	classVariableNames: 'Current WinClipboardTypes'
	poolDictionaries: ''
	category: 'ExtendedClipboard'!
!classDefinition: 'ExtendedClipboardInterface class' category: #ExtendedClipboard!
ExtendedClipboardInterface class
	instanceVariableNames: ''!

!classDefinition: #ExtendedClipboardMacInterface category: #ExtendedClipboard!
ExtendedClipboardInterface subclass: #ExtendedClipboardMacInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExtendedClipboard'!
!classDefinition: 'ExtendedClipboardMacInterface class' category: #ExtendedClipboard!
ExtendedClipboardMacInterface class
	instanceVariableNames: ''!

!classDefinition: #ExtendedClipboardUnixInterface category: #ExtendedClipboard!
ExtendedClipboardInterface subclass: #ExtendedClipboardUnixInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExtendedClipboard'!
!classDefinition: 'ExtendedClipboardUnixInterface class' category: #ExtendedClipboard!
ExtendedClipboardUnixInterface class
	instanceVariableNames: ''!

!classDefinition: #ExtendedClipboardWinInterface category: #ExtendedClipboard!
ExtendedClipboardInterface subclass: #ExtendedClipboardWinInterface
	instanceVariableNames: 'lastSeenSequenceNr'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExtendedClipboard'!
!classDefinition: 'ExtendedClipboardWinInterface class' category: #ExtendedClipboard!
ExtendedClipboardWinInterface class
	instanceVariableNames: ''!

!classDefinition: #Win32Bitmap category: #'ExtendedClipboard-Win32'!
ExternalStructure subclass: #Win32Bitmap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ExtendedClipboard-Win32'!
!classDefinition: 'Win32Bitmap class' category: #'ExtendedClipboard-Win32'!
Win32Bitmap class
	instanceVariableNames: ''!


!ExtendedClipboardInterface methodsFor: 'private' stamp: 'jmv 6/Jan/2023 14:12:07'!
addClipboardData: data dataFormat: aFormat
	clipboard = 0 ifTrue: [
		"Should never happen!!"
		self error: 'broken clipboard'.
		^self].
	self primAddClipboardData: clipboard data: data dataFormat: aFormat asUtf8BytesOrByteString.! !

!ExtendedClipboardInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 14:47:00'!
clearClipboard
	clipboard = 0 ifTrue: [^self].
	^ self primClearClipboard: clipboard! !

!ExtendedClipboardInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 14:47:00'!
createClipboard
	clipboard = 0 ifTrue: [^self].
	^ self primCreateClipboard! !

!ExtendedClipboardInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 14:47:00'!
getClipboardFormat: formatNumber
	clipboard = 0 ifTrue: [^nil].
	^ self primGetClipboardFormat: clipboard formatNumber: formatNumber! !

!ExtendedClipboardInterface methodsFor: 'private' stamp: 'jmv 6/Jan/2023 14:12:42'!
readClipboardData: format
	^clipboard = 0 ifFalse: [
		self primReadClipboardData: clipboard format: format  asUtf8BytesOrByteString ]! !

!ExtendedClipboardInterface methodsFor: 'testing' stamp: 'jmv 12/Nov/2013 08:50:00'!
canImportRTF
	^Smalltalk includesKey: #RTFTextBuilder! !

!ExtendedClipboardInterface methodsFor: 'testing' stamp: 'jmv 18/Jan/2011 23:44:00'!
canStore
	"Store into clipboard implemented on most platforms.
	Answer false in subclasses that can't store into clipboard."
	^clipboard ~= 0! !

!ExtendedClipboardInterface methodsFor: 'testing' stamp: 'jmv 10/Nov/2013 09:46:00'!
isOperational
	"Can be redefined if needed.
	We should check that ClipboardExtendedPlugin is available and operational."
	^clipboard ~= 0! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 21/Dec/2013 09:30:00'!
fromBMPBytes: bytes
	^ (BMPReadWriter onBinaryStream: bytes readStream) nextImage! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 29/Jan/2024 15:38:42'!
fromISO8859Bytes: bytes

	^(String streamContents: [ :strm | bytes do: [ :byte | strm nextPut: (Character codePoint: byte) ]])
		withCuisLineEndings! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 21/Dec/2013 09:30:00'!
fromJPEGBytes: bytes
	^ (JPEGReadWriter2 onBinaryStream: bytes readStream) nextImage! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 21/Dec/2013 09:31:00'!
fromPNGBytes: bytes
	^ (PNGReadWriter onBinaryStream: bytes readStream) nextImage! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 17/Mar/2011 09:23:00'!
fromRefStreamBytes: bytes
	^ ReferenceStream unStream: bytes! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 26/Jan/2011 10:24:00'!
fromTIFFBytes: bytes
	^ (TIFFReadWriter on: bytes readStream) nextImage! !

!ExtendedClipboardInterface methodsFor: 'object from raw data' stamp: 'jmv 4/Jan/2023 18:25:24'!
fromUTF8Bytes: utf8Bytes

	^(UnicodeString fromUtf8Bytes: utf8Bytes) withCuisLineEndings! !

!ExtendedClipboardInterface methodsFor: 'initialization' stamp: 'tak 16/May/2007 12:03:00'!
initialize
	clipboard := [self createClipboard] on: Error do: [:ex | clipboard := 0]! !

!ExtendedClipboardInterface methodsFor: 'system primitives' stamp: 'JSM 6/May/2006 16:32:00'!
primAddClipboardData: aClipboard data: data dataFormat: aFormat

	<primitive:'ioAddClipboardData' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.! !

!ExtendedClipboardInterface methodsFor: 'system primitives' stamp: 'JSM 6/May/2006 16:32:00'!
primClearClipboard: aClipboard

	<primitive:'ioClearClipboard' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.
! !

!ExtendedClipboardInterface methodsFor: 'system primitives' stamp: 'JSM 6/May/2006 18:53:00'!
primCreateClipboard
	<primitive:'ioCreateClipboard' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.
! !

!ExtendedClipboardInterface methodsFor: 'system primitives' stamp: 'JSM 6/May/2006 16:33:00'!
primGetClipboardFormat: aClipboard formatNumber: formatNumber

	<primitive:'ioGetClipboardFormat' module: 'ClipboardExtendedPlugin'>
	^ self primitiveFailed.
! !

!ExtendedClipboardInterface methodsFor: 'system primitives' stamp: 'JSM 6/May/2006 16:33:00'!
primReadClipboardData: aClipboard format: format

	<primitive:'ioReadClipboardData' module: 'ClipboardExtendedPlugin'>.
	^ self primitiveFailed.
! !

!ExtendedClipboardInterface methodsFor: 'api - retrieve' stamp: 'jmv 19/Jan/2011 15:17:00'!
retrieveId
	^self subclassResponsibility! !

!ExtendedClipboardInterface methodsFor: 'api - retrieve' stamp: 'jmv 26/Jan/2011 10:09:00'!
retrieveObject
	^self subclassResponsibility! !

!ExtendedClipboardInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeForm: aForm id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	^self subclassResponsibility! !

!ExtendedClipboardInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeString: aString id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	^self subclassResponsibility! !

!ExtendedClipboardInterface methodsFor: 'api - store' stamp: 'jmv 17/Mar/2011 09:35:00'!
storeText: aText id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id is usually at the end, but don't include it if we include a streamed representation."

	^self subclassResponsibility! !

!ExtendedClipboardInterface class methodsFor: 'system startup' stamp: 'JSM 6/May/2006 15:33:00'!
shutDown: quitting
! !

!ExtendedClipboardInterface class methodsFor: 'system startup' stamp: 'JSM 6/May/2006 15:33:00'!
startUp: resuming
	"The image is either being newly started (resuming is true), or it's just been snapshotted"

	Current := nil.
	self current.! !

!ExtendedClipboardInterface class methodsFor: 'accessing' stamp: 'jmv 11/Jan/2011 13:59:00'!
current
	| platform |
	Current ifNil: [
		platform _ Smalltalk platformName.
		Current _ platform = 'unix'
			ifTrue: [ ExtendedClipboardUnixInterface new ]
			ifFalse: [
				platform = 'Win32'
					ifTrue: [ ExtendedClipboardWinInterface new ]
					ifFalse: [ ExtendedClipboardMacInterface new ]]].
	^ Current.! !

!ExtendedClipboardInterface class methodsFor: 'class initialization' stamp: 'mir 10/May/2006 19:12:00'!
initialize
	"ExtendedClipboardInterface initialize"
	Current := nil.
	Smalltalk
		addToStartUpList: self after: nil;
		addToShutDownList: self after: nil.! !

!ExtendedClipboardMacInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 15:56:00'!
readPrimAvailableFormats
	| i availableFormats formatData |
	availableFormats _ OrderedCollection new.
	i _ 1.
	[
		formatData _ self getClipboardFormat: i.
		formatData notNil ] whileTrue: [
			availableFormats add: formatData.
			i _ i + 1 ].
	^ availableFormats! !

!ExtendedClipboardMacInterface methodsFor: 'api - retrieve' stamp: 'jmv 19/Jan/2011 14:55:00'!
retrieveId
	| bytes formats |
	formats _ self readPrimAvailableFormats.
	(formats includes: 'cuis-id')
		ifTrue: [
			bytes _ self readClipboardData: 'cuis-id'.
			^ bytes asString ].
	^ nil! !

!ExtendedClipboardMacInterface methodsFor: 'api - retrieve' stamp: 'jmv 6/Jan/2023 14:15:22'!
retrieveObject
	| conversion bytes |
	self readPrimAvailableFormats do: [ :format |
		conversion _ format caseOf: {
			"Unformatted plain text"
			[ 'public.utf8-plain-text' ] -> [ #fromUTF8Bytes: ].
			"Bitmap images (not sure if all of them are actually used in this platform)"
			[ 'public.png' ] -> [ #fromPNGBytes: ].
			[ 'public.tiff' ] -> [ #fromTIFFBytes: ].
			[ 'public.jpeg' ] -> [ #fromJPEGBytes: ].
			[ 'public.bmp' ] -> [ #fromBMPBytes: ].
			[ 'cuis-refStreamed' ] -> [ #fromRefStreamBytes: ].
			"Rich text"
			[ 'public.rtf' ] -> [ self canImportRTF ifTrue: [#fromRTFBytes:] ]	"Only if package RTFImporting is loaded"
		} otherwise: [ nil ].
		conversion ifNotNil: [
			bytes _ self readClipboardData: format.
			^self perform: conversion with: bytes ]
		].
	^nil	"Could not read any of the available formats"! !

!ExtendedClipboardMacInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeForm: aForm id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	self clearClipboard.
	self addClipboardData: (PNGReadWriter bytesFor: aForm) dataFormat: 'public.png'.
	"
	Could be done in addition...
	self addClipboardData: (JPEGReadWriter2 bytesFor: aForm) dataFormat: 'public.jpeg'
	"
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardMacInterface methodsFor: 'api - store' stamp: 'jmv 26/May/2022 12:08:29'!
storeString: aString id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	self clearClipboard.
	self
		addClipboardData: aString asUtf8Bytes
		dataFormat: 'public.utf8-plain-text'.
	"Could be done in addition (maybe with cr line ending?)
	self
		addClipboardData: (aString asByteArray collect: [ :c | Character latin1ToMacRoman: c ])
		dataFormat: 'com.apple.traditional-mac-plain-text'.
		"
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardMacInterface methodsFor: 'api - store' stamp: 'bp 17/Sep/2023 19:44:06'!
storeText: aText id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id is usually at the end, but don't include it if we include a streamed representation."
	self clearClipboard.

	"SmartRefStream handles class changes, but a little testing shows stuff is twice as big, and takes 20 times more time to generate"
	self addClipboardData: (ReferenceStream streamedRepresentationOf: aText) dataFormat: 'cuis-refStreamed'.

	"RTF format
	#rtfString might not be loaded (Package RTFExporting)"
	[ self addClipboardData: aText rtfString asUtf8Bytes dataFormat: 'public.rtf' ] on: MessageNotUnderstood do: [].

	"Just the string"
	self
		addClipboardData: aText asString asUtf8Bytes
		dataFormat: 'public.utf8-plain-text'.

	"If we store a streamed representation, don't store the id. In these cases, we prefer rebuilding the object, regardless of
	whether it is present in memory (because we stored it in the clipboard) or not (because some other Cuis image did it)"
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardUnixInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 15:56:00'!
readPrimAvailableFormats
	| i availableFormats formatData |
	availableFormats _ OrderedCollection new.
	i _ 1.
	[
		formatData _ self getClipboardFormat: i.
		formatData notNil ] whileTrue: [
			availableFormats add: formatData.
			i _ i + 1 ].
	^ availableFormats! !

!ExtendedClipboardUnixInterface methodsFor: 'api - retrieve' stamp: 'jmv 19/Jan/2011 15:17:00'!
retrieveId
	| bytes formats |
	formats _ self readPrimAvailableFormats.
	(formats includes: 'cuis-id')
		ifTrue: [
			bytes _ self readClipboardData: 'cuis-id'.
			^ bytes asString ].
	^ nil! !

!ExtendedClipboardUnixInterface methodsFor: 'api - retrieve' stamp: 'jmv 17/Mar/2011 09:08:00'!
retrieveObject
	| conversion bytes |
	self readPrimAvailableFormats do: [ :format |
		conversion _ format caseOf: {
			"Unformatted plain text"
			[ 'UTF8_STRING' ] -> [ #fromUTF8Bytes: ].
			"Bitmap images (not sure if all of them are actually used in this platform)"
			[ 'image/png' ] -> [ #fromPNGBytes: ].
			[ 'image/tiff' ] -> [ #fromTIFFBytes: ].
			[ 'image/jpeg' ] -> [ #fromJPEGBytes: ].
			[ 'image/bmp' ] -> [ #fromBMPBytes: ].
			[ 'cuis-refStreamed' ] -> [ #fromRefStreamBytes: ].
			"Rich text"
"			[ '????rtf' ] -> [ #fromRTFBytes: ]"
		} otherwise: [ nil ].
		conversion ifNotNil: [
			bytes _ self readClipboardData: format.
			^self perform: conversion with: bytes ]
		].
	^nil	"Could not read any of the available formats"! !

!ExtendedClipboardUnixInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeForm: aForm id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	self clearClipboard.
	self addClipboardData: (PNGReadWriter bytesFor: aForm) dataFormat: 'image/png'.
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardUnixInterface methodsFor: 'api - store' stamp: 'jmv 26/May/2022 12:08:36'!
storeString: aString id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	self clearClipboard.
	self
		addClipboardData: aString asUtf8Bytes
		dataFormat: 'UTF8_STRING'.
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardUnixInterface methodsFor: 'api - store' stamp: 'jmv 26/May/2022 12:08:43'!
storeText: aText id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id is usually at the end, but don't include it if we include a streamed representation."
	self clearClipboard.

	"SmartRefStream handles class changes, but a little testing shows stuff is twice as big, and takes 20 times more time to generate"
	self addClipboardData: (ReferenceStream streamedRepresentationOf: aText) dataFormat: 'cuis-refStreamed'.

	"onvert text to RTF and implement"
	"
	self
		addClipboardData: aText rtfString
		dataFormat: '????'
	"
	self
		addClipboardData: aText asString asUtf8Bytes
		dataFormat: 'UTF8_STRING'.

	"If we store a streamed representation, don't store the id. In these cases, we prefer rebuilding the object, regardless of
	whether it is present in memory (because we stored it in the clipboard) or not (because some other Cuis image did it)"
	self addClipboardData: otherString dataFormat: 'cuis-id'! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 27/Jul/2015 15:44:00'!
addClipboardData: data dataFormat: aFormat
	self canStore ifFalse: [
		self error: 'Writing to Extended Clipboard not implemented in Windows' ]! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 27/Jul/2015 15:45:00'!
clearClipboard
	self canStore ifFalse: [
		self error: 'Writing to Extended Clipboard not implemented in Windows' ]! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 27/Jul/2015 15:46:00'!
createClipboard
	"Not on Windows"
	clipboard := 0! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'mir 23/Jan/2008 12:11:00'!
lastSeenSequenceNr
	^lastSeenSequenceNr ifNil: [lastSeenSequenceNr := -1].! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 7/Mar/2018 18:52:53'!
readClipboardData: format
	| answer |
	self openClipboard.
	[
		answer _ (self getClipboardData: format) "fromCString" fromCStringAsByteArray
	] ensure: [ self closeClipboard ].
	^ answer! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 15:56:00'!
readPrimAvailableFormats
	| i availableFormats |
	availableFormats _ OrderedCollection new.
	i _ 0.
	self openClipboard.
	[
		i _ self format: i.
		i ~= 0 ] whileTrue: [
			availableFormats add: i ].
	self closeClipboard.
	^ availableFormats! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 16:05:00'!
retrieveWindowsBMPForm
	| handle bitmap result form bitBuffer byteCount |
	self openClipboard.
	[handle := self getClipboardDataHandle: 2 " 'image/*' CF_BITMAP"]
		ensure: [self closeClipboard].

	"Check handle"
	"Allocate buffer for bitmap structure"
	bitmap := Win32Bitmap new.
	result := self getObject: handle bufferSize: Win32Bitmap byteSize buffer: bitmap.
	byteCount := bitmap bmWidth * bitmap bmHeight * bitmap bmBitsPixel / 8.
	bitBuffer := ByteArray new: byteCount.
	result := self getBitmapBitsFrom: handle count: byteCount buffer: bitBuffer.

	result = 0
		ifTrue: [
			^nil].
	form := Form fromWin32BitMap: bitmap bits: bitBuffer.
	^form! !

!ExtendedClipboardWinInterface methodsFor: 'private' stamp: 'jmv 18/Jan/2011 16:05:00'!
retrieveWindowsDIBForm
	| handle bitmap result form bitBuffer |
	self openClipboard.
	[handle := self getClipboardDataHandle: 8 "CF_DIB"]
		ensure: [self closeClipboard].

	"Check handle"
	"Allocate buffer for bitmap structure"
"	result := self getObject: handle bufferSize: Win32BitmapInfoHeader byteSize buffer: nil."
self halt.
	bitmap := Win32Bitmap new.
	result := self getObject: handle bufferSize: Win32Bitmap byteSize buffer: bitmap.
	bitBuffer := ExternalAddress allocate: bitmap bmWidth*bitmap bmHeight*bitmap bmBitsPixel.
	bitmap bmBits: (ExternalData fromHandle: bitBuffer type: ExternalType byte asPointerType).
	result := self getObject: handle bufferSize: Win32Bitmap byteSize buffer: bitmap.

	result = 0
		ifTrue: [
			bitBuffer free.
			self halt.
			^nil].
	form := Form fromWin32BitMap: bitmap.
	bitBuffer free.
	^form! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'jmv 10/Nov/2013 09:55:00'!
attemptCloseClipboard
	"ExtendedClipboardWinInterface new closeClipboard"
	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/getclipboardformatname.asp

BOOL CloseClipboard(      

    VOID 
);"
	<apicall: long 'CloseClipboard' () module: 'user32.dll'>! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 14/Feb/2008 23:56:00'!
clipboardAvailable: format
	"ExtendedClipboardWinInterface new clipboardAvailable: 8"

	"http://msdn2.microsoft.com/en-us/library/ms649047(VS.85).aspx

BOOL IsClipboardFormatAvailable(      
    UINT format
);"
	<apicall: bool 'IsClipboardFormatAvailable' (ulong) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 10/May/2006 17:56:00'!
closeClipboard
	"ExtendedClipboardWinInterface new closeClipboard"
	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/getclipboardformatname.asp

BOOL CloseClipboard(      

    VOID 
);"
	<apicall: long 'CloseClipboard' () module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 10/May/2006 16:09:00'!
format: format
	"ExtendedClipboardInterface format: 0"

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/emptyclipboard.asp

UINT EnumClipboardFormats(      

    UINT format
);"
	<apicall: ulong 'EnumClipboardFormats' (ulong) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 15/Feb/2008 00:09:00'!
getBitmapBitsFrom: handle count: nrOfBytes buffer: buffer
	"ExtendedClipboardWinInterface new getBitmapBitsFrom: handle count: nrOfBytes buffer: buffer"

	"http://msdn2.microsoft.com/en-us/library/ms532332(VS.85).aspx

LONG GetBitmapBits(
  HBITMAP hbmp,      // handle to bitmap
  LONG cbBuffer,     // number of bytes to copy
  LPVOID lpvBits     // buffer to receive bits
);"
	<apicall: long 'GetBitmapBits' (void* long byte* ) module: 'Gdi32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 10/May/2006 17:57:00'!
getClipboardData: format
	"ExtendedClipboardWinInterface new getClipboardData: 1"

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/getclipboarddata.asp

HANDLE GetClipboardData(      

    UINT uFormat
);"
	<apicall: byte * 'GetClipboardData' (ulong) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 14/Feb/2008 23:38:00'!
getClipboardDataHandle: format
	"ExtendedClipboardWinInterface new getBMPClipboardData: 2"

	"http://msdn2.microsoft.com/en-us/library/ms649039.aspx

HANDLE GetClipboardData(      

    UINT uFormat
);"
	<apicall: void* 'GetClipboardData' (ulong) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 10/May/2006 16:09:00'!
getClipboardFormatName: format name: string size: stringSize
	"ExtendedClipboardInterface getClipboardFormatName: 49171 name: (String new:128) size: 128"

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/getclipboardformatname.asp

int GetClipboardFormatName(      

    UINT format,
    LPTSTR lpszFormatName,
    int cchMaxCount
);"
	<apicall: long 'GetClipboardFormatNameA' (ulong char* long) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 23/Jan/2008 12:13:00'!
getClipboardSequenceNumber

	"http://msdn2.microsoft.com/en-us/library/ms649042(VS.85).aspx

 DWORD GetClipboardSequenceNumber(      
    VOID 
);"
	<apicall: long 'GetClipboardSequenceNumber' () module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 14/Feb/2008 21:57:00'!
getObject: handle bufferSize: bufferSize buffer: buffer
	"ExtendedClipboardWinInterface new getObject: handle bufferSize buffer: buffer"

	"http://msdn2.microsoft.com/en-us/library/ms533268.aspx

int GetObject(
  HGDIOBJ hgdiobj,  // handle to graphics object
  int cbBuffer,     // size of buffer for object information
  LPVOID lpvObject  // buffer for object information
);"
	<apicall: long 'GetObjectW' (void* long Win32TagBitmap *) module: 'Gdi32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'jmv 13/Mar/2012 12:33:00'!
listFormats
	"ExtendedClipboardWinInterface new listFormats"

	| currentFormat result data buffer |
	result := self openClipboard: 0.
	result = 0
		ifTrue: [^self halt].
	currentFormat := 0.
	[
		'' print.
		currentFormat := self format: currentFormat.
		Transcript show: 'format: '; show: currentFormat printString; newLine.
		(#(1 16 7 49510 49355) includes: currentFormat)
			ifTrue: [
				data := self getClipboardData: currentFormat.
				Transcript show: data fromCString; newLine].
"		13 = currentFormat
			ifTrue: [
				data := self getClipboardData: currentFormat.
				Transcript show: data fromUTF16String; newLine]."

		currentFormat > 18
			ifTrue: [
				buffer := ByteArray new:128.
				self getClipboardFormatName: currentFormat name: buffer size: buffer size.
				Transcript show: 'buffer: '; show: buffer; newLine].
		currentFormat ~= 0
	] whileTrue.
	self closeClipboard! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'mir 23/Jan/2008 12:11:00'!
openClipboard
	| result |
	result := self openClipboard: 0.
	lastSeenSequenceNr := self getClipboardSequenceNumber.
	result = 0
		ifTrue: [self halt].
! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'jmv 10/Nov/2013 09:51:00'!
openClipboard: windowOwner
	"ExtendedClipboardInterface openClipboard: 0"

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/openclipboard.asp

BOOL OpenClipboard(      

    HWND hWndNewOwner
);"
	<apicall: long 'OpenClipboard' (long) module: 'user32.dll'>
	"^self externalCallFailed"
	^0! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'jmv 29/Jan/2024 15:39:28'!
setClipboardFormat: format data: data 
	"ExtendedClipboardInterface current setClipboardFormat: 1 data: ('clipboard test' copyWith: (Character codePoint: 0)) "

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/setclipboarddata.asp

HANDLE SetClipboardData(      

    UINT uFormat,
    HANDLE hMem
);"
	<apicall: long 'SetClipboardData' (ulong char*) module: 'user32.dll'>
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'system calls' stamp: 'jmv 13/Jan/2011 11:16:00'!
setClipboardString: string
	"ExtendedClipboardInterface current setClipboardString: 'clipboard test'"

	"http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/clipboard/clipboardreference/clipboardfunctions/setclipboarddata.asp

HANDLE SetClipboardData(      

    UINT uFormat,
    HANDLE hMem
);"
"	Warning (jmv) This code from Sophie gives a compilation error in Cuis. Learn why.
	<apicall: long 'SetClipboardData' (ulong char*) module: 'user32.dll'>
"
	^self externalCallFailed! !

!ExtendedClipboardWinInterface methodsFor: 'testing' stamp: 'jmv 18/Jan/2011 22:27:00'!
canStore
	"Store into clipboard not yet implemented on Windows"
	^false! !

!ExtendedClipboardWinInterface methodsFor: 'testing' stamp: 'mir 23/Jan/2008 12:07:00'!
hasExternalClipboardChanged
	| prevSequenceNr |
	prevSequenceNr :=  self lastSeenSequenceNr.
	self
		openClipboard;
		closeClipboard.
	^prevSequenceNr < self lastSeenSequenceNr! !

!ExtendedClipboardWinInterface methodsFor: 'testing' stamp: 'jmv 10/Nov/2013 09:56:00'!
isOperational
	"Check that we can access the Wiindows clipboard."
	| result |
	result _ self openClipboard: 0.
	self attemptCloseClipboard.
	^result ~= 0! !

!ExtendedClipboardWinInterface methodsFor: 'api - retrieve' stamp: 'jmv 19/Jan/2011 15:18:00'!
retrieveId
	"Implement!!"
	^nil! !

!ExtendedClipboardWinInterface methodsFor: 'api - retrieve' stamp: 'jmv 7/Mar/2018 18:49:23'!
retrieveObject
	| conversion formatName bytes |
	conversion _ nil.
	self readPrimAvailableFormats do: [ :format |

		"Bitmap images "		
		format = 2 ifTrue: [
			^ self retrieveWindowsBMPForm ].	"CF_BITMAP: read using FFI"

		"Unformatted plain text"
		format = 1 ifTrue: [
			conversion _ #fromISO8859Bytes: ].	"'text/plain' CF_TEXT"
			"Check for other formats such as 13 'text/unicode' CF_UNICODETEXT if needed"


		format > 18 ifTrue: [
			formatName := ByteArray new:128.
			self getClipboardFormatName: format name: formatName size: formatName size.
			(formatName asString beginsWith: 'Rich Text Format') ifTrue: [
				self canImportRTF ifTrue: [conversion _ #fromRTFBytes:] ].		"text/rtf" "Only if package RTFImporting is loaded"
		].

		"Work on this when we do copy to clipboard"
		"[ 'cuis-refStreamed' ] -> [ #fromRefStreamBytes: ]"

		conversion ifNotNil: [
			bytes _ self readClipboardData: format.
			^self perform: conversion with: bytes ]
	].
	^ nil	"Could not read any of the available formats"! !

!ExtendedClipboardWinInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeForm: aForm id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	"To be implemented"
	self halt! !

!ExtendedClipboardWinInterface methodsFor: 'api - store' stamp: 'jmv 26/Jan/2011 10:07:00'!
storeString: aString id: otherString
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id at the end."
	"To be implemented"
	self halt! !

!ExtendedClipboardWinInterface methodsFor: 'api - store' stamp: 'jmv 17/Mar/2011 09:35:00'!
storeText: aText id: otherString
	"To be implemented (store both as string and as RTF)"
	"I presume the order is: most preferred format first, least desirable format last.
	Cuis object id is usually at the end, but don't include it if we include a streamed representation."
	self halt! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmBits
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: 21) type: ExternalType byte asPointerType! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmBits: anObject
	"This method was automatically generated"
	handle pointerAt: 21 put: anObject getHandle.! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmBitsPixel
	"This method was automatically generated"
	^handle signedShortAt: 19! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmBitsPixel: anObject
	"This method was automatically generated"
	handle signedShortAt: 19 put: anObject! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmHeight
	"This method was automatically generated"
	^handle signedLongAt: 9! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmHeight: anObject
	"This method was automatically generated"
	handle signedLongAt: 9 put: anObject! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmPlanes
	"This method was automatically generated"
	^handle signedShortAt: 17! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmPlanes: anObject
	"This method was automatically generated"
	handle signedShortAt: 17 put: anObject! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmType
	"This method was automatically generated"
	^handle signedLongAt: 1! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmType: anObject
	"This method was automatically generated"
	handle signedLongAt: 1 put: anObject! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmWidth
	"This method was automatically generated"
	^handle signedLongAt: 5! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmWidth: anObject
	"This method was automatically generated"
	handle signedLongAt: 5 put: anObject! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmWidthBytes
	"This method was automatically generated"
	^handle signedLongAt: 13! !

!Win32Bitmap methodsFor: 'accessing' stamp: 'mir 14/Feb/2008 22:47:00'!
bmWidthBytes: anObject
	"This method was automatically generated"
	handle signedLongAt: 13 put: anObject! !

!Win32Bitmap class methodsFor: 'instance creation' stamp: 'mir 14/Feb/2008 22:47:00'!
fields
	"Win32TagBitmap defineFields"


	^ #(
		#(#bmType 'long')
		#(#bmWidth 'long')
		#(#bmHeight 'long')
		#(#bmWidthBytes 'long')
		#(#bmPlanes 'short')
		#(#bmBitsPixel 'short')
		#(#bmBits 'byte*')
	)

"typedef struct tagBITMAP {
  LONG   bmType; 
  LONG   bmWidth; 
  LONG   bmHeight; 
  LONG   bmWidthBytes; 
  WORD   bmPlanes; 
  WORD   bmBitsPixel; 
  LPVOID bmBits; 
} BITMAP"
! !

!Form methodsFor: '*extendedClipboard-Win32' stamp: 'mir 16/Feb/2008 19:52:00'!
fromWin32BitMap: bitmapBits
	0 to: self height-1 do: [:y |
		1 to: self width do: [:x |
			bits integerAt: (y*width) + x put: ((bitmapBits unsignedLongAt: 1 + (y*width*4) + ((x-1) *4)) bitOr: 16rFF000000)
	]].
! !

!Form class methodsFor: '*extendedClipboard-Win32' stamp: 'mir 15/Feb/2008 10:24:00'!
fromWin32BitMap: bitmap bits: bits
	| form |
	form := self extent: bitmap bmWidth@bitmap bmHeight depth: bitmap bmBitsPixel.
	form fromWin32BitMap: bits.
	^form! !
ExtendedClipboardInterface initialize!
