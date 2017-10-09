'From Cuis 5.0 of 7 November 2016 [latest update: #3193] on 8 October 2017 at 6:43:01 pm'!

!WriteStream methodsFor: 'services' stamp: 'jmv 10/8/2017 17:58:45'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	See inheritance"! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:58:13'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [Character space numericValue]
		ifFalse: [Character space ].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad)! !


!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:02:10'!
basicNext
	"Answer the next byte or character (depending on mode) from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:02:58'!
nextPut: char
	"Write the given byte or character (depending on mode) to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: char.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	^ char
! !

!StandardFileStream methodsFor: 'private' stamp: 'jmv 10/8/2017 17:04:44'!
collectionSpecies
	"Answer the species of collection into which the receiver can stream.
	This is ByteArray or String, depending on the mode."
	
	^buffer1 species! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 10/8/2017 17:59:09'!
nextPut: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.
	You can see an analysis of which objects are written out by doing: 
	(SmartRefStream statsOfSubObjects: anObject)
	(SmartRefStream tallyOfSubObjects: anObject)
	(SmartRefStream subObjects: anObject ofClass: aClass)"

| info |
topCall
	ifNil: [
		topCall _ anObject.
		'Please wait while objects are counted' 
			displayProgressAt: Sensor mousePoint
			from: 0 to: 10
			during: [ :barBlock | info _ self instVarInfo: anObject].
		byteStream binary.
		'Writing an object file' displayProgressAt: Sensor mousePoint
			from: 0 to: objCount*4	"estimate"
			during: [ :barBlock |
				objCount _ 0.
				progressBar _ barBlock.
				self setStream: byteStream reading: false.
					"set basePos, but keep any class renames"
				super nextPut: ReferenceStream versionCode.
				super nextPut: info.
				super nextPut: anObject.		"<- the real writing"
				].
					"Note: the terminator, $!!, is not doubled inside object data"
		"references is an IDict of every object that got written"
		byteStream ascii.
		byteStream nextPutAll: '!!'; newLine; newLine.
		byteStream padToEndIfCantTruncate.
		topCall _ progressBar _ nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].
! !

!methodRemoval: DummyStream #padToEndWith:!
DummyStream removeSelector: #padToEndWith:!
!methodRemoval: StandardFileStream #padToEndWith:!
StandardFileStream removeSelector: #padToEndWith:!
!methodRemoval: FileStream #text!
FileStream removeSelector: #text!
!methodRemoval: RWBinaryOrTextStream #padToEndWith:!
RWBinaryOrTextStream removeSelector: #padToEndWith:!
!methodRemoval: RWBinaryOrTextStream #text!
RWBinaryOrTextStream removeSelector: #text!

!RWBinaryOrTextStream reorganize!
('accessing' reset)
('testing' isBinary)
('mode' useBytes useCharacters)
('services' contents)
('reading' next next: next:into:startingAt: upToEnd)
('writing' next:putAll:startingAt: nextPut: nextPutAll:)
!

