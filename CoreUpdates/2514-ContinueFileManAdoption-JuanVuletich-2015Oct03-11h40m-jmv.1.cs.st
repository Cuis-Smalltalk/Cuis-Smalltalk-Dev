'From Cuis 4.2 of 25 July 2013 [latest update: #2513] on 3 October 2015 at 2:09:40.148821 pm'!

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 10/3/2015 14:02'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	process _ tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	depth _ 20.
	stack _ process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext _ process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine ]]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 10/3/2015 13:54'!
fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	((Smalltalk imageName asFileEntry name withoutSuffix: '.image'), '-AllCode.st') asFileEntry writeStream: [ :stream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: stream]]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 10/3/2015 11:46'!
save

	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		(FileDirectory forFullFileName: fullFileName) exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			FileDirectory smalltalkImageDirectory fullNameFor: self packageFileName].

	fullFileName asFileEntry forceWriteStream: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!SystemDictionary methodsFor: 'image format' stamp: 'jmv 10/3/2015 13:43'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ Smalltalk imageName asFileEntry readStream: [ :stream |
		(stream binary; next: 4)
			unsignedLongAt: 1
			bigEndian: Smalltalk isBigEndian ]! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 10/3/2015 11:55'!
fromFileNamedOrNil: fileName
	"Read a Form or ColorForm from the given file.
	This method shows the usual way to do this."

	^ [ self fromFileNamed: fileName ]
		on: FileDoesNotExistException
		do: nil! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 10/3/2015 12:21'!
putForm: aForm onFileNamed: fileName
	"Store the given form on a file of the given name."

	fileName asFileEntry forceWriteStream: [ :stream |
		(self onBinaryStream: stream binary) nextPutImage: aForm ]! !


!JPEGReadWriter2 methodsFor: 'public access' stamp: 'jmv 10/3/2015 12:24'!
nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	"odd width images of depth 16 give problems; avoid them."
	sourceForm _ (aForm depth = 32) | (aForm width even & (aForm depth = 16))
		ifTrue: [aForm]
		ifFalse: [aForm asFormOfDepth: 32].
	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	buffer _ ByteArray new: sourceForm width * sourceForm height.
	byteCount _ self primJPEGWriteImage: jpegCompressStruct 
		onByteArray: buffer
		form: sourceForm
		quality: quality
		progressiveJPEG: progressiveFlag
		errorMgr: jpegErrorMgr2Struct.
	byteCount = 0 ifTrue: [self error: 'buffer too small for compressed data'].
	stream next: byteCount putAll: buffer startingAt: 1.
	stream close! !


!ContextPart class methodsFor: 'examples' stamp: 'jmv 10/3/2015 11:49'!
trace: aBlock onFileNamed: fileName
	"
	ContextPart trace: [3 factorial] onFileNamed: 'trace'
	"
	"This method uses the simulator to print calls to a file."

	fileName asFileEntry forceWriteStream: [ :stream |
		self trace: aBlock on: stream ]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/3/2015 12:47'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/STspace.text') asFileEntry writeStream: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 10/3/2015 12:50'!
crc16OfFileNamed: fileName
	"
	StandardFileStream crc16OfFileNamed: 'cursor.jpeg'
	StandardFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStream: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 10/3/2015 13:58'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex _ 1.
		lastIndex _ 1.
		entries at: 1 put: 'Transcript'.	
		unfinishedEntry reset.
		lastDisplayPosition _ 0.
		
		logToFile ifTrue: [
			self filename asFileEntry forceWriteStream: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 10/3/2015 13:58'!
clearFile
	accessSemaphore critical: [
		self filename asFileEntry forceWriteStream: [ :stream |
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 10/3/2015 13:57'!
addEntry: aString logToFile: otherString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."

	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [
			firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		"external file"
		otherString ifNotNil: [
			self filename asFileEntry appendStream: [ :stream |
				stream nextPutAll: otherString ]]
	]! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 10/3/2015 14:03'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler |
	
	"VM statistics (Memory use and GC, mainly)"
	'MemoryStats.txt' asFileEntry forceWriteStream: [ :stream |
		stream nextPutAll: Utilities vmStatisticsReportString ].
	
	"Process list"
	'ProcessList.txt' asFileEntry forceWriteStream: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	'ThePig.txt' asFileEntry forceWriteStream: [ :stream |
		ProcessBrowser dumpPigStackOn: stream ].
	
	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		'FullTally.txt' asFileEntry forceWriteStream: [ :stream |
			profiler _ AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	'MemoryAnalysis.txt' asFileEntry forceWriteStream: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ]

] forkNamed: 'CPU usage analysis'! !

!methodRemoval: ProcessBrowser class #dumpPigStackOn:andClose:!
ProcessBrowser class removeSelector: #dumpPigStackOn:andClose:!
