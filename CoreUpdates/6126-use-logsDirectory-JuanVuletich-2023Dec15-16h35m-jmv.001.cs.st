'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:37:28 pm'!

!ContextPart class methodsFor: 'examples' stamp: 'jmv 12/15/2023 14:55:41'!
trace: aBlock onFileNamed: fileName
	"
	ContextPart trace: [3 factorial] onFileNamed: 'trace'
	"
	"This method uses the simulator to print calls to a file."

	DirectoryEntry logsDirectory // fileName forceWriteStreamDo: [ :stream |
		self trace: aBlock on: stream ].! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/15/2023 15:23:30'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename := (Preferences at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-', Utilities dateTimeSuffix, '.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file := DirectoryEntry logsDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 12/15/2023 15:07:56'!
clear

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 1.
		entries atAllPut: nil.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition := 0.
		
		logToFile ifTrue: [
			DirectoryEntry logsDirectory // self filename forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 12/15/2023 15:08:10'!
clearFile
	accessSemaphore critical: [
		DirectoryEntry logsDirectory // self filename forceWriteStreamDo: [ :stream |
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 12/15/2023 15:08:17'!
writeToFile: anEntry

	logToFile ifTrue: [
		DirectoryEntry logsDirectory // self filename appendStreamDo: [ :stream | stream nextPutAll: anEntry ]]! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 12/15/2023 15:09:10'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler pig |
	
	"VM statistics (Memory use and GC, mainly)"
	DirectoryEntry logsDirectory // 'MemoryStats.txt' forceWriteStreamDo: [ :stream |
		stream nextPutAll: self vmStatisticsReportString ].

	"Process list"
	DirectoryEntry logsDirectory // 'ProcessList.txt' forceWriteStreamDo: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

	"Process taking most CPU"
	DirectoryEntry logsDirectory // 'ThePig.txt' forceWriteStreamDo: [ :stream |
		pig := ProcessBrowser dumpPigStackOn: stream ].

	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		DirectoryEntry logsDirectory // 'FullTally.txt' forceWriteStreamDo: [ :stream |
			profiler := AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	DirectoryEntry logsDirectory // 'MemoryAnalysis.txt' forceWriteStreamDo: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ].

	^pig! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 12/15/2023 15:28:47'!
printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	DirectoryEntry logsDirectory // 'STspace.text' writeStreamDo: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 12/15/2023 15:28:53'!
printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		'STspace.text1' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
			--- do something that uses space here ---
		'STspace.text2' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| coll1 coll2 item |
	coll1 := OrderedCollection new.
	DirectoryEntry logsDirectory // fileName1 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [coll1 add: stream crLfNextLine]].
	
	coll2 := OrderedCollection new.
	DirectoryEntry logsDirectory // fileName2 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [
			item := stream crLfNextLine.
			((coll1 includes: item) and: [(item endsWith: 'percent') not])
				ifTrue: [coll1 remove: item]
				ifFalse: [coll2 add: item]]].

	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.
! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 12/15/2023 15:29:00'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	DirectoryEntry logsDirectory // aFileName forceWriteStreamDo: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asPlainString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !

