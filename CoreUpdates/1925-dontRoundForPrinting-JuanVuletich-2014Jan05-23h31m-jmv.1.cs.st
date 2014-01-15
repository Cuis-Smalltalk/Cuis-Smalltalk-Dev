'From Cuis 4.2 of 25 July 2013 [latest update: #1924] on 5 January 2014 at 11:31:29.631275 pm'!

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 1/5/2014 21:52'!
processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally _ CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent _ tally
				ifNotNil: [
					(((tally occurrencesOf: each) * 100.0 / tally size) rounded
						asString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (each browserPrintStringFull: false)
		]! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 1/5/2014 21:52'!
dumpTally: tally on: aStream
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	tally sortedCounts do: [ :assoc |
		((assoc key / tally size) * 100.0) rounded printOn: aStream.
		aStream
			nextPutAll: '%   ';
			print: assoc value identityHash; space;
			nextPutAll: assoc value name;
			newLine.
	]! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 1/5/2014 21:23'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd			:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows asStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !


!MessageTally methodsFor: 'reporting' stamp: 'jmv 1/5/2014 21:25'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime _ time.
	oldSpaceEnd			_ gcStats at: 1.
	youngSpaceEnd		_ gcStats at: 2.
	memoryEnd			_ gcStats at: 3.
	fullGCs				_ gcStats at: 7.
	fullGCTime			_ gcStats at: 8.
	incrGCs				_ gcStats at: 9.
	incrGCTime			_ gcStats at: 10.
	tenureCount			_ gcStats at: 11.
	rootOverflows		_ gcStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows asStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 1/5/2014 21:51'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime upTime2 fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	upTime := Time millisecondClockValue.

	str := WriteStream on: (String new: 1000).
	str	nextPutAll: 'uptime			';
		print: (upTime / 1000 / 60 // 60); nextPut: $h;
		print: (upTime / 1000 / 60 \\ 60) asInteger; nextPut: $m;
		print: (upTime / 1000 \\ 60) asInteger; nextPut: $s; newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd asStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd asStringWithCommas; nextPutAll: ' bytes (';
		print: ((oldSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommas; nextPutAll: ' bytes (';
		print: ((youngSpaceEnd - oldSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommas; nextPutAll: ' bytes (';
		print: ((youngSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommas; nextPutAll: ' bytes (';
		print: ((memoryEnd - youngSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) asStringWithCommas.
	fullGCs + incrGCs > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime / (fullGCs + incrGCs)) rounded); 
			nextPutAll: 'ms between GCs)'
	].
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime), avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) asStringWithCommas.
	fullGCs2 + incrGCs2 > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime2 / (fullGCs2 + incrGCs2)) rounded); 
			nextPutAll: 'ms between GCs)'.
	].
	str newLine.
	str	nextPutAll: '	uptime		'; print: ((upTime2 / 1000.0) rounded); nextPutAll: 's'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime), avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 asStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 1 put: upTime.
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

