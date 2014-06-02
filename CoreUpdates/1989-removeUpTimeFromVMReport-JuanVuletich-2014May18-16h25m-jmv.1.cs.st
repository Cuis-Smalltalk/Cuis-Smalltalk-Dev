'From Cuis 4.2 of 25 July 2013 [latest update: #1987] on 18 May 2014 at 4:32:46.105292 pm'!

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 5/18/2014 16:32'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.

	str := WriteStream on: (String new: 1000).

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd - oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	memoryEnd - youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 printStringWithCommas; nextPutAll: 'ms'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 printStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

