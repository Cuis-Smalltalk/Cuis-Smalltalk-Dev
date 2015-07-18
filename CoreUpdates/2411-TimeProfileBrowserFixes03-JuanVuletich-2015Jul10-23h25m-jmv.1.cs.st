'From Cuis 4.2 of 25 July 2013 [latest update: #2410] on 10 July 2015 at 11:27:33.43291 pm'!

!MessageSet methodsFor: 'contents' stamp: 'jmv 7/10/2015 23:26'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector source |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment].
		selector == #Definition ifTrue: [
			^ class definition].
		selector == #Hierarchy ifTrue: [^ class printHierarchy]].
	source _ class sourceMethodAt: selector ifAbsent: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	source _ self sourceStringPrettifiedAndDiffed.
	^ source asText makeSelectorBoldIn: class! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 23:27'!
reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows line |
	upTime := totalTime.
	oldSpaceEnd		:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**Memory**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ ' old ', oldSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ ' young ', (youngSpaceEnd - oldSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  ' used ', youngSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  ' free ', (memoryEnd - youngSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**GCs**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' full ', fullGCs printString,' totalling ', fullGCTime printStringWithCommas, 'ms (', (100.0 * fullGCTime / upTime) rounded printString, '% uptime)'.
	fullGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * fullGCTime / fullGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' incr ', incrGCs printString, ' totalling ', incrGCTime printStringWithCommas, 'ms (', (100.0 * incrGCTime / upTime) rounded printString, '% uptime)'.
	incrGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * incrGCTime / incrGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' tenures ', tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		line _ line, ' (avg ', (1.0 * incrGCs / tenureCount) asInteger printString, ' GCs/tenure)'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' root table ', rootOverflows printStringWithCommas, ' overflows'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.! !

