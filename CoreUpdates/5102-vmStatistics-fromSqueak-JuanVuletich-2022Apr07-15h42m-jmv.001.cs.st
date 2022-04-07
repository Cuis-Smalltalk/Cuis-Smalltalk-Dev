'From Cuis 6.0 [latest update: #5101] on 7 April 2022 at 4:24:09 pm'!

!Stream methodsFor: 'printing' stamp: 'jmv 4/7/2022 16:03:27'!
print: anObject fractionDigits:  placesDesired
	anObject printOn: self fractionDigits:  placesDesired! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 4/7/2022 16:10:38'!
formatVMParameter: aNumber
	"Format a VM parameter that may have been reported either as a Float or
	an Integer. If a VM parameter is defined in units of milliseconds but internally
	measured to microsecond precision by the VM, the VM may choose to return
	the value as a Float in order to retain full precision. Otther representations
	such as Fraction may seem more natural but are difficult to instantiate in
	the VM, hence the presumption of Float for representation of higher precision
	millisecond values."

	^ aNumber isFloat
		ifTrue: [ aNumber < 1000
			ifTrue: [ aNumber printStringFractionDigits: 5 ]
			ifFalse: [ aNumber rounded printStringWithCommas ] ]
		ifFalse: [ aNumber asInteger printStringWithCommas ].! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 4/7/2022 16:22:50'!
vmStatisticsReportOn: str
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memorySize fullGCs fullGCTime fullGCCompactionTime incrGCs incrGCTime tenureCount upTime
	  upTime2 fullGCs2 fullGCTime2  incrGCs2 incrGCTime2 tenureCount2 freeSize youngSize used spur timeReporter idleMs idleMs2 codeCompactionTime codeCompactionsOrNil codeCompactionTime2 codeCompactions2 maxParams |
	"Pad to protect for a VM that does not answer the expected parameter array"
	maxParams := 63. "max position referenced in this method"
	params := Array streamContents: [ :strm |
				strm nextPutAll: Smalltalk getVMParameters.
				[strm position < maxParams] whileTrue: [strm nextPut: nil]].
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memorySize			:= params at: 3.
	fullGCs			:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs			:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	fullGCCompactionTime	:= params at: 18.
	codeCompactionsOrNil	:= params at: 62.
	codeCompactionTime	:= params at: 63.
	
	upTime := (params at: 20) ~= 0 "utcMicrosecondClock at startup in later Spur VMs"
				ifTrue: [Time primUtcMicrosecondClock - (params at: 20) + 500 // 1000]
				ifFalse: [Time primMillisecondClock]. "Fall back on old microsecond clock; Good for 47.5 days"
	spur := (params at: 41)
				ifNotNil: [ :p | p anyMask: 16]
				ifNil: [false].
	timeReporter := [:time| | seconds |
					seconds := time / 1000.
					seconds >= (60*60*24)
						ifTrue:
							[str print: seconds / 60 // 60 // 24; nextPut: $d; space;
								print: seconds / 60 // 60 \\ 24; nextPut: $h; space]
						ifFalse:
							[seconds >= (60*60) ifTrue:
								[str print: (seconds / 60 // 60); nextPut: $h; space]].
					str
						print: (seconds / 60 \\ 60) asInteger; nextPut: $m; space;
						print: (seconds \\ 60) asInteger; nextPut: $s].
	str	nextPutAll: 'uptime		'.
	timeReporter value: upTime.
	(idleMs := params at: 16) ~= 0 ifTrue:
		[str tab; nextPutAll: '(runtime '.
		 idleMs := idleMs // 1000.
		 timeReporter value: upTime - idleMs.
		 str nextPutAll: ', idletime '.
		 timeReporter value: idleMs.
		 str nextPut: $)].
	str newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: (self formatVMParameter: memorySize); nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old		';
		nextPutAll: (self formatVMParameter: oldSpaceEnd); nextPutAll: ' bytes (';
		print: oldSpaceEnd / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	spur ifTrue:
		[(youngSize := (params at: 19)  * 7 // 5) = 0 ifTrue: [params at: 44].
		 youngSize := youngSize roundUpTo: 1024.
		 str	nextPutAll: '	young		';
			nextPutAll: (self formatVMParameter: youngSize); nextPutAll: ' bytes (';
			print: youngSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
		youngSize := youngSpaceEnd. "used eden"
		freeSize := (params at: 54) + (params at: 44) - youngSize.
		used := youngSize + oldSpaceEnd - freeSize.
		str	nextPutAll: '	used		';
			nextPutAll: (self formatVMParameter: used); nextPutAll: ' bytes (';
			print: used / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	free		';
			nextPutAll: (self formatVMParameter: freeSize); nextPutAll: ' bytes (';
			print: freeSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine]
		ifFalse: "Earlier VM and V3 object memory"
		[str	nextPutAll: '	young		';
			nextPutAll: (self formatVMParameter: (youngSpaceEnd - oldSpaceEnd)); nextPutAll: ' bytes (';
			print: (youngSpaceEnd - oldSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	used		';
			nextPutAll: (self formatVMParameter: youngSpaceEnd); nextPutAll: ' bytes (';
			print: (youngSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine.
		str	nextPutAll: '	free		';
			nextPutAll: (self formatVMParameter: (memorySize - youngSpaceEnd)); nextPutAll: ' bytes (';
			print: (memorySize - youngSpaceEnd / memorySize * 100) fractionDigits: 1; nextPutAll: '%)'; newLine].

	str	nextPutAll: 'GCs					';
		nextPutAll: (self formatVMParameter: (fullGCs + incrGCs)).
	fullGCs + incrGCs > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime / (fullGCs + incrGCs) fractionDigits: 1; 
			nextPutAll: ' ms between GCs'.
		 idleMs > 0 ifTrue:
			[str
				space;
				print: upTime - idleMs / (fullGCs + incrGCs) fractionDigits: 1; 
				nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime); nextPutAll: ' ms (';
		print: fullGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime)';
		nextPutAll: ', avg '; print: (fullGCTime / (fullGCs max: 1)) fractionDigits: 1; nextPutAll: ' ms'.
	(spur and: [fullGCCompactionTime ~= 0]) ifTrue:
		[str	newLine; nextPutAll: '		marking			';
			nextPutAll: (self formatVMParameter: (fullGCTime - fullGCCompactionTime)); nextPutAll: ' ms (';
			print: (fullGCTime - fullGCCompactionTime) / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: (fullGCTime - fullGCCompactionTime / (fullGCs max: 1)) fractionDigits: 1;
			nextPutAll: ' ms,'; newLine;
			nextPutAll: '		compacting			';
			nextPutAll: (self formatVMParameter: fullGCCompactionTime); nextPutAll: ' ms (';
			print: fullGCCompactionTime / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: fullGCCompactionTime / (fullGCs max: 1) fractionDigits: 1;
			nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: (spur ifTrue: ['	scavenges				'] ifFalse: ['	incr			']);
		nextPutAll: (self formatVMParameter: incrGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime); nextPutAll: ' ms (';
		print: incrGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime), avg '; print: incrGCTime / incrGCs fractionDigits: 1; nextPutAll: ' ms'; newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount).
	tenureCount = 0 ifFalse:
		[spur
			ifTrue: [str nextPutAll: ' (avg '; print: tenureCount // (incrGCs max: 1); nextPutAll: ' tenures per scavenge)']
			ifFalse: [str nextPutAll: ' (avg '; print: incrGCs // tenureCount; nextPutAll: ' GCs/tenure)']].
	str	newLine.

	codeCompactionsOrNil ifNotNil:
		[str
			nextPutAll: '	code compactions				'.
		codeCompactionsOrNil = 0
			ifTrue: [str
				nextPutAll: codeCompactionsOrNil asString; newLine]
			ifFalse: [str
				nextPutAll: (self formatVMParameter: codeCompactionsOrNil); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime); nextPutAll: ' ms (';
				print: codeCompactionTime / (upTime - idleMs) * 100 fractionDigits: 3;
				nextPutAll: '% runtime), avg '; print: codeCompactionTime / codeCompactionsOrNil fractionDigits: 1; nextPutAll: ' ms'; newLine]].

LastStats ifNil: [LastStats := Array new: 9]
ifNotNil: [
	LastStats size < 9 ifTrue:
		[LastStats := LastStats, (Array new: 9 - LastStats size withAll: 0)].
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).
	idleMs2 := idleMs - (LastStats at: 7).
	codeCompactionsOrNil ifNotNil:
		[codeCompactions2	:= codeCompactionsOrNil - (LastStats at: 8).
		 codeCompactionTime2	:= codeCompactionTime - (LastStats at: 9)].

	str	nextPutAll: 'GCs since last view					';
		nextPutAll: (self formatVMParameter: (fullGCs2 + incrGCs2)).
	fullGCs2 + incrGCs2 > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime2 // (fullGCs2 + incrGCs2); 
			nextPutAll: ' ms between GCs'.
			idleMs2 > 0 ifTrue:
				[str
					nextPutAll: ', ';
					print: upTime2 - idleMs2 // (fullGCs2 + incrGCs2);
					nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str	newLine.
	str	nextPutAll: '	uptime				'; print: (upTime2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, runtime ';		   print: (upTime2 - idleMs2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, idletime ';		   print: (idleMs2 / 1000.0) fractionDigits: 1; nextPutAll: ' s'; newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs2).
	fullGCs2 + fullGCTime2 > 0 ifTrue:
		[str	nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime2); nextPutAll: ' ms (';
			print: fullGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
			nextPutAll: '% runtime)'.
		 str	nextPutAll: ', avg '; print: fullGCTime2 / (fullGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: (spur ifTrue: ['	scavenges				'] ifFalse: ['	incr			']);
		nextPutAll: (self formatVMParameter: incrGCs2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime2); nextPutAll: ' ms (';
		print: incrGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
		nextPutAll: '% runtime), avg '; print: incrGCTime2 / (incrGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'.
	str	newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount2).
	tenureCount2 = 0 ifFalse:
		[spur
			ifTrue: [str nextPutAll: ' (avg '; print: tenureCount2 // (incrGCs2 max: 1); nextPutAll: ' tenures per scavenge)']
			ifFalse: [str nextPutAll: ' (avg '; print: incrGCs2 // tenureCount2; nextPutAll: ' GCs/tenure)']].
	str	newLine.
	codeCompactionsOrNil ifNotNil:
		[str	nextPutAll: '	code compactions				';
			nextPutAll: (self formatVMParameter: codeCompactions2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime2); nextPutAll: ' ms (';
			print: codeCompactionTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 3;
			nextPutAll: '% runtime)'.
		 codeCompactions2 > 0 ifTrue:
			[str nextPutAll: ', avg '; print: codeCompactionTime2 / codeCompactions2 fractionDigits: 1; nextPutAll: ' ms'; newLine.]]
].
	LastStats := { upTime. fullGCs. fullGCTime. incrGCs. incrGCTime. tenureCount. idleMs. codeCompactionsOrNil. codeCompactionTime }.! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 4/7/2022 16:23:20'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	^String streamContents: [:s| self vmStatisticsReportOn: s] estimatedSize: 2048! !

!methodRemoval: Utilities class #textMarkerForShortReport stamp: 'jmv 4/7/2022 16:07:11'!
Utilities class removeSelector: #textMarkerForShortReport!
