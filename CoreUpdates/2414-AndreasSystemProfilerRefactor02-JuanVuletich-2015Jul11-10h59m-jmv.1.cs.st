'From Cuis 4.2 of 25 July 2013 [latest update: #2413] on 11 July 2015 at 11:18:07.783281 am'!

!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/11/2015 11:15'!
printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime
	self subclassResponsibility! !


!QFakeTally methodsFor: 'report' stamp: 'jmv 7/11/2015 11:17'!
printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime

	| percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage := tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 1.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		lineStream
			nextPutAll: '-----Primitives (called at an unknown point below here in the call tree)------' ].

	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: self! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 7/11/2015 11:17'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 1.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector ].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 23:32'!
report
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :textStream |
		self
			reportTextOn: textStream
			linesOn: (DummyStream on: nil)
			talliesOn: (DummyStream on: nil) ]! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 1/7/2014 21:39'!
report: strm cutoff: threshold
	tallyRoot tally = 0
		ifTrue: [ strm nextPutAll: ' - no tallies obtained' ]
		ifFalse: [
			strm
				nextPutAll: 'Reporting - ' , totalTally printStringWithCommas;
				nextPutAll: ' tallies, ' , totalTime printStringWithCommas , ' msec.';
				newLine;
				newLine.
			tallyRoot
				fullPrintOn: strm
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: strm.
		self reportProcessStatsOn: strm ].! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 7/11/2015 11:16'!
treePrintOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines thisTab: myTabAndTreeLines total: total totalTime: totalTime threshold: threshold
	| sons sonTabAndTreeLine |
	self printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime.
	sons := self sonsOver: threshold.
	sons isEmpty ifFalse:[
		tabsAndTreeLines addLast: myTabAndTreeLines.
		sons := self sortCollectionFrom: sons.
		1 to: sons size do:[:i | 
			sonTabAndTreeLine := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
			(sons at: i)
				treePrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream
				tabs: tabsAndTreeLines
				thisTab: sonTabAndTreeLine
				total: total
				totalTime: totalTime
				threshold: threshold].
			tabsAndTreeLines removeLast ]! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/17/2015 22:44'!
printOn: aStream
	blockNesting timesRepeat: [
		aStream nextPutAll: '[] in '].
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally! !

!methodRemoval: QSystemTally #printOn:linesOn:talliesOn:total:totalTime:!
QSystemTally removeSelector: #printOn:linesOn:talliesOn:total:totalTime:!
!methodRemoval: QFakeTally #printOn:linesOn:talliesOn:total:totalTime:!
QFakeTally removeSelector: #printOn:linesOn:talliesOn:total:totalTime:!
!methodRemoval: QAbstractTally #printOn:linesOn:talliesOn:total:totalTime:!
QAbstractTally removeSelector: #printOn:linesOn:talliesOn:total:totalTime:!
