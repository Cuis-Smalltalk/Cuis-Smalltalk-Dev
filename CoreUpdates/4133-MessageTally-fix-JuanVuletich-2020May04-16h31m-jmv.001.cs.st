'From Cuis 5.0 [latest update: #4127] on 4 May 2020 at 4:40:37 pm'!

!QSystemTally methodsFor: 'report' stamp: 'jmv 5/4/2020 16:31:25'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream nextPutAll: '% ('.
		percentage * totalTime / 100 printOn: lineStream fractionDigits: 1.
		lineStream nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [
			self flag: #profilerFriendlyCall:.
			parentTally methodSymbol == #profilerFriendlyCall:
				ifTrue: [
					lineStream nextPutAll: '   -- primitive (reported properly)' ]
				ifFalse: [
					lineStream nextPutAll: '   -- primitive (real sender possibly omitted, see #profilerFriendlyCall:)' ]
			].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !

