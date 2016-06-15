'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 15 June 2016 at 2:02:02.670984 pm'!

!AndreasSystemProfiler methodsFor: 'accessing result' stamp: 'jmv 6/15/2016 12:38'!
totalMillisecondsFor: aSelector
	"Answer the sum of the time spent in all appareances in aSelector in the tree"
	| totalTallies |
	totalTallies _ 0.
	tallyRoot
		treeDFSDo: [ :eachTally |
			(eachTally methodSymbol == aSelector and: [ eachTally blockNestingCount = 0]) ifTrue: [
				totalTallies _ eachTally tally + totalTallies ]]
		afterChildrenDo: [ :eachTally | ]
		threshold: 0.0.
	^ totalTallies asFloat / tallyRoot tally * totalTime! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 6/15/2016 14:00'!
treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold
	"Deep First Search"
	| sons |
	aBlock value: self.
	sons _ self sonsOver: threshold.
	sons isEmpty ifFalse: [
		sons _ self sortCollectionFrom: sons.
		1 to: sons size do: [ :i |
			(sons at: i) treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold ]].
	anotherBlock value: self! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 6/15/2016 12:13'!
rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups dashes line tabsAndTreeLines thisTab |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g | | sons |
		sons _ self sortCollectionFrom: g.
		dashes _ '--------------------------------'.
		line _ 'Process: ', (g anyOne process browserPrintStringFull: false).
		textStream
			nextPutAll: dashes; newLine;
			nextPutAll: line; newLine;
			nextPutAll: dashes; newLine.
		linesStream
			nextPut: dashes;
			nextPut: line;
			nextPut: dashes.
		talliesStream
			nextPut: nil; nextPut: nil; nextPut: nil.
		sons do: [ :son |
			tabsAndTreeLines _ OrderedCollection new.
			thisTab _ '  '.
			son
				treeDFSDo: [ :eachTally |
					eachTally printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime.
					tabsAndTreeLines addLast: thisTab.
					thisTab := (eachTally sonsOver: threshold) size > 1 ifTrue: ['  |'] ifFalse: ['  '].
				]
				afterChildrenDo: [ :eachTally | tabsAndTreeLines removeLast ]
				threshold: threshold.
			].
		textStream newLine ]! !

!methodRemoval: QAbstractTally #treePrintOn:linesOn:talliesOn:tabs:thisTab:total:totalTime:threshold:!
QAbstractTally removeSelector: #treePrintOn:linesOn:talliesOn:tabs:thisTab:total:totalTime:threshold:!
