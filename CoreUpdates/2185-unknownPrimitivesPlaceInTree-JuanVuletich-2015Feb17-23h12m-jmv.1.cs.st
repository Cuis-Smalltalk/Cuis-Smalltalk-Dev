'From Cuis 4.2 of 25 July 2013 [latest update: #2184] on 17 February 2015 at 11:13:13.068604 pm'!
!classDefinition: #QAbstractTally category: #'Tools-Profiling'!
Link subclass: #QAbstractTally
	instanceVariableNames: 'process receivers tally'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #QFakeTally category: #'Tools-Profiling'!
QAbstractTally subclass: #QFakeTally
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QAbstractTally methodsFor: 'converting' stamp: 'jmv 2/17/2015 20:28'!
asArray
	"esto es feo. normalmente los link esta en un LinkedList.
	Estamos de acuerdo que usualmente es el pedo... pero, es el elemento una coleccion o no????"
	"(jmv) (style) Mhhh. Normally instances of Link are in a LinkedList, that provides collection protocol.
	Here, we allow a subInstance of Link to start behaving as a Collection.
	Not bad per se, but inconsistent with other uses of Link..."
	| link |
	^Array streamContents:[:s|
		link := self.
		[link == nil] whileFalse:[
			s nextPut: link.
			link := link nextLink.
		].
	].! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:29'!
fullPrintOn: aStream threshold: perCent time: totalTime reportOnly: aProcessOrNil
	| threshold |  
	threshold _ (perCent asFloat / 100 * tally) rounded.
	aStream nextPutAll: '**Tree**'; newLine.
	self
		rootPrintOn: aStream
		total: tally
		totalTime: totalTime
		threshold: threshold
		reportOnly: aProcessOrNil! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:31'!
printOn: aStream
	self subclassResponsibility! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:32'!
printOn: aStream total: total totalTime: totalTime
	self subclassResponsibility! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:32'!
rootPrintOn: aStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g |
		| sons |
		sons _ self sortCollectionFrom: g.
		aStream
			nextPutAll: '--------------------------------';
			newLine;
			nextPutAll: 'Process: ';
			nextPutAll: (g anyOne process browserPrintStringFull: false);
			newLine;
			nextPutAll: '--------------------------------';
			newLine.
		sons do: [ :son |
			son
				treePrintOn: aStream
				tabs: OrderedCollection new
				thisTab: '  '
				total: total
				totalTime: totalTime
				threshold: threshold].
		aStream newLine ]! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:32'!
sonsOver: threshold
	"Answer the sons with tallys over the given threshold"
	receivers ifNil:[^#()].
	^receivers asArray select:[:son | son tally > threshold].! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:51'!
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime threshold: threshold 
	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	self printOn: aStream total: total totalTime: totalTime.
	sons := self sonsOver: threshold.
	sons isEmpty ifFalse:[
		tabs addLast: myTab.
		sons := self sortCollectionFrom: sons.
		1 to: sons size do:[:i | 
			sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
			(sons at: i) treePrintOn: aStream
						tabs: tabs
						thisTab: sonTab
						total: total
						totalTime: totalTime
						threshold: threshold].
			tabs removeLast]! !

!QAbstractTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 20:32'!
addTallies: count
	"Bump this tally by the specified amount"
	tally _ tally + count! !

!QAbstractTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 20:41'!
tallyMethod: aMethod by: count
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !

!QAbstractTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 20:33'!
process
	^process! !

!QAbstractTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 20:33'!
process: aProcess
	process := aProcess! !

!QAbstractTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 20:33'!
tally
	"Answer the tally count for this node"
	^tally! !

!QAbstractTally methodsFor: 'misc' stamp: 'jmv 2/17/2015 20:34'!
sortCollectionFrom: aCollection
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]! !

!QAbstractTally methodsFor: 'testing' stamp: 'jmv 2/17/2015 20:37'!
isFake
	^false! !

!QAbstractTally methodsFor: 'initialization' stamp: 'jmv 2/17/2015 20:40'!
initialize
	tally := 0! !


!QFakeTally methodsFor: 'testing' stamp: 'jmv 2/17/2015 20:37'!
isFake
	^true! !

!QFakeTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 20:42'!
method
	^nil! !

!QFakeTally methodsFor: 'report' stamp: 'jmv 2/17/2015 20:45'!
printOn: aStream total: total totalTime: totalTime

	| percentage |
	percentage := tally asFloat / total * 100.0.
	percentage printOn: aStream fractionDigits: 1.
	aStream
		nextPutAll: '% (';
		nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
		nextPutAll: ' ms) '.

	aStream
		nextPutAll: '-----Primitives (called at an unknown point below here in the call tree)------';
		newLine ! !

