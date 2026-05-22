Class {
	#name : 'QAbstractTally',
	#superclass : 'Link',
	#instVars : [
		'process',
		'receivers',
		'tally'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'tallying' }
QAbstractTally >> addTallies: count [
	"Bump this tally by the specified amount"
	tally := tally + count
]

{ #category : 'converting' }
QAbstractTally >> asArray [
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
	].
]

{ #category : 'report' }
QAbstractTally >> fullPrintOn: textStream linesOn: linesStream talliesOn: talliesStream threshold: perCent time: totalTime reportOnly: aProcessOrNil [
	| threshold line |  
	threshold := (perCent asFloat / 100 * tally) rounded.
	line := '**Tree**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	self
		rootPrintOn: textStream
		linesOn: linesStream
		talliesOn: talliesStream 
		total: tally
		totalTime: totalTime
		threshold: threshold
		reportOnly: aProcessOrNil
]

{ #category : 'initialization' }
QAbstractTally >> initialize [
	tally := 0
]

{ #category : 'report' }
QAbstractTally >> printOn: aStream [
	self subclassResponsibility
]

{ #category : 'report' }
QAbstractTally >> printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally [
	self subclassResponsibility
]

{ #category : 'accessing' }
QAbstractTally >> process [
	^process
]

{ #category : 'accessing' }
QAbstractTally >> process: aProcess [
	process := aProcess
]

{ #category : 'report' }
QAbstractTally >> rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil [

	| groups dashes line tabsAndTreeLines thisTab |
	groups := (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g | | sons |
		sons := self sortCollectionFrom: g.
		dashes := '--------------------------------'.
		line := 'Process: ', (g anyOne process browserPrintStringFull: false).
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
			tabsAndTreeLines := OrderedCollection new.
			thisTab := '  '.
			son
				treeDFSDo: [ :eachTally :parentTally |
					eachTally
						printOn: textStream
						linesOn: linesStream talliesOn: talliesStream
						tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally.
					tabsAndTreeLines addLast: thisTab.
					thisTab := (eachTally sonsOver: threshold) size > 1 ifTrue: ['  |'] ifFalse: ['  '].
				]
				afterChildrenDo: [ :eachTally | tabsAndTreeLines removeLast ]
				threshold: threshold
				parent: self.
			].
		textStream newLine ]
]

{ #category : 'report' }
QAbstractTally >> sonsOver: threshold [
	"Answer the sons with tallys over the given threshold"
	receivers ifNil:[^#()].
	^receivers asArray select:[:son | son tally >= threshold].
]

{ #category : 'misc' }
QAbstractTally >> sortCollectionFrom: aCollection [
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]
]

{ #category : 'accessing' }
QAbstractTally >> tally [
	"Answer the tally count for this node"
	^tally
]

{ #category : 'tallying' }
QAbstractTally >> tallyPrimInMethod: aMethod by: count [
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		(node method == aMethod and: [ node wasInPrimitive ]) ifTrue: [
			^node addTallies: count ].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node wasInPrimitive: true.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count
]

{ #category : 'report' }
QAbstractTally >> treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: parentTally [
	"Deep First Search"
	| sons |
	aBlock value: self value: parentTally.
	sons := self sonsOver: threshold.
	sons isEmpty ifFalse: [
		sons := self sortCollectionFrom: sons.
		1 to: sons size do: [ :i |
			(sons at: i) treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: self ]].
	anotherBlock value: self
]
