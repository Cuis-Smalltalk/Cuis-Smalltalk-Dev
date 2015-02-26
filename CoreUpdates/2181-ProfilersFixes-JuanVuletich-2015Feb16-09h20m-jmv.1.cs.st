'From Cuis 4.2 of 25 July 2013 [latest update: #2180] on 16 February 2015 at 9:20:54.03183 am'!

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 22:06'!
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

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 21:57'!
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
						tabs: (tabs size < self maxTabs 
								ifTrue: [tabs]
								ifFalse: [(tabs select: [:x | x = '[']) copyWith: '['])
						thisTab: sonTab
						total: total
						totalTime: totalTime
						threshold: threshold].
			tabs removeLast]! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 20:23'!
tally: context inProcess: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender leaf |

	"Add to this node if appropriate"
	"No es preciso subir mas
	Matchear proceso? clase?. Clase murio. Proceso no importa!!"
	context method == method ifTrue: [
		self addTallies: count.
		^self ].

	"No sender? Add new branch to the tree."
	sender _ context sender.
	"No entiendo cuando podria ocurrir...
	Creo que cuando ya no puedo subir mas en la recursion.
	Creo que normalmente solo pasa una vez, las siguientes las encuentra
	en la condicion de arriba."
	"no entiendo bien quien soy en este caso..."
	sender ifNil: [
		self addTallies: count.
		^self tallyLeafChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary)"
	"recursivamente intento encontrar o agregar el sender primero,
	y despues me agrego a mi"
	leaf _ self tally: sender inProcess: aProcess by: count.
	^leaf tallyLeafChild: context inProcess: aProcess by: count! !


!MessageTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 20:23'!
tally: context by: count
	"Explicitly tally the specified context and its stack."
	| sender |
	
	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	(sender _ context sender)ifNil: [
		^ (self bumpBy: count) tallyPath: context by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender by: count) tallyPath: context by: count! !

!MessageTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 20:23'!
tally: context in: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender |

	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	sender _ context sender.
	sender ifNil: [
		^ (self bumpBy: count) tallyPath: context in: aProcess by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender in: aProcess by: count) tallyPath: context in: aProcess by: count! !

