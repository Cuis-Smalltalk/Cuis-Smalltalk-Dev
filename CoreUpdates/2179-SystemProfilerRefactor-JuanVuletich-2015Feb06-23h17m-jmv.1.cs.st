'From Cuis 4.2 of 25 July 2013 [latest update: #2178] on 7 February 2015 at 12:02:06.614406 am'!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally senders '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QSystemTally commentStamp: '<historical>' prior: 0!
Tally for assembling system profiles. It's a subclass of Link so receivers actually holds the first element in a linked list. This is good for making the profiling overhead low and relatively constant (no OrderedCollections to grow).!


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/6/2015 23:39'!
addToLeafReportDict: leafDict
	"Leaves Dict has no more than one entry for each method,
	even if this method appeared in many leves in the tree."
	| leafNode |
	leafNode _ leafDict
		at: method
		ifAbsentPut: [
			(QSystemTally new class: class method: method)
				process: process ].
	leafNode addTallies: tally! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/5/2015 23:02'!
leavesIntoLeafReportDict: leafDict
	| callees |
	callees _ self sonsOver: 0.
	callees size = 0
		ifTrue: [
			"We are the leaf"
			self addToLeafReportDict: leafDict ]
		ifFalse: [
			callees do: [ :node |
				node leavesIntoLeafReportDict: leafDict ]]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 00:00'!
printOn: aStream total: total totalTime: totalTime

	| className aSelector aClass percentage |
	percentage := tally asFloat / total * 100.0.
	percentage printOn: aStream fractionDigits: 1.
	aStream
		nextPutAll: '% (';
		nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
		nextPutAll: ' ms) '.

	aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
	className := aClass name contractTo: self maxClassNameSize.
	aStream
		nextPutAll: class name;
		nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
		nextPutAll: (aSelector contractTo: self maxClassPlusSelectorSize - className size);
		newLine ! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 00:01'!
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime threshold: threshold 
	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue:[self printOn: aStream total: total totalTime: totalTime ].
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

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/5/2015 22:16'!
addTallies: count
	"Bump this tally by the specified amount"
	tally _ tally + count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/6/2015 23:36'!
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
	sender _ context home sender.
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

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/6/2015 23:54'!
tallyLeafChild: context inProcess: aProcess by: count
	"Find a suitable child, or add a new one if needed."
	| aMethod node |
	aMethod := context method.
	node _ receivers.
	[node == nil] whileFalse: [
		(node method == aMethod and: [ node process == aProcess ]) ifTrue: [
			^node addTallies: count ].
		node _ node nextLink ].
	"Entender en que caso pudo fallar arriba por el proceso, y agregarse aca.
	O sea, en que caso habra 2 nodos hermanos iguales salvo el proceso.
	mhhhhhh"
	node _ QSystemTally new class: context receiver class method: aMethod.
	node process: aProcess.
	node nextLink: receivers.
	receivers _ node.
	^node addTallies: count! !

!QSystemTally methodsFor: 'misc' stamp: 'jmv 2/5/2015 21:48'!
sortCollectionFrom: aCollection
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/6/2015 23:51'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop method leaf |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Time primHighResClock.
	[ true ]
		whileTrue: [ 
			tallyStart := tallyStop.
			tallyStart := Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyStop := Time primHighResClock.
			tallyTicks := tallyStop - tallyStart.
			totalTicks := totalTicks + tallyTicks.
			process := Smalltalk profileSample.
			method := Smalltalk profilePrimitive.
			totalTally := totalTally + 1.
			process
				ifNotNil: [ 
					leaf := tallyRoot
						tally: (process suspendedContext ifNil: [ thisContext ])
						inProcess: process
						by: tallyTicks.
					"por que cadorna hace falta esto????
					pero seguro que si hace falta...
					en que casos el method no es process suspendedContext method?"
					"mhhh en
						AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
						ni siquiera es el sender, y aparece cualquier banana. es preciso ver la doc
						
						
						p. ej
  |  2.9% (7 ms) (Number>>#raisedToInteger:)
  |    2.2% (5 ms) (Float>>#timesTwoPower: )

raisedToInteger: NO envia timesTwoPower:

					"
					method ifNotNil: [ leaf tallyMethod: method by: tallyTicks ] ] ]! !


!QSystemTally methodsFor: 'converting' stamp: 'jmv 2/5/2015 21:49'!
asArray
	"esto es feo. normalmente los link esta en un LinkedList.
	Estamos de acuerdo que usualmente es el pedo... pero, es el elemento una coleccion o no????"
	| link |
	^Array streamContents:[:s|
		link := self.
		[link == nil] whileFalse:[
			s nextPut: link.
			link := link nextLink.
		].
	].! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 00:00'!
leavesPrintOn: aStream threshold: threshold time: totalTime
	"Build and print a report on leaves of the tree, but without tree structure,
	condensing in one appareance the (possibly) many times the method appears
	as a leaf on the call tree."
	| dict |
	dict _ IdentityDictionary new: 100.
	self leavesIntoLeafReportDict: dict.
	((self sortCollectionFrom: dict)
		select: [:node | node tally > threshold]) do: [:node |
			node printOn: aStream total: tally totalTime: totalTime ]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/6/2015 23:58'!
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
			nextPutAll: g anyOne process browserPrintString;
			newLine;
			nextPutAll: '--------------------------------';
			newLine.
		sons do: [ :son |
			son
				treePrintOn: aStream
				tabs: OrderedCollection new
				thisTab: ''
				total: total
				totalTime: totalTime
				threshold: threshold].
		aStream newLine ]! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/6/2015 23:55'!
tallyMethod: aMethod by: count
	"Called explicitly and needs to decrement receiver's tally count"
	"hace falta??????
	Se parece demasiado a #tallyLeafChild:inProcess:by:
	precisaria el proceso?????
	"
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !

!methodRemoval: QSystemTally #asSortedCollection:!
QSystemTally removeSelector: #asSortedCollection:!
!methodRemoval: QSystemTally #bump:fromSender:!
QSystemTally removeSelector: #bump:fromSender:!
!methodRemoval: QSystemTally #bumpBy:!
QSystemTally removeSelector: #bumpBy:!
!methodRemoval: QSystemTally #copyWithTally:!
QSystemTally removeSelector: #copyWithTally:!
!methodRemoval: QSystemTally #into:fromSender:!
QSystemTally removeSelector: #into:fromSender:!
!methodRemoval: QSystemTally #intoX:!
QSystemTally removeSelector: #intoX:!
!methodRemoval: QSystemTally #isPrimitives!
QSystemTally removeSelector: #isPrimitives!
!methodRemoval: QSystemTally #leavesInto:!
QSystemTally removeSelector: #leavesInto:!
!methodRemoval: QSystemTally #leavesInto:fromSender:!
QSystemTally removeSelector: #leavesInto:fromSender:!
!methodRemoval: QSystemTally #primitives:!
QSystemTally removeSelector: #primitives:!
!methodRemoval: QSystemTally #printOn:total:totalTime:tallyExact:!
QSystemTally removeSelector: #printOn:total:totalTime:tallyExact:!
!methodRemoval: QSystemTally #tally:in:by:!
QSystemTally removeSelector: #tally:in:by:!
!methodRemoval: QSystemTally #tallyPath:in:by:!
QSystemTally removeSelector: #tallyPath:in:by:!
!methodRemoval: QSystemTally #treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:!
QSystemTally removeSelector: #treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QSystemTally reorganize!
('converting' asArray)
('report' addToLeafReportDict: fullPrintOn:threshold:time:reportOnly: leavesIntoLeafReportDict: leavesPrintOn:threshold:time: printOn: printOn:total:totalTime: rootPrintOn:total:totalTime:threshold:reportOnly: sonsOver: treePrintOn:tabs:thisTab:total:totalTime:threshold:)
('tallying' addTallies: tally:inProcess:by: tallyLeafChild:inProcess:by: tallyMethod:by:)
('accessing' maxClassNameSize maxClassPlusSelectorSize maxTabs method process process: tally)
('initialization' class:method:)
('misc' sortCollectionFrom:)
!

