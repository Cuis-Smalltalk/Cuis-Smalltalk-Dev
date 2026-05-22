"
My instances observe and report the amount of time spent in methods.

MessageTally provides two different strategies available for profiling:

* tallySends: and friends use the interpreter simulator to run the block, recording every method call. It gives you accurate counts of how many times methods get called, and by exactly which route. If you're debugging, or trying to figure out if a given method is getting called too many times, this is your tool.

* spyOn: and friends use a high-priority Process to interrupt the block or process being spied on at periodic intervals. The interrupted call stack is then examined for caller information. This was moved to AndreasSystemProfiler.
"
Class {
	#name : 'MessageTally',
	#superclass : 'Magnitude',
	#instVars : [
		'class',
		'method',
		'tally',
		'receivers',
		'senders'
	],
	#classVars : [
		'DefaultPollPeriod',
		'Timer'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'spying' }
MessageTally class >> spyOn: aBlock [
	"
	Spy on aBlock, in the current process. Do not include statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]]
	"
	"
	MessageTally spyOn:[10000 timesRepeat: [3.14159 printString]]
	"

	^AndreasSystemProfiler spyOn: aBlock
]

{ #category : 'spying' }
MessageTally class >> tallySends: aBlock [
	"
	MessageTally tallySends: [3.14159 printString]
	"

	^ self tallySendsTo: nil inBlock: aBlock
]

{ #category : 'spying' }
MessageTally class >> tallySendsTo: receiver inBlock: aBlock [
	"
	MessageTally tallySends: [3.14159 printString]
	"
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."
	| prev tallies startTime totalTime |
	startTime := Time localMillisecondClock.
	tallies := self new class: aBlock receiver class method: aBlock method.
	prev := aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					(receiver == nil or: [ current receiver == receiver ])
						ifTrue: [ tallies tally: current by: 1 ]].
				prev := current]].

	totalTime := Time localMillisecondClock - startTime / 1000.0.
	UISupervisor whenUIinSafeState: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s
					nextPutAll: 'This simulation took ';
					nextPutAll: totalTime printString;
					nextPutAll: ' seconds.';
					newLine.
				tallies fullPrintExactOn: s ]))
			label: 'Spy Results'
			wrap: false ]
]

{ #category : 'comparing' }
MessageTally >> < aMessageTally  [
	^tally > aMessageTally tally
]

{ #category : 'comparing' }
MessageTally >> = aMessageTally [

	self == aMessageTally ifTrue: [ ^ true ].
	self species == aMessageTally species ifFalse: [^ false].
	^ aMessageTally method == method
]

{ #category : 'comparing' }
MessageTally >> > aMessageTally  [
	^tally < aMessageTally tally
]

{ #category : 'collecting leaves' }
MessageTally >> bump: hitCount [
	tally := tally + hitCount
]

{ #category : 'collecting leaves' }
MessageTally >> bump: hitCount fromSender: senderTally [
	"Add this hitCount to the total, and include a reference to the
	sender responsible for the increment"
	self bump: hitCount.
	senders ifNil: [senders := OrderedCollection new].
	senderTally == nil
		ifFalse: [senders add: (senderTally copyWithTally: hitCount)]
]

{ #category : 'tallying' }
MessageTally >> bumpBy: count [

	tally := tally + count
]

{ #category : 'private' }
MessageTally >> class: aClass method: aMethod  [
	class := aClass.
	method := aMethod.
	tally := 0.
	receivers := Array new: 0
]

{ #category : 'private' }
MessageTally >> copyWithTally: hitCount [

	^ (MessageTally new class: class method: method)
		bump: hitCount
]

{ #category : 'printing' }
MessageTally >> fullPrintExactOn: aStream [

	aStream nextPutAll: '**Tree**'; newLine.
	self
		treePrintExactOn: aStream
		tabs: OrderedCollection new
		thisTab: ''.
	aStream nextPut: Character newPage; newLine.
	aStream nextPutAll: '**Leaves**'; newLine.
	self leavesPrintExactOn: aStream
]

{ #category : 'comparing' }
MessageTally >> hash [
	"Hash is reimplemented because = is implemented."

	^method hash
]

{ #category : 'collecting leaves' }
MessageTally >> into: leafDict fromSender: senderTally [

	| leafNode |
	leafNode := leafDict at: method
		ifAbsent: [
			leafDict at: method
				put: (MessageTally new class: class method: method)].
	leafNode bump: tally fromSender: senderTally
]

{ #category : 'comparing' }
MessageTally >> isPrimitives [
	"Detect pseudo node used to carry tally of local hits"
	^ receivers == nil
]

{ #category : 'collecting leaves' }
MessageTally >> leavesInto: leafDict fromSender: senderTally [

	| rcvrs |
	rcvrs := self sonsOver: 0.
	rcvrs isEmpty
		ifTrue: [ self into: leafDict fromSender: senderTally ]
		ifFalse: [
			rcvrs do: [ :node |
				node isPrimitives
					ifTrue: [ node leavesInto: leafDict fromSender: senderTally ]
					ifFalse: [ node leavesInto: leafDict fromSender: self ]]]
]

{ #category : 'printing' }
MessageTally >> leavesPrintExactOn: aStream [

	| dict |
	dict := IdentityDictionary new.
	self leavesInto: dict fromSender: nil.
	dict asArray sort
		do: [ :node |
			node printOn: aStream.
			node printSenderCountsOn: aStream ]
]

{ #category : 'private' }
MessageTally >> method [
	^method
]

{ #category : 'private' }
MessageTally >> primitives: anInteger  [
	tally := anInteger.
	receivers := nil
]

{ #category : 'printing' }
MessageTally >> printOn: aStream [

	| myTally aSelector aClass |
	myTally := tally.
	receivers
		ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
	aStream
		print: myTally;
		space.
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass == class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]
]

{ #category : 'printing' }
MessageTally >> printSenderCountsOn: aStream [

	| mergedSenders |
	mergedSenders := IdentityDictionary new.
	senders do: [ :node | | mergedNode |
		mergedNode := mergedSenders at: node method ifAbsent: nil .
		mergedNode
			ifNil: [ mergedSenders at: node method put: node ]
			ifNotNil: [ mergedNode bump: node tally ]].
	mergedSenders asArray sort do: [ :node | 
		10 to: node tally printString size by: -1 do: [ :i | aStream space ].
		node printOn: aStream ]
]

{ #category : 'comparing' }
MessageTally >> sonsOver: threshold [

	| hereTally sons |
	(receivers isNil or: [ receivers isEmpty ]) ifTrue: [ ^#() ].
	hereTally := tally.
	sons := receivers select: [ :son | "subtract subNode tallies for primitive hits here"
		hereTally := hereTally - son tally.
		son tally > threshold ].
	hereTally > threshold
		ifTrue: [
			| last |
			last := MessageTally new class: class method: method.
			^sons copyWith: (last primitives: hereTally)].
	^sons
]

{ #category : 'private' }
MessageTally >> tally [
	^tally
]

{ #category : 'tallying' }
MessageTally >> tally: context by: count [
	"Explicitly tally the specified context and its stack."
	| sender |
	
	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	(sender := context sender)ifNil: [
		^ (self bumpBy: count) tallyPath: context by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender by: count) tallyPath: context by: count
]

{ #category : 'tallying' }
MessageTally >> tallyPath: context by: count [

	| aMethod path |
	aMethod := context method.
	
	"Find the correct child (if there)"
	receivers do: [ :oldTally | 
		oldTally method == aMethod ifTrue: [path := oldTally]].
	
	"Add new child if needed"
	path ifNil: [
		path := MessageTally new class: context receiver class method: aMethod.
		receivers := receivers copyWith: path].
	
	^ path bumpBy: count
]

{ #category : 'printing' }
MessageTally >> treePrintExactOn: aStream tabs: tabs thisTab: myTab [

	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue: [
			self printOn: aStream ].
	sons := receivers.
	sons isEmpty 
		ifFalse: [
			tabs addLast: myTab.
			sons := sons asArray sort.
			(1 to: sons size) do: [ :i | 
					sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
					(sons at: i)
						treePrintExactOn: aStream
						tabs: tabs
						thisTab: sonTab ].
			tabs removeLast]
]
