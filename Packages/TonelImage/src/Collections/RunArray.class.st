"
My instances provide space-efficient storage of data which tends to be constant over long runs of the possible indices. Essentially repeated values are stored singly and then associated with a "run" length that denotes the number of consecutive occurrences of the value.

My two important variables are
	runs	An array of how many elements are in each run
	values	An array of what the value is over those elements

The variables lastIndex, lastRun and lastOffset cache the last access
so that streaming through RunArrays is not an N-squared process.

Many complexities of access can be bypassed by using the method
	RunArray withStartStopAndValueDo:
"
Class {
	#name : 'RunArray',
	#superclass : 'SequenceableCollection',
	#instVars : [
		'runs',
		'values',
		'lastIndex',
		'lastRun',
		'lastOffset',
		'canJoinMessage'
	],
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'instance creation' }
RunArray class >> new [
	^self runs: #() values: #()
]

{ #category : 'instance creation' }
RunArray class >> new: size withAll: value  [
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)
]

{ #category : 'instance creation' }
RunArray class >> newFrom: aCollection  [
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection := self new.
	aCollection do: [:x | newCollection addLast: x].
	^newCollection

"	RunArray newFrom: {1. 2. 2. 3}
	{1. $a. $a. 3} as: RunArray
	({1. $a. $a. 3} as: RunArray) values
"
]

{ #category : 'instance creation' }
RunArray class >> readFrom: aStream [
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size := aStream nextUint16BigEndian: true.
	runs := Array new: size.
	values := Array new: size.
	1 to: size do: [ :x |
		runs at: x put: (aStream nextUint16BigEndian: true).
		values at: x put: (aStream nextUint16BigEndian: true) ].
	^ self runs: runs values: values
]

{ #category : 'instance creation' }
RunArray class >> runs: newRuns values: newValues  [
	"Answer a new instance of RunArray with runs and values specified
	by the arguments."

	| instance |
	instance := self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance
]

{ #category : 'copying' }
RunArray >> , aRunArray  [
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns answer |
	(aRunArray isMemberOf: RunArray)
		ifFalse: [
			new := self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	self isEmpty ifTrue: [^aRunArray copy].
	aRunArray isEmpty ifTrue: [^self copy].
	(self canJoin: (values at: values size) and: (aRunArray values at: 1))
		ifFalse: [ 
			answer := RunArray
				runs: runs , aRunArray runs
				values: values , aRunArray values.
			answer canJoinMessage: canJoinMessage.
			^answer ].
	newRuns := runs
					copyReplaceFrom: runs size
					to: runs size
					with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	answer := RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values).
	answer canJoinMessage: canJoinMessage.
	^answer
]

{ #category : 'accessing' }
RunArray >> = otherArray  [
	self == otherArray ifTrue: [ ^ true ].
	
	self species == otherArray species ifFalse: [^ false].

	"Test if all my elements are equal to those of otherArray"
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
 	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]
]

{ #category : 'adding' }
RunArray >> add: newObject  [
	"Adding to an Interval is not allowed."

	self shouldNotImplement
]

{ #category : 'adding' }
RunArray >> addFirst: value [
	"Add value as the first element of the receiver."
	lastIndex := nil.  "flush access cache"
	(runs size ~= 0 and: [ self canJoin: values first and: value ])
		ifTrue: [
			runs at: 1 put: runs first + 1]
		ifFalse: [
			runs := {1}, runs.
			values := {value}, values]
]

{ #category : 'adding' }
RunArray >> addLast: value [
	"Add value as the last element of the receiver."
	lastIndex := nil.		"flush access cache"
	(runs size ~= 0 and: [ self canJoin: values last and: value ])
		ifTrue: [
			runs at: runs size put: runs last + 1 ]
		ifFalse: [
			runs := runs copyWith: 1.
			values := values copyWith: value ]
]

{ #category : 'accessing' }
RunArray >> at: index [

	self at: index setRunOffsetAndValue: [:run :offset :value | ^value]
]

{ #category : 'private' }
RunArray >> at: index setRunOffsetAndValue: aBlock  [
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit := runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue:  "cache not loaded, or beyond index - start over"
			[run := 1.
			offset := index-1]
		ifFalse:  "cache loaded and before index - start at cache"
			[run := lastRun.
			offset := lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset := offset - (runs at: run).
			run := run + 1].
	lastIndex := index.  "Load cache for next access"
	lastRun := run.
	lastOffset := offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run := run - 1.
			offset := offset + (runs at: run)].
	^aBlock
		value: run	"an index into runs and values"
		value: offset	"zero-based offset from beginning of this run"
		value: (values at: run)	"value for this run"
]

{ #category : 'private' }
RunArray >> basicReplaceAttributesFrom: start to: stop with: replacement [
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	| answer |
	answer := self
		copyReplaceFrom: start
		to: stop
		with: replacement.
	answer coalesce.
	^ answer
]

{ #category : 'private' }
RunArray >> canJoin: aValue and: anotherValue [
	^ canJoinMessage
		ifNil: [ aValue = anotherValue ]
		ifNotNil: [
			canJoinMessage valueWithArguments: {aValue. anotherValue} ]
]

{ #category : 'accessing' }
RunArray >> canJoinMessage: aMessageSend [
	canJoinMessage := aMessageSend
]

{ #category : 'adding' }
RunArray >> coalesce [
	"Try to combine adjacent runs"
	| ind |
	ind := 2.
	[ ind > values size ] whileFalse: [
		(self canJoin: (values at: ind-1) and: (values at: ind))
			ifFalse: [ ind := ind + 1 ]
			ifTrue: [		"two are the same, combine them"
				values := values copyReplaceFrom: ind to: ind with: #().
				runs at: ind-1 put: (runs at: ind-1) + (runs at: ind).
				runs := runs copyReplaceFrom: ind to: ind with: #() ]]
]

{ #category : 'copying' }
RunArray >> copy [

	^self copyFrom: 1 to: self size
]

{ #category : 'copying' }
RunArray >> copyFrom: start to: stop [
	| newRuns run1 run2 offset1 offset2 answer | 
	stop < start ifTrue: [
		answer := RunArray new.
		answer canJoinMessage: canJoinMessage.
		^answer ].
	self at: start setRunOffsetAndValue: [ :r :o :value1 |
		run1 := r.
		offset1 := o. 
		value1 ].
	self at: stop setRunOffsetAndValue: [ :r :o :value2 |
		run2 := r.
		offset2 := o.
		value2].
	run1 = run2
		ifTrue: [
			newRuns := Array with: offset2 - offset1 + 1]
		ifFalse: [
			newRuns := runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1 ].
	answer := RunArray runs: newRuns values: (values copyFrom: run1 to: run2).
	answer canJoinMessage: canJoinMessage.
	^answer
]

{ #category : 'copying' }
RunArray >> copyReplaceFrom: start to: stop with: replacement  [
	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)
]

{ #category : 'private' }
RunArray >> find: attribute [
	"Return the first interval over which this attribute applies"
	| begin end |
	begin := 0.
	self withStartStopAndValueDo: [ :start :stop :attributes |
		(attributes includes: attribute)
			ifTrue: [
				begin = 0 ifTrue: [begin := start].
				end := stop]
			ifFalse: [
				begin > 0 ifTrue: [^ begin to: end]]].
	begin > 0 ifTrue: [^ begin to: end].
	^ nil
]

{ #category : 'accessing' }
RunArray >> first [
	^values at: 1
]

{ #category : 'testing' }
RunArray >> is: aSymbol [
	^aSymbol == #RunArray or: [ super is: aSymbol ]
]

{ #category : 'accessing' }
RunArray >> isEmpty [
	^runs size = 0
]

{ #category : 'accessing' }
RunArray >> last [
	^values at: values size
]

{ #category : 'private' }
RunArray >> mapValues: mapBlock [
	"NOTE: only meaningful to an entire set of runs"
	values := values collect: mapBlock
]

{ #category : 'printing' }
RunArray >> printOn: aStream [
	self printNameOn: aStream.
	aStream
		nextPutAll: ' runs: ';
		print: runs;
		nextPutAll: ' values: ';
		print: values
]

{ #category : 'adding' }
RunArray >> rangeOf: attr startingAt: startPos [
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.
Note that an attribute may span several adjancent runs. "

	self at: startPos 
		setRunOffsetAndValue: 
            [:run :offset :value | 
               ^(value includes: attr)
                  ifFalse: [startPos to: startPos - 1]
                  ifTrue:
                    [ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |
                     lastRelevantPosition := startPos - offset + (runs at: run) - 1.
                     firstRelevantPosition := startPos - offset.
                     idxOfCandidateRun := run + 1.
                     [idxOfCandidateRun <= runs size 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun + 1]. 
                     idxOfCandidateRun := run - 1.
                     [idxOfCandidateRun >= 1 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun - 1]. 
 
                    firstRelevantPosition to: lastRelevantPosition]
		  ]
]

{ #category : 'converting' }
RunArray >> reversed [

  ^self class runs: runs reversed values: values reversed
]

{ #category : 'accessing' }
RunArray >> runLengthAt: index  [
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]
]

{ #category : 'private' }
RunArray >> runs [
	^runs
]

{ #category : 'enumerating' }
RunArray >> runsAndValuesDo: aBlock [
	"Evaluate aBlock with run lengths and values from the receiver"
	^runs with: values do: aBlock.
]

{ #category : 'enumerating' }
RunArray >> runsFrom: start to: stop do: aBlock [
	"Evaluate aBlock with all existing runs in the range from start to stop"
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue: [ :firstRun :offset :firstValue|
		| run value index |
		run := firstRun.
		value := firstValue.
		index := start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run := run + 1.
			value := values at: run.
			index := index + (runs at: run)]].

]

{ #category : 'private' }
RunArray >> setRuns: newRuns setValues: newValues [
	lastIndex := nil.  "flush access cache"
	runs := newRuns asArray.
	values := newValues asArray.
]

{ #category : 'accessing' }
RunArray >> size [
	| size |
	size := 0.
	1 to: runs size do: [:i | size := size + (runs at: i)].
	^size
]

{ #category : 'printing' }
RunArray >> storeOn: aStream [
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'statistics' }
RunArray >> sum: aBlock ifEmpty: emptyBlockOrValue [
        "
        (RunArray new: 4096 withAll: 3) sum =  12288.
        ([(RunArray new) sum] on: Error do: [:ex | ex messageText] ) = 'this collection is empty'.
        "
	| initial sum |
	self isEmpty ifTrue: [^ emptyBlockOrValue value].
        initial := aBlock value: self first.
        sum := initial.
        self runsAndValuesDo: [:run :value | sum := sum + (run * (aBlock value: value))].
        ^ sum - initial
]

{ #category : 'accessing' }
RunArray >> values [
	"Answer the values in the receiver."
	^values
]

{ #category : 'accessing' }
RunArray >> withStartStopAndValueDo: aBlock [
	| start stop |
	start := 1.
	runs with: values do:
		[:len : val | stop := start + len - 1.
		aBlock value: start value: stop value: val.
		start := stop + 1]
		
]

{ #category : 'printing' }
RunArray >> writeOn: aStream [

	aStream nextUint16Put: runs size bigEndian: true.
	1 to: runs size do: [ :x |
		aStream nextUint16Put: (runs at: x) bigEndian: true.
		aStream nextUint16Put: (values at: x) bigEndian: true ]
]
