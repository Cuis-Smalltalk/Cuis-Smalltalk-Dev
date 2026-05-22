"
I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: Morph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally

Also try:
'MemoryAnalysis.txt' asFileEntry forceWriteStreamDo: [ :stream |
	SpaceTally new printSpaceAnalysis: 1 on: stream ]
"
Class {
	#name : 'SpaceTally',
	#superclass : 'Object',
	#instVars : [
		'results'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'fileOut' }
SpaceTally >> compareTallyIn: beforeFileName to: afterFileName [
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys before after diff |
	beforeDict := Dictionary new.
	beforeFileName asFileEntry readStreamDo: [ :s |
		[s atEnd ] whileFalse: [
			a := Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].

	afterDict := Dictionary new.
	afterFileName asFileEntry readStreamDo: [ :s |
		[ s atEnd ] whileFalse: [
			a := Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer := String streamContents: [ :stream |
		allKeys := (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.
		allKeys do: [ :each |
			before := beforeDict at: each ifAbsent: [#(0 0 0)].
			after := afterDict at: each ifAbsent: [#(0 0 0)].
			diff := before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
			diff = #(0 0 0) ifFalse: [
				stream nextPutAll: each, '  ', diff printString; newLine.
			].
		]].

	TextModel new contents: answer; openLabel: 'space diffs'
]

{ #category : 'class analysis' }
SpaceTally >> computeSpaceUsage [

	| entry c |
	1 to: results size do: [:i |
		entry := results at: i.
		c := Smalltalk at: entry analyzedClassName.
		entry codeSize: c spaceUsed.
		entry instanceCount: c instanceCount.
		entry spaceForInstances: (self spaceForInstancesOf: c).
		Smalltalk garbageCollectMost].
	

]

{ #category : 'class analysis' }
SpaceTally >> preAllocateResultsFor: classes [

	results := OrderedCollection new.
	classes do: [:cl | results add: (SpaceTallyItem analyzedClassName: cl name)].
	results := results asArray.

]

{ #category : 'fileOut' }
SpaceTally >> printSpaceAnalysis	 [
	"
	SpaceTally new printSpaceAnalysis
	"

	DirectoryEntry logsDirectory // 'STspace.text' writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			self printSpaceAnalysis: 1 on: fileStream ]].
]

{ #category : 'fileOut' }
SpaceTally >> printSpaceAnalysis: threshold on: aStream [
	"Warning!
	This is most likely wrong. It was written for 32 bit V3 images.
	Please take a look at #spaceForInstancesOf:
	Review both methods, and any other one that might be related.
	"
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace := cl spaceUsed.
				barBlock value: (n := n+1).
				Smalltalk garbageCollectMost.
				instCount := cl instanceCount.
				instSpace := 8 * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!"
				cl isVariable
					ifTrue: [
						eltSize := cl bytesPerBasicElement.
						cl allInstancesDo: [ :x |
							instSpace := instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace := instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace := totalCodeSpace + codeSpace.
				totalInstCount := totalInstCount + instCount.
				totalInstSpace := totalInstSpace + instSpace]].
	totalPercent := 0.0.

	aStream writeSystemTimeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results := (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent := s spaceForInstances*100.0/totalInstSpace.
		totalPercent := totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil
]

{ #category : 'fileOut' }
SpaceTally >> printSpaceDifferenceFrom: fileName1 to: fileName2 [
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		'STspace.text1' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
			--- do something that uses space here ---
		'STspace.text2' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| coll1 coll2 item |
	coll1 := OrderedCollection new.
	DirectoryEntry logsDirectory // fileName1 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [coll1 add: stream crLfNextLine]].
	
	coll2 := OrderedCollection new.
	DirectoryEntry logsDirectory // fileName2 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [
			item := stream crLfNextLine.
			((coll1 includes: item) and: [(item endsWith: 'percent') not])
				ifTrue: [coll1 remove: item]
				ifFalse: [coll2 add: item]]].

	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.

]

{ #category : 'accessing' }
SpaceTally >> results [

	^ results
]

{ #category : 'fileOut' }
SpaceTally >> saveTo: aFileName [
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	DirectoryEntry logsDirectory // aFileName forceWriteStreamDo: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asPlainString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]
]

{ #category : 'instance size' }
SpaceTally >> spaceForInstancesOf: aClass [
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instances total |
	instances := aClass allInstances.
	instances isEmpty ifTrue: [^#(0 0)].
	total := 0.
	aClass isVariable
		ifTrue:
			[instances do:
				[:i| total := total + (aClass byteSizeOfInstanceOfSize: i basicSize)]]
		ifFalse:
			[total := instances size * aClass byteSizeOfInstance].
	^{ total. instances size }
]

{ #category : 'class analysis' }
SpaceTally >> spaceTally: classes  [
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables. "
	"
	SpaceTally new spaceTally: (Array with: TextModelMorph with: Point)
	"
	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
		 sort: [ :a :b | a spaceForInstances > b spaceForInstances ];
		 yourself
]

{ #category : 'class analysis' }
SpaceTally >> systemWideSpaceTally [
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"(SpaceTally new systemWideSpaceTally asSortedCollection: [:a :b | a last > b last]) asArray"

	self preAllocateResultsFor: Smalltalk allClasses.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results

]
