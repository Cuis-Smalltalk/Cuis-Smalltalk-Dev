'From Cuis 6.0 [latest update: #6009] on 9 September 2023 at 8:18:56 pm'!
!classDefinition: #SymbolSet category: #'Kernel-Text'!
Object subclass: #SymbolSet
	instanceVariableNames: 'buckets bucketCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 20:06:40'!
do: aBlock

	buckets do:
		[:eachBucket |
			eachBucket do:
				[:eachSymbol |
					eachSymbol notNil
						ifTrue: [aBlock value: eachSymbol]
				]
		]! !

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 20:06:45'!
do: aBlock after: aSymbol

	| found |
	found := false.
	self do:
		[:each |
			found ifTrue: [aBlock value: each].
			each = aSymbol ifTrue: [found := true]
		]! !

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 19:55:07'!
intern: aString

	| hash lookup |
	hash := aString hash.
	lookup := self basicLookup: aString withHash: hash.
	lookup notNil ifTrue: [^lookup].
	^self basicIntern: aString withHash: hash.! !

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 19:54:52'!
lookup: aString

	^self basicLookup: aString withHash: aString hash! !

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 20:02:03'!
rehash

	| newBuckets |
	newBuckets := self newBuckets: self newBucketCount sized: self initialBucketSize.
	self buckets: newBuckets.
	Symbol allInstances do:
		[:symbol | self basicInternNew: symbol withHash: symbol hash]! !

!SymbolSet methodsFor: 'lookup' stamp: 'sqr 9/9/2023 19:52:48'!
size

	^buckets inject: 0 into: [:t :x | x count: [:s | s notNil] :: + t]! !

!SymbolSet methodsFor: 'private' stamp: 'sqr 9/9/2023 20:01:53'!
initialize

	| newBuckets |
	super initialize.
	newBuckets := self newBuckets: self initialBuckets sized: self initialBucketSize.
	self buckets: newBuckets! !

!SymbolSet methodsFor: 'private' stamp: 'sqr 12/30/2021 06:51:22'!
newBucketSized: anInteger

	^WeakArray new: anInteger! !

!SymbolSet methodsFor: 'private' stamp: 'sqr 12/30/2021 07:26:06'!
newBuckets: newBucketCount sized: bucketSize

	| answer |
	answer := Array new: newBucketCount.
	1 to: answer size do:
		[:k |
			| newBucket |
			newBucket := self newBucketSized: bucketSize.
			answer at: k put: newBucket
		].
	^answer! !

!SymbolSet methodsFor: 'private - accessing' stamp: 'sqr 9/9/2023 20:01:47'!
buckets: anArray

	buckets := anArray.
	bucketCount := anArray size.! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 9/9/2023 19:50:07'!
basicAdd: aSymbol withHash: anInteger

	| bucketIndex bucket bucketSize newBucket |
	bucketIndex := self bucketIndexFor: anInteger.
	bucket := buckets at: bucketIndex.
	bucketSize := bucket size.
	1 to: bucket size do:
		[:k |
			(bucket at: k) == nil ifTrue:
				[
					bucket at: k put: aSymbol.
					^self
				]
		].
	newBucket := self newBucketSized: bucketSize + self initialBucketGrowth.
	newBucket replaceFrom: 1 to: bucketSize with: bucket startingAt: 1.
	newBucket at: bucketSize + 1 put: aSymbol.
	buckets at: bucketIndex put: newBucket! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 12/30/2021 07:30:42'!
basicIntern: aString withHash: anInteger
	"Look up again due to possible race conditions"

	| answer |
	answer := self basicLookup: aString withHash: anInteger.
	answer notNil ifTrue: [^answer].
	^self basicInternNew: aString withHash: anInteger! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 9/9/2023 20:00:28'!
basicInternNew: aStringOrSymbol withHash: anInteger

	| interned preferByteStringForAscii |
	interned := aStringOrSymbol isSymbol
		ifTrue: [ aStringOrSymbol ]
		ifFalse: [
			preferByteStringForAscii := aStringOrSymbol.
			(aStringOrSymbol isUnicodeString and: [ aStringOrSymbol isAscii ])
				ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
			preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ].
	self basicAdd: interned withHash: anInteger.
	^interned! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 12/30/2021 06:40:48'!
basicLookup: aString withHash: anInteger

	| bucket |
	bucket := self bucketForHash: anInteger.
	^bucket detect: [:any | any = aString] ifNone: [nil]! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 9/9/2023 19:50:15'!
bucketForHash: anInteger

	| bucketIndex |
	bucketIndex := self bucketIndexFor: anInteger.
	^buckets at: bucketIndex! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 9/9/2023 19:49:37'!
bucketIndexFor: anInteger

	^anInteger \\ bucketCount + 1! !

!SymbolSet methodsFor: 'private - lookup' stamp: 'sqr 12/30/2021 07:24:39'!
newRehashBuckets

	| newBucketCount |
	newBucketCount := self size // (self initialBucketSize - 1) max: self initialBuckets.
	^self newBuckets: newBucketCount sized: self initialBucketSize.! !

!SymbolSet methodsFor: 'private - sizes' stamp: 'sqr 12/30/2021 06:55:31'!
initialBucketGrowth

	^4! !

!SymbolSet methodsFor: 'private - sizes' stamp: 'sqr 12/30/2021 06:29:50'!
initialBucketSize

	^8! !

!SymbolSet methodsFor: 'private - sizes' stamp: 'sqr 12/30/2021 06:29:44'!
initialBuckets

	^512! !

!SymbolSet methodsFor: 'private - sizes' stamp: 'sqr 9/9/2023 19:53:48'!
newBucketCount

	^self size // (self initialBucketSize - 1) max: self initialBuckets! !

!SymbolSet methodsFor: 'private - sizes' stamp: 'sqr 12/30/2021 07:18:02'!
targetBucketSize

	^self initialBucketSize! !

