'From Cuis6.3 [latest update: #6218] on 7 February 2024 at 4:36:58 pm'!

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:13:49'!
copyBlank: requestedSize
	"Answer an instance like me, of size n, but without content.
	All elements may be nil, zero or some other default value if needed."

	^self species new: requestedSize! !


!OrderedCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:14:31'!
copyBlank: requestedSize
	"Answer an instance like me, of size n, but without content.
	All entries are nil."

	^self species basicNew initializeOfSize: requestedSize! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:12:32'!
copyFrom: start count: n
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self copyBlank: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:14:06'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1 max: 0.
	^(self copyBlank: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:14:40'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self copyBlank: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 2/7/2024 16:15:40'!
grownTo: newSize

	| grownCollection |
	grownCollection := self copyBlank: newSize.
	grownCollection replaceFrom: 1 to: self size with: self startingAt: 1.
	^grownCollection! !


!ClosureScanner methodsFor: 'private - depth first scan queue' stamp: 'jmv 2/7/2024 16:21:05'!
newScanQueueSegment
	"Segment the scan queue, otherwise the GC will be forced to scan the whole
	queue every time (obviously, the scan queue will be in the remember table)"

	| newScanQueue |
	newScanQueue := OrderedCollection basicNew.
	newScanQueue setContents: (Array new: self scanQueueSegmentTargetSize).
	newScanQueue resetTo: 1.
	^newScanQueue! !

