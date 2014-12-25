'From Cuis 4.2 of 25 July 2013 [latest update: #2111] on 30 November 2014 at 4:13:49.05923 pm'!

!Collection methodsFor: 'private' stamp: 'jmv 11/30/2014 15:59'!
species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."
	"Redefined here just for reference. See inheritance.
	#collect: always uses #species
	#select: and #copy avoid using it in SortedCollection"

	^ self class! !


!OrderedCollection methodsFor: 'private' stamp: 'jmv 11/26/2014 20:28'!
species
	"aSortedCollection collect: should answer an OrderedCollection"

	^OrderedCollection! !


!SortedCollection methodsFor: 'enumerating' stamp: 'jmv 11/30/2014 15:55'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	"Use self class and not self species. SortedCollection is special,
	the answer for #collect: (an OrderedCollection) is of different kind than
	the answer for #select: or #copy"
	newCollection _ self class sortBlock: sortBlock.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 11/30/2014 12:26'!
atAll: indexArray
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	^ self species streamContents: [ :stream |
		1 to: indexArray size do: [ :index |
			stream nextPut: (self at: (indexArray at: index)) ]]! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 11/30/2014 11:50'!
copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self species new: 0])! !

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 11/30/2014 11:50'!
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self species new: 0])! !


!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 11/30/2014 11:37'!
mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek = $(
				ifTrue: [tempSequence addAll: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~= $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAll: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek = $]] whileTrue: [tempNames next].
		tempNames peek = $[ ifTrue:
			[tempNames next]].
	^map! !


!Heap methodsFor: 'enumerating' stamp: 'jmv 11/26/2014 20:04'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| answer |
	answer _ self species sortBlock: sortBlock.
	self do: [ :each |
		(aBlock value: each)
			ifTrue: [ answer add: each ]].
	^ answer! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 11/30/2014 11:52'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection _ self species new.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection! !


!SortedCollection methodsFor: 'adding' stamp: 'jmv 11/30/2014 11:36'!
addAll: aCollection
	"optimization"
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection! !

!SortedCollection methodsFor: 'copying' stamp: 'jmv 11/30/2014 15:55'!
copy

	| newCollection |
	"Use self class and not self species. SortedCollection is special,
	the answer for #collect: (an OrderedCollection) is of different kind than
	the answer for #select: or #copy"
	newCollection _ self class sortBlock: sortBlock.
	newCollection addAll: self.
	^newCollection! !


!Set methodsFor: 'enumerating' stamp: 'jmv 11/26/2014 20:39'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet _ self species new: self size.
	array do: [ :each | each ifNotNil: [ newSet add: (aBlock value: each)]].
	^ newSet! !


!Dictionary methodsFor: 'enumerating' stamp: 'jmv 11/26/2014 20:31'!
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the
	resulting values into a new collection. Answer it."
	| newCollection |
	newCollection _ OrderedCollection new: self size.
	self do: [:each | newCollection add: (aBlock value: each)].
	^ newCollection! !

!methodRemoval: MessageTally #species!
MessageTally removeSelector: #species!
!methodRemoval: FileDirectory #withAllSubdirectoriesCollect:!
FileDirectory removeSelector: #withAllSubdirectoriesCollect:!
!methodRemoval: Dictionary #copyEmpty!
Dictionary removeSelector: #copyEmpty!
!methodRemoval: SortedCollection #collect:!
SortedCollection removeSelector: #collect:!
!methodRemoval: SortedCollection #copyEmpty!
SortedCollection removeSelector: #copyEmpty!
!methodRemoval: OrderedCollection #addAll:!
OrderedCollection removeSelector: #addAll:!
!methodRemoval: OrderedCollection #addAllLast:!
OrderedCollection removeSelector: #addAllLast:!
!methodRemoval: OrderedCollection #copyEmpty!
OrderedCollection removeSelector: #copyEmpty!
!methodRemoval: Heap #copyEmpty!
Heap removeSelector: #copyEmpty!
!methodRemoval: Heap #species!
Heap removeSelector: #species!

!SequenceableCollection class reorganize!
('stream creation' streamContents: streamContents:estimatedSize: streamContents:limitedTo:)
!

!methodRemoval: SequenceableCollection #copyEmpty!
SequenceableCollection removeSelector: #copyEmpty!
!methodRemoval: SequenceableCollection #groupsOf:atATimeCollect:!
SequenceableCollection removeSelector: #groupsOf:atATimeCollect:!
!methodRemoval: CharacterSet #species!
CharacterSet removeSelector: #species!
!methodRemoval: Collection #copyEmpty!
Collection removeSelector: #copyEmpty!
