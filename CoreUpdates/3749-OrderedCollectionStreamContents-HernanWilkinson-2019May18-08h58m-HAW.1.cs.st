'From Cuis 5.0 of 7 November 2016 [latest update: #3748] on 18 May 2019 at 5:07:05 pm'!

!Collection class methodsFor: 'instance creation' stamp: 'HAW 5/18/2019 09:36:08'!
ofSize: aSize

	"Create a new collection of size aSize with nil as its elements.
	This method exists because OrderedCollection new: aSize creates an
	empty collection,  not one of size aSize that it is necesary for #streamContents: 
	when sent to OrderedCollection"
	
	^ self new: aSize! !


!OrderedCollection methodsFor: 'initialization' stamp: 'HAW 5/18/2019 16:49:45'!
initializeOfSize: aSize

	array := Array new: aSize.
	firstIndex := 1.
	lastIndex := aSize.! !


!OrderedCollection class methodsFor: 'instance creation' stamp: 'HAW 5/18/2019 16:49:03'!
ofSize: aSize

	"See superclass implementation"
	
	^ super new initializeOfSize: aSize! !


!SequenceableCollection class methodsFor: 'stream creation' stamp: 'HAW 5/18/2019 16:51:23'!
streamContents: blockWithArg estimatedSize: estimatedSize

	| stream originalContents |
	
	stream _ WriteStream on: (self ofSize: estimatedSize).
	blockWithArg value: stream.
	originalContents _ stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents ]
		ifFalse: [ stream contents ]! !


!WriteStream methodsFor: 'private' stamp: 'HAW 5/18/2019 16:53:59'!
growTo: anInteger
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize grownCollection newSize |
	oldSize _ collection size.
     newSize _ anInteger + (oldSize max: 20).
	grownCollection _ collection class ofSize: newSize.
	collection _ grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit _ collection size! !


!OrderedCollection reorganize!
('accessing' at: at:put: size)
('adding' add: add:after: add:afterIndex: add:before: add:beforeIndex: addAllFirst: addAllLast: addFirst: addLast: at:ifAbsentPut: growSize)
('copying' copy copyFrom:to: copyReplaceFrom:to:with: copyWith: reversed)
('enumerating' collect: collect:from:to: do: reverseDo: select: with:collect: with:with:collect: withIndexCollect:)
('inspecting' inspectorClass)
('removing' remove:ifAbsent: removeAll removeAllSuchThat: removeAt: removeFirst removeFirst: removeLast removeLast:)
('sorting' sort sort:)
('testing' hasContentsInExplorer)
('private' errorNoSuchElement find: insert:before: makeRoomAtFirst makeRoomAtLast removeIndex: reset resetTo: setCollection: setContents: species)
('comparing' =)
('converting' asNewArray)
('initialization' initializeOfSize:)
!


!OrderedCollection class reorganize!
('instance creation' new new: newFrom: ofSize:)
!

