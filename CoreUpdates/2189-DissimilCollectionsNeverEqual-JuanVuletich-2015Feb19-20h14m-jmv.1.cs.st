'From Cuis 4.2 of 25 July 2013 [latest update: #2188] on 19 February 2015 at 8:37:54.305598 pm'!

!OrderedCollection methodsFor: 'comparing' stamp: 'jmv 2/19/2015 20:32'!
= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."


	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self == otherCollection ifTrue: [^ true].
	self class == otherCollection class ifFalse: [^ false].
	^ self hasEqualElements: otherCollection! !


!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 2/19/2015 20:36'!
= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	"#( 1 2 3) = (1 to: 3) -> false"
	otherCollection isInterval ifTrue: [ ^false ].
	^ self hasEqualElements: otherCollection! !


!Interval methodsFor: 'comparing' stamp: 'jmv 2/19/2015 20:36'!
= otherCollection

	self == otherCollection ifTrue: [^ true].
	"(1 to: 3) = #( 1 2 3) -> false"
	otherCollection isInterval ifFalse: [ ^false ].
	^ start = otherCollection first
		and: [step = otherCollection increment
			and: [self last = otherCollection last]]! !


!SortedCollection methodsFor: 'comparing' stamp: 'jmv 2/19/2015 20:30'!
= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	"Any object is equal to itself"
	self == aSortedCollection ifTrue: [ ^ true ].

	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self class == aSortedCollection class ifFalse: [ ^ false ].

	sortBlock = aSortedCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: aSortedCollection ! !


!OrderedCollection reorganize!
('accessing' at: at:put: size)
('adding' add: add:after: add:afterIndex: add:before: add:beforeIndex: addAllFirst: addFirst: addLast: at:ifAbsentPut: growSize)
('copying' copyFrom:to: copyReplaceFrom:to:with: copyWith: reversed)
('enumerating' collect: collect:from:to: do: reverseDo: select: with:collect: withIndexCollect:)
('inspecting' inspectorClass)
('removing' remove:ifAbsent: removeAll removeAllSuchThat: removeAt: removeFirst removeFirst: removeLast removeLast:)
('sorting' sort sort: sorted:)
('testing' hasContentsInExplorer)
('private' errorNoSuchElement find: insert:before: makeRoomAtFirst makeRoomAtLast removeIndex: reset resetTo: setCollection: setContents: species)
('comparing' =)
!

