'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 20 December 2013 at 3:19:26.749052 pm'!

!OrderedCollection methodsFor: 'private' stamp: 'sqr 12/20/2013 15:15'!
find: oldObject
	"Answer an index in the range [firstIndex, lastIndex]"

	| index |
	index _ firstIndex.
	[index <= lastIndex] whileTrue:
		[(array at: index) = oldObject ifTrue: [^index].
		index _ index + 1].
	self errorNotFound: oldObject! !

!OrderedCollection methodsFor: 'private' stamp: 'sqr 12/20/2013 15:14'!
insert: anObject before: spot
	"Spot is an index in the range [firstIndex, lastIndex]"

	| delta spotIndex|
	spotIndex _ spot.
	delta _ spotIndex - firstIndex.
	firstIndex = 1 ifTrue: 
		[self makeRoomAtFirst.
		spotIndex _ firstIndex + delta].
	firstIndex _ firstIndex - 1.
	array
		replaceFrom: firstIndex
		to: spotIndex - 2
		with: array
		startingAt: firstIndex + 1.
	array at: spotIndex - 1 put: anObject.
	^anObject! !

!OrderedCollection methodsFor: 'private' stamp: 'sqr 12/20/2013 15:11'!
makeRoomAtFirst

	| size newArray newFirst newLast lastGap writeGap |
	size _ self size.
	lastGap _ size - lastIndex.
	lastGap - 4 * 5 > size ifTrue:
		[
			newLast _ size - (lastGap bitShift: -5).
			writeGap _ newLast - lastIndex.
			writeGap > 0 ifTrue:
				[
					newFirst _ firstIndex + writeGap.
					newLast to: newFirst by: -1 do:
						[:each | array at: each put: (array at: each - writeGap)].
					array from: firstIndex to: newFirst - 1 put: nil.
					firstIndex _ newFirst.
					lastIndex _ newLast.
					^self
				]
		].
	newLast _ size + self growSize.
	newArray _ Array new: newLast.
	newFirst _ firstIndex + newLast - lastIndex.
	newArray replaceFrom: newFirst to: newLast with: array startingAt: firstIndex.
	array _ newArray.
	firstIndex _ newFirst.
	lastIndex _ newLast! !

!OrderedCollection methodsFor: 'private' stamp: 'sqr 12/20/2013 15:12'!
makeRoomAtLast

	| size newArray newFirst newLast |
	size _ self size.
	firstIndex - 4 * 5 > size ifTrue:
		[
			newFirst _ (firstIndex bitShift: -5) + 1.
			newFirst < firstIndex ifTrue:
				[
					newLast _ lastIndex - firstIndex + newFirst.
					array
						replaceFrom: newFirst
						to: newLast
						with: array
						startingAt: firstIndex.
					array from: newLast + 1 to: lastIndex put: nil.
					firstIndex _ newFirst.
					lastIndex _ newLast.
					^self
				]
		].
	newArray _ Array new: size + self growSize.
	newLast _ lastIndex - firstIndex + 1.
	newArray replaceFrom: 1 to: newLast with: array startingAt: firstIndex.
	array _ newArray.
	firstIndex _ 1.
	lastIndex _ newLast! !

!OrderedCollection methodsFor: 'private' stamp: 'sqr 12/20/2013 15:16'!
removeIndex: removedIndex
 	"The removedIndex value is in the range [firstIndex, lastIndex]"

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex _ lastIndex - 1! !

!methodRemoval: OrderedCollection #grow!
OrderedCollection removeSelector: #grow!

!OrderedCollection reorganize!
('accessing' at: at:put: size)
('adding' add: add:after: add:afterIndex: add:before: add:beforeIndex: addAll: addAllFirst: addAllFirstUnlessAlreadyPresent: addAllLast: addFirst: addLast: at:ifAbsentPut: growSize)
('copying' copyEmpty copyFrom:to: copyReplaceFrom:to:with: copyWith: reversed)
('enumerating' collect: collect:from:to: do: reverseDo: select: with:collect: withIndexCollect:)
('inspecting' inspectorClass)
('removing' remove:ifAbsent: removeAll removeAllSuchThat: removeAt: removeFirst removeFirst: removeLast removeLast:)
('sorting' sort sort: sorted:)
('testing' hasContentsInExplorer)
('private' collector errorNoSuchElement find: insert:before: makeRoomAtFirst makeRoomAtLast removeIndex: reset resetTo: setCollection: setContents:)
!

