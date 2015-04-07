'From Cuis 4.2 of 25 July 2013 [latest update: #2208] on 7 March 2015 at 11:14:16.206667 pm'!

!OrderedCollection methodsFor: 'converting' stamp: 'jmv 3/7/2015 23:13'!
asOrderedCollection

	^self! !


!SortedCollection methodsFor: 'converting' stamp: 'jmv 3/7/2015 23:13'!
asSortedCollection

	^self! !


!SortedCollection reorganize!
('accessing' at:put: median sortBlock sortBlock:)
('adding' add: addAll: addFirst:)
('comparing' =)
('copying' copy postCopy)
('enumerating' select:)
('private' defaultSort:to: indexForInserting: insert:before: reSort)
('converting' asSortedCollection)
!


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
('converting' asOrderedCollection)
!

