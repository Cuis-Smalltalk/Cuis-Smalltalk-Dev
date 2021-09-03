'From Cuis 5.0 [latest update: #4832] on 3 September 2021 at 11:27:46 am'!

!SpaceTally commentStamp: 'jmv 9/3/2021 10:57:56' prior: 0!
I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: Morph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally

Also try:
'MemoryAnalysis.txt' asFileEntry forceWriteStreamDo: [ :stream |
	SpaceTally new printSpaceAnalysis: 1 on: stream ]!

