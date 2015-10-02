'From Cuis 4.2 of 25 July 2013 [latest update: #2506] on 27 September 2015 at 7:30:54.011843 pm'!

!SpaceTally commentStamp: 'jmv 9/21/2015 16:36' prior: 0!
I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally

Also try:
'MemoryAnalysis.txt' asFileEntry forceWriteStreamContents: [ :Stream |
	SpaceTally new printSpaceAnalysis: 1 on: stream ]!

