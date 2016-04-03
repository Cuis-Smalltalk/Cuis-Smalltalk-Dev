'From Cuis 4.2 of 25 July 2013 [latest update: #2706] on 18 March 2016 at 10:48:05.26572 am'!

!SpaceTally commentStamp: 'jmv 3/18/2016 10:03' prior: 0!
I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: Morph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally

Also try:
'MemoryAnalysis.txt' asFileEntry forceWriteStreamContents: [ :stream |
	SpaceTally new printSpaceAnalysis: 1 on: stream ]!

