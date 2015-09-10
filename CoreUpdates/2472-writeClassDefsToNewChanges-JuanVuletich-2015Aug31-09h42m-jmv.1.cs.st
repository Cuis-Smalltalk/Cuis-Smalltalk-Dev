'From Cuis 4.2 of 25 July 2013 [latest update: #2470] on 31 August 2015 at 9:46:40.553099 am'!
"Change Set:		2471-CuisCore-AuthorName-2015Aug31-09h42m
Date:				31 August 2015
Author:			Juan Vuletich

Original code and comment by Eliot Miranda at Squeak's Kernel-eem.938

Have ClassDescription>>moveChangesTo: write the class's definition before the changes if there are any so that the changes file is a potential application recovery vehicle, not just a crash recovery vehicle."!


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/31/2015 09:46'!
moveChangesTo: newFile 
	"Used in the process of condensing changes, this message requests that 
	the source code of all methods of the receiver that have been changed 
	should be moved to newFile.
	
	Write the class's definition before the changes if there are any so that the changes file is a potential application recovery vehicle, not just a crash recovery vehicle."

	| changes |
	changes _ self methodDict keys select: [:sel | (self methodDict at: sel) fileIndex > 1].
	
         changes isEmpty ifTrue:
                 [^self].
         newFile newLine; newLine; nextChunkPut: self definition; newLine.

	self fileOutChangedMessages: changes asSet
		on: newFile
		moveSource: true
		toFile: 2! !

