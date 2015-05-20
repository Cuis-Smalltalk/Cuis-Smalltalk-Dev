'From Cuis 4.2 of 25 July 2013 [latest update: #2327] on 20 May 2015 at 8:52:46.946987 am'!

!StandardFileStream class methodsFor: 'file creation' stamp: 'jmv 5/20/2015 08:49'!
newFileNamed: fileName do: aBlock
	"Create a new file with the given name.
	If the file already exists, give the chance to pick another name or overwrite it
	Evaluate aBlock, and close the file"

	(self newFileNamed: fileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !


!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 5/20/2015 08:52'!
newFileNamed: localFileName do: aBlock
	"Create a new file with the given name in this directory and pass it as argument to aBlock."

	^ FileStream newFileNamed: (self fullNameFor: localFileName) do: aBlock
! !

