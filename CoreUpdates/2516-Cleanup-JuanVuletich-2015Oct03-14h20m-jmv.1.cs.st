'From Cuis 4.2 of 25 July 2013 [latest update: #2513] on 3 October 2015 at 2:28:35.143016 pm'!

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 10/3/2015 14:25'!
newFileNamed: localFileName do: aBlock
	"Create a new file with the given name in this directory and pass it as argument to aBlock."

	(self newFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!methodRemoval: StandardFileStream class #newFileNamed:do:!
StandardFileStream class removeSelector: #newFileNamed:do:!
