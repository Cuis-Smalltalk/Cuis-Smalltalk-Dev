'From Cuis 4.2 of 25 July 2013 [latest update: #2519] on 4 October 2015 at 10:03:42.147832 am'!

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/4/2015 10:01'!
fileOut

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', self name, '.st') asFileEntry writeStream: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/4/2015 09:57'!
fileOutCategory: categoryName

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', self name,'-',categoryName,'.st') asFileEntry writeStream: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 10/4/2015 09:57'!
fileOutMethod: selector

	(FmDirectoryEntry smalltalkImageDirectory pathName, '/', self name,'-', selector, '.st') asFileEntry writeStream: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]! !

!methodRemoval: SystemOrganizer #fileOutCategory:on:!
SystemOrganizer removeSelector: #fileOutCategory:on:!
