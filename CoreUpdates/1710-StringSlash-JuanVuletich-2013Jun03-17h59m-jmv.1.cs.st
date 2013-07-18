'From Cuis 4.1 of 12 December 2012 [latest update: #1709] on 3 June 2013 at 5:59:11.198265 pm'!

!String methodsFor: 'arithmetic' stamp: 'jmv 6/3/2013 16:10'!
/ arg
	"So we can write
	'folderName'/ 'fileName.ext'
	and it works on any platdorm
	"
	^String streamContents: [ :strm |
		strm
			nextPutAll: self;
			nextPut: FileDirectory pathNameDelimiter;
			nextPutAll: arg ]! !

