'From Cuis 4.2 of 25 July 2013 [latest update: #2704] on 15 March 2016 at 1:33:23.305681 pm'!

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 3/15/2016 13:03'!
fileExists: fileEntry

	| f |
	f _ self concreteStreamClass new open: fileEntry pathName forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 3/15/2016 13:12'!
fileSize: fileEntry

	| f size |
	f _ self concreteStreamClass new open: fileEntry pathName forWrite: false.
	f ifNil: [^ nil].
	size _ f size.
	f close.
	^ size! !


!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 3/15/2016 13:33'!
fileSize
	"Note: as this value is cached, it will not be updated if the file changes... mhhh...."

	"Slow, original version that reads values from containing directory."
	"fileSize ifNil: [self initValuesFromParent]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a direcotry"
	fileSize ifNil: [fileSize _ self fileAccessor fileSize: self].

	^fileSize! !

!FmFileEntry methodsFor: 'testing' stamp: 'jmv 3/15/2016 13:04'!
exists
"
	| fileNames |
	fileNames := self fileAccessor fileNamesIn: self parent.
	^self fileAccessor isCaseSensitive 
		ifTrue: [ fileNames includes: self name ]
		ifFalse: [ fileNames anySatisfy: [ :n | n sameAs: self name ]]
"
	^self fileAccessor fileExists: self! !

!methodRemoval: StandardFileStream class #isAFileFullNamed:!
StandardFileStream class removeSelector: #isAFileFullNamed:!
