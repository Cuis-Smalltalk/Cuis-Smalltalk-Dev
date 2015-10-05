'From Cuis 4.2 of 25 July 2013 [latest update: #2520] on 5 October 2015 at 12:23:36.974751 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 10/5/2015 12:22'!
imagePath
	"Answer the path for the directory containing the image file."
	"
	Smalltalk imagePath
	"
	| pathName |
	pathName _ self imageName.
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator! !

