'From Cuis 4.2 of 25 July 2013 [latest update: #2547] on 21 October 2015 at 10:47:16.224976 am'!

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 10/21/2015 10:43'!
packageNameFrom: fullName

	^fullName asFileEntry nameWithoutExtension! !


!FmFileEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 10:45'!
extension
	^self fileAccessor extensionFor: name! !

!FmFileEntry methodsFor: 'accessing-file name' stamp: 'jmv 10/21/2015 10:46'!
nameWithoutExtension
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	^self fileAccessor baseNameFor: name! !

!methodRemoval: String #sansPeriodSuffix!
String removeSelector: #sansPeriodSuffix!
