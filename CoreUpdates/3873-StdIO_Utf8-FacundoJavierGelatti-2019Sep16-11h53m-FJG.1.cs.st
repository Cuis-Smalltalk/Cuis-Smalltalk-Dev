'From Cuis 5.0 [latest update: #3871] on 16 September 2019 at 11:54:40 am'!

!StdIOWriteStream methodsFor: 'character writing' stamp: 'FJG 9/15/2019 02:53:23'!
printUtf8CodePoint: utf8CodePoint
	"Example: printing a checkmark on the console
		 'StdIOWriteStream stdout printUtf8CodePoint: 16r2713; flush.'
	"
	
	| characterBytes |

	characterBytes _ Character utf8BytesOfUnicodeCodePoint: utf8CodePoint.
	
	self primWrite: fileID from: characterBytes startingAt: 1 count: characterBytes size
! !

