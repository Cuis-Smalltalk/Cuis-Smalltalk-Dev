'From Cuis 4.0 of 3 April 2012 [latest update: #1253] on 6 April 2012 at 10:00:53 am'!

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 4/6/2012 09:59'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."
	| fullName |

	"If full path name is not specified, get it assuming default directory."
	fullName _ self fullNameFor: fileName.

	^ FileStream oldFileOrNoneNamed: fullName
! !


!FileStream class methodsFor: 'instance creation' stamp: 'jmv 4/6/2012 09:58'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |

	"If full path name is not specified, get it assuming default directory."
	fullName _ self fullName: fileName.

	^(self concreteStream isAFileNamed: fullName)
		ifTrue: [self concreteStream readOnlyFileNamed: fullName]! !

