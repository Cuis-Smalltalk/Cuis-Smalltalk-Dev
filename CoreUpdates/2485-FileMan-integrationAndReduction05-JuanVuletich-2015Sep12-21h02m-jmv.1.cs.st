'From Cuis 4.2 of 25 July 2013 [latest update: #2483] on 12 September 2015 at 9:03:11.691875 pm'!

!FmFileIOAccessor class methodsFor: 'instance creation' stamp: 'jmv 9/12/2015 21:02'!
default
	Default isNil
		ifTrue: [
			Default _ self new].
	^ Default! !

!classRemoval: #FmFileDirectoryFileIOAccessorCuis!
Smalltalk removeClassNamed: #FmFileDirectoryFileIOAccessorCuis!
