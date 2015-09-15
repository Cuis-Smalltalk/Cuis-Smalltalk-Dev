'From Cuis 4.2 of 25 July 2013 [latest update: #2479] on 12 September 2015 at 10:44:19.971319 am'!

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 9/12/2015 10:43'!
beDefault
	self = FmFileIOAccessor ifTrue: [^self]. "This method is for a subclass"
	Default := self new! !

!methodRemoval: FmFileIOAccessor class #dialectUtilClass!
FmFileIOAccessor class removeSelector: #dialectUtilClass!
!methodRemoval: FmFileIOAccessor #dialectUtilClass!
FmFileIOAccessor removeSelector: #dialectUtilClass!
!classRemoval: #FmPortableUtil!
Smalltalk removeClassNamed: #FmPortableUtil!
!classRemoval: #FmSqPortableUtil!
Smalltalk removeClassNamed: #FmSqPortableUtil!
