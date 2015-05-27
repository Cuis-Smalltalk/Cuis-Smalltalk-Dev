'From Cuis 4.2 of 25 July 2013 [latest update: #2336] on 26 May 2015 at 10:58:47.373331 am'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'jmv 5/26/2015 10:43'!
nextHandlerContext

	^ self sender ifNotNil: [ :sndr | sndr findNextHandlerContext ]! !

