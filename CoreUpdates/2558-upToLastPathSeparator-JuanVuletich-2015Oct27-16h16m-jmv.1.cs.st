'From Cuis 4.2 of 25 July 2013 [latest update: #2557] on 27 October 2015 at 4:17:12.839492 pm'!

!String methodsFor: '*fileman-core-accessing' stamp: 'mu 5/7/2007 15:49'!
upToLastPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	! !

