'From Cuis7.9 [latest update: #8217] on 24 September 2026 at 10:55:10 am'!

!Interval methodsFor: 'accessing' stamp: 'jmv 24/Sep/2026, 1:54:38 pm (UTC)'!
anyOne
 	"This message will fail for an empty Interval, super would not."
	self emptyCheck.
 	^self at: 1! !

