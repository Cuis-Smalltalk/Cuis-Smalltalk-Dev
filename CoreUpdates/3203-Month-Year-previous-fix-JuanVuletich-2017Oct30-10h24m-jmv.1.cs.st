'From Cuis 5.0 of 7 November 2016 [latest update: #3202] on 30 October 2017 at 10:28:09 am'!

!Timespan methodsFor: 'smalltalk-80' stamp: 'jmv 10/30/2017 10:26:58'!
previous
	"
	(Month month: 10 year: 2017) previous
	(Year yearNumber: 2016) previous
	"
	^self class classDefinesDuration
		ifTrue: [ self class including: self end - duration ]
		ifFalse: [ self class starting: start - duration duration: duration ]! !

