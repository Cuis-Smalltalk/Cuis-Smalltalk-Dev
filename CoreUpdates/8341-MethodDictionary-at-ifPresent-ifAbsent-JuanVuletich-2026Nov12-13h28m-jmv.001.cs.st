'From Cuis7.9 [latest update: #8340] on 12 November 2026 at 1:28:46 pm'!

!MethodDictionary methodsFor: 'accessing' stamp: 'jmv 12/Nov/2026, 3:11:58 pm (UTC)'!
at: key ifPresent: presentBlock ifAbsent: absentBlock

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ absentBlock value ].
	^ presentBlock value: (array at: index)! !

