'From Cuis 5.0 of 7 November 2016 [latest update: #2984] on 30 November 2016 at 3:22:11 pm'!

!SequenceableCollection methodsFor: 'math functions' stamp: 'jmv 11/30/2016 15:04:11'!
+= anObject
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v + anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) + (anObject at: i)) ]]! !

!SequenceableCollection methodsFor: 'math functions' stamp: 'jmv 11/25/2016 11:41:25'!
-= anObject
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v - anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) - (anObject at: i)) ]]! !

!SequenceableCollection methodsFor: 'math functions' stamp: 'jmv 11/30/2016 15:21:00'!
derivative
	| displaced answer |
	displaced _ self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first - self first.	"Some reasonable zero"
	answer _ self copy.
	answer -= displaced.
	^answer! !

!SequenceableCollection methodsFor: 'math functions' stamp: 'jmv 11/29/2016 14:23:32'!
integral
	| answer |
	answer _ self copy.
	2 to: answer size do: [ :i |
		answer at: i put: (answer at: i) + (answer at: i-1) ].
	^answer! !

!methodRemoval: FloatArray #derivative!
FloatArray removeSelector: #derivative!
