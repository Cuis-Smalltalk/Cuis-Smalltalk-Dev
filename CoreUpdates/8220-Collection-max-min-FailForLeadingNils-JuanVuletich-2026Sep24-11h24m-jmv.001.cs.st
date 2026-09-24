'From Cuis7.9 [latest update: #8218] on 24 September 2026 at 11:24:46 am'!

!Collection methodsFor: 'statistics' stamp: 'jmv 24/Sep/2026, 2:18:33 pm (UTC)'!
max: aBlock
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."

	| first answer |
	self emptyCheck.
	first := true.
	self do: [ :each |
		first
			ifTrue: [
				first := false.
				answer := aBlock value: each ]
			ifFalse: [
				answer := answer max: (aBlock value: each) ]].
	^ answer! !

!Collection methodsFor: 'statistics' stamp: 'jmv 24/Sep/2026, 2:17:19 pm (UTC)'!
min: aBlock
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."

	| first answer |
	self emptyCheck.
	first := true.
	self do: [ :each |
		first
			ifTrue: [
				first := false.
				answer := aBlock value: each ]
			ifFalse: [
				answer := answer min: (aBlock value: each) ]].
	^ answer! !

