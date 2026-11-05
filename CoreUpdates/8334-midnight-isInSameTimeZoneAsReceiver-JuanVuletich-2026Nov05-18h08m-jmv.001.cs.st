'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 6:16:16 pm'!

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/Nov/2026, 9:16:12 pm (UTC)'!
midnight
	"Answer a DateAndTime starting at midnight in the same local time zone as the receiver."

	^self class basicNew
		setJdn: jdn
		seconds: 0
		nano: 0
		offset: offset
! !

