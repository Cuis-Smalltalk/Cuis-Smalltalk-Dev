'From Cuis7.9 [latest update: #8001] on 16 June 2026 at 5:34:18 pm'!

!Date class methodsFor: 'smalltalk-80' stamp: 'jmv 9/Apr/2010 11:00:00'!
today
	"In local time, the date in the current time zone.
	This is, the date 'here'."

	^ DateAndTime now date! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'brp 27/Jul/2003 17:09:00'!
today
	"Usually you prefer
	Date today"

	^ self midnight! !

