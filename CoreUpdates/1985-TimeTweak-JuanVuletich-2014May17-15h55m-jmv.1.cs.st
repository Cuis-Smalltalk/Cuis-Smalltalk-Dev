'From Cuis 4.2 of 25 July 2013 [latest update: #1984] on 17 May 2014 at 4:32:09.944103 pm'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/17/2014 16:31'!
humanWordsForSecondsAgo: secs
	| date today |
	"Return natural language for this date and time in the past."

	secs <= 1 ifTrue: [^ 'a second ago'].
	secs < 45 ifTrue: [^ secs printString, ' seconds ago'].
	secs < 90 ifTrue: [^ 'a minute ago'].
	secs < "45*60" 2700 ifTrue: [^ (secs//self secondsInMinute) printString, ' minutes ago'].
	secs < "90*60" 5400 ifTrue: [^ 'an hour ago'].
	secs < "18*60*60" 64800 ifTrue: [^ (secs//self secondsInHour) printString, ' hours ago'].
	"now work with dates"
	today _ Date today.
	date _ (DateAndTime now - (Duration seconds: secs)) date.
	date = Date yesterday ifTrue: [^ 'yesterday'].
	date > (today - 8 days) ifTrue: [^ 'last ', date dayOfWeekName].
	date > (today - 13 days) ifTrue: [^ 'a week ago'].
	date > (today - 28 days) ifTrue: [
		^ ((today start - date start) days //7) printString, ' weeks ago'].
	date > (today - 45 days) ifTrue: [^ 'a month ago'].
	date > (today - 300 days) ifTrue: [^ 'last ', date monthName].
	^ date monthName, ', ', date yearNumber printString

"Example
#(0.5 30 62 130 4000 10000 60000 90000 345600 864000 1728000 3456000 17280000 34560000 345600000) 
		collect: [:ss | Time humanWordsForSecondsAgo: ss].
"! !

!methodRemoval: Time class #namesForTimes:!
Time class removeSelector: #namesForTimes:!
