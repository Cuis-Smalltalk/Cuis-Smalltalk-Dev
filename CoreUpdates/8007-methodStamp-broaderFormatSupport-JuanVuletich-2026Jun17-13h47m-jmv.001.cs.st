'From Cuis7.9 [latest update: #8004] on 17 June 2026 at 2:24:51 pm'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 17/Jun/2026 14:22:58'!
authorAndStamp: authorAndStamp partsDo: aBlock
	"Given a method time stamp string (see senders, see #authorAndStampAsSaved)
	evaluate a block on its parts: author initials[String] and timestamp[DateAndTime].
	Parts may be nil if not present.
	Support the various formats saved by Smalltalk-80, Squeak and Cuis over the years.
(Examples follow)
	CompiledMethod authorAndStamp: 'Squeak1.2 29/Jun/1997 10:40:55' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: ' 9/Jun/1997 00:00:00' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 10/Jan/2014 22:53:00' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am ' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 pm ' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 23:02:25' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am (-03:00)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25Z' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 (UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25(UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am (UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011T14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011X14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011T4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011X4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5-20-2011 4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
"
	| authorAndDateParts authorPart colonPosition authorDateSeparator timePart datePart utcOffsetPart utcOffset dateAndTime |
	authorAndStamp ifNil: [^aBlock value: nil value: nil].
	authorAndDateParts := authorAndStamp.

	"Look for a Time (perhaps with an UTC offset afterwards)"
	colonPosition := authorAndStamp indexOf: $:.
	colonPosition = 0 ifFalse: [ | dateEnd timeStart utcOffsetStart |
		timeStart := (authorAndStamp at: colonPosition-2) isDigit
			ifTrue: [colonPosition-2]
			ifFalse: [colonPosition-1].
		dateEnd := (authorAndStamp at: timeStart-2) isDigit
			ifTrue: [timeStart-2]
			ifFalse: [timeStart-3].
		timePart := authorAndStamp copyFrom: timeStart to: authorAndStamp size.
		authorAndDateParts := authorAndStamp copyFrom: 1 to: dateEnd.
		"Look for a final UTC offset"
		utcOffsetPart := nil.
		utcOffsetStart := timePart indexOf: $(.
		utcOffsetStart = 0
			ifFalse: [
				utcOffsetPart := timePart copyFrom: utcOffsetStart+1 to: timePart size-1.
				timePart := timePart copyFrom: 1 to: utcOffsetStart-1 ]
			ifTrue: [
				utcOffsetStart := timePart indexOf: $Z ifAbsent: [timePart indexOf: $U].
				utcOffsetStart = 0 ifFalse: [
					utcOffsetPart := timePart copyFrom: utcOffsetStart to: timePart size.
					timePart := timePart copyFrom: 1 to: utcOffsetStart-1 ]].
		utcOffset := utcOffsetPart ifNotNil: [
			(utcOffsetPart = 'UTC' or: [ utcOffsetPart = 'Z'])
				ifTrue: [Duration zero]
				ifFalse: [Duration fromString: utcOffsetPart ]]].

	"Look for a Date. Separate authorPart and datePart."
	authorDateSeparator := authorAndDateParts lastIndexOf: Character space.
	datePart := authorAndDateParts copyFrom: authorDateSeparator+1 to: authorAndDateParts size.
	((datePart occurrencesOf: $/) >=2 or: [ (datePart occurrencesOf: $-) >=2  ])
		ifTrue: [
			authorPart := authorDateSeparator > 0 ifTrue: [authorAndDateParts copyFrom: 1 to: authorDateSeparator] ifFalse: ['unknown']]
		ifFalse: [
			authorPart := authorAndDateParts.
			datePart := nil.].

	dateAndTime := datePart ifNotNil: [
		DateAndTime
			date: (Date fromString: datePart)
			time: (timePart ifNotNil: [Time fromString: timePart withBlanksTrimmed])
			offset: utcOffset ].
	^aBlock
		value: authorPart withBlanksTrimmed
		value: dateAndTime! !

