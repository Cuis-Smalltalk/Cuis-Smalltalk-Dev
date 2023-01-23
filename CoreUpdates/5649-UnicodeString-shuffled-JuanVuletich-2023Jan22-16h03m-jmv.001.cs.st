'From Cuis 6.0 [latest update: #5646] on 22 January 2023 at 4:07:23 pm'!

!UnicodeString methodsFor: 'copying' stamp: 'jmv 1/22/2023 16:06:34'!
shuffled
	^ Random withDefaultDo: [ :random |
		"To answer a mutable collection when receiver is, for example, an Interval."
		^(self asArray shuffleBy: random) as: UnicodeString ].

"Examples:
	'This is a String' asUnicodeString shuffled
"! !

