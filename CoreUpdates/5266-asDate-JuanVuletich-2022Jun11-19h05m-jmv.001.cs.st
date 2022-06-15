'From Cuis 6.0 [latest update: #5263] on 11 June 2022 at 7:06:25 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/12/2017 11:22:17'!
asDate
	"Many allowed forms, see Date>>#readFrom:
		'2014/6/30' asDate.
		'2014/6/30' asUtf8String asDate.
		'70/12/30' asDate.
		'70/12/30' asUtf8String asDate.
		'12/30/70' asDate.
		'12/30/70' asUtf8String asDate.
		'30/12/70' asDate.
		'30/12/70' asUtf8String asDate.
		'4/5/6' asDate.
		'4/5/6' asUtf8String asDate.
		'15 April 1982' asDate.
		'15 April 1982' asUtf8String asDate.
	"

	^ Date fromString: self! !

!methodRemoval: String #asDate stamp: 'jmv 6/11/2022 19:06:05'!
String removeSelector: #asDate!
