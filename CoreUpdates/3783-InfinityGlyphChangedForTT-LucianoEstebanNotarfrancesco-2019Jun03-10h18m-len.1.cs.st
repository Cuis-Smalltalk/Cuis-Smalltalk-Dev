'From Cuis 5.0 of 7 November 2016 [latest update: #3781] on 3 June 2019 at 10:25:49 am'!

!String methodsFor: 'text conversion helpers' stamp: 'len 6/3/2019 10:24:00'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText bold! !

!String methodsFor: 'text conversion helpers' stamp: 'len 6/3/2019 10:24:16'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText italic! !

!String methodsFor: 'text conversion helpers' stamp: 'len 6/3/2019 10:24:18'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText sub! !

!String methodsFor: 'text conversion helpers' stamp: 'len 6/3/2019 10:24:20'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText super! !

!String methodsFor: 'text conversion helpers' stamp: 'len 6/3/2019 10:24:24'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText under! !


!Text methodsFor: 'adding emphasis' stamp: 'len 6/3/2019 10:24:26'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis bold from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'len 6/3/2019 10:24:28'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis italic from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'len 6/3/2019 10:24:30'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis subscript from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'len 6/3/2019 10:24:32'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis superscript from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'len 6/3/2019 10:24:34'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis underlined from: 1 to: string size! !

