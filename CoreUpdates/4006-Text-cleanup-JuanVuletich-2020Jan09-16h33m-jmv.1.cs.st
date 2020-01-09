'From Cuis 5.0 [latest update: #4005] on 9 January 2020 at 4:34:58 pm'!

!Text methodsFor: 'emphasis' stamp: 'jmv 1/9/2020 11:59:43'!
colorAt: characterIndex
	"Answer the color for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ color ]! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:15:35'!
color: aColor
	"Stuff like
	'Hello world' blue edit
	"
	self addAttribute: (TextColor color: aColor)! !


!Text methodsFor: 'emphasis' stamp: 'jmv 1/9/2020 12:11:24'!
addAttribute: att 
	string size = 0 ifTrue: [ ^self ].
	^ self addAttribute: att from: 1 to: self size! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:16:25'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis bold! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:16:49'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis italic! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:18:12'!
pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	self addAttribute: (TextFontFamilyAndSize pointSize: pointSize)! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:18:47'!
struck
	"Stuff like
	('Hello world' struck ) edit
	"
	self addAttribute: TextEmphasis struckThrough! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:19:00'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis subscript! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:19:10'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis superscript! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 1/9/2020 12:19:21'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis underlined! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 1/9/2020 12:16:39'!
centered
	"Stuff like
	('Hello world' centered ) edit
	"
	self addAttribute: TextAlignment centered! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 1/9/2020 12:17:33'!
justified
	"Stuff like
	('Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. ' justified ) edit
	"
	self addAttribute: TextAlignment justified! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 1/9/2020 12:17:54'!
leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	self addAttribute: TextAlignment leftFlush! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 1/9/2020 12:18:23'!
rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	self addAttribute: TextAlignment rightFlush! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:12:58'!
black
	"Stuff like
	'Hello world' black edit
	"
	self color: Color black! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:12:40'!
blue
	"Stuff like
	'Hello world' blue edit
	"
	self color: Color blue! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:13:20'!
cyan
	"Stuff like
	'Hello world' cyan edit
	"
	self color: Color cyan! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:13:38'!
gray
	"Stuff like
	'Hello world' gray edit
	"
	self color: Color gray! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:13:51'!
green
	"Stuff like
	'Hello world' green edit
	"
	self color: Color green! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:14:13'!
magenta
	"Stuff like
	'Hello world' magenta edit
	"
	self color: Color magenta! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:14:27'!
red
	"Stuff like
	'Hello world' red edit
	"
	self color: Color red! !

!Text methodsFor: 'adding color' stamp: 'jmv 1/9/2020 12:14:45'!
yellow
	"Stuff like
	'Hello world' yellow edit
	"
	self color: Color yellow! !

