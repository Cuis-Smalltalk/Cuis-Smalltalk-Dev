'From Cuis 4.2 of 25 July 2013 [latest update: #2107] on 11 November 2014 at 9:08:15.172934 am'!

!Color methodsFor: 'private' stamp: 'jmv 11/10/2014 20:58'!
setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 
	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ brightness - ((r1 max: g1) max: b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 11/10/2014 21:01'!
setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ luma - (0.299*r1) - (0.587*g1) - (0.114*b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0 
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1 colorSpace: #sRGB! !

!Color methodsFor: 'private' stamp: 'jmv 11/10/2014 20:31'!
setHue: hue saturation: saturation brightness: brightness
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t | 
	s _ saturation asFloat min: 1.0 max: 0.0.
	v _ brightness asFloat min: 1.0 max: 0.0.
	hf _ hue \\ 360.
	i _ hf // 60.  			"integer part of hue"
	f _ (hf \\ 60) / 60.0.         	"fractional part of hue"

	p _ (1.0 - s) * v.
	q _ (1.0 - (s * f)) * v.
	t _ (1.0 - (s * (1.0 - f))) * v.

	0 = i ifTrue: [ ^ self setRed: v green: t blue: p colorSpace: #sRGB ].
	1 = i ifTrue: [ ^ self setRed: q green: v blue: p colorSpace: #sRGB ].
	2 = i ifTrue: [ ^ self setRed: p green: v blue: t colorSpace: #sRGB ].
	3 = i ifTrue: [ ^ self setRed: p green: q blue: v colorSpace: #sRGB ].
	4 = i ifTrue: [ ^ self setRed: t green: p blue: v colorSpace: #sRGB ].
	5 = i ifTrue: [ ^ self setRed: v green: p blue: q colorSpace: #sRGB ].

	self error: 'implementation error'! !

!methodRemoval: TranslucentColor #fix!
TranslucentColor removeSelector: #fix!
!methodRemoval: Color #fix!
Color removeSelector: #fix!
