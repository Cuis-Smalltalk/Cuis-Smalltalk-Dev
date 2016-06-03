'From Cuis 4.2 of 25 July 2013 [latest update: #2788] on 2 June 2016 at 8:44:19.333635 pm'!

!Form methodsFor: 'converting' stamp: 'jmv 6/2/2016 20:31'!
asGrayForm
	| answer map |

	"For lower bit depths, the 15 bit ColorMap loses no information,
	and answers the real #luminance of each pixel."
	self depth < 32 ifTrue: [
		answer _ GrayForm extent: width@height.
		map _ self colormapIfNeededForGray8bpp.
		(BitBlt toForm: answer)
			colorMap: map;
			copy: self boundingBox
			from: 0@0 in: self
			fillColor: nil rule: Form over.
		answer offset: self offset.
		^ answer ].

	"For 32bpp, the approach below would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3! !

