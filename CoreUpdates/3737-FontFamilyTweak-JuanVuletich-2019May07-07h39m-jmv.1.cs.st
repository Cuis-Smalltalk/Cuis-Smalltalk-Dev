'From Cuis 5.0 of 7 November 2016 [latest update: #3736] on 7 May 2019 at 9:43:54 am'!

!StrikeFontFamily methodsFor: 'accessing' stamp: 'jmv 5/7/2019 09:21:59'!
atPointSize: aNumber put: aFontAndSize
	"aFontAndSize must have emphasis = 0, i.e. it is a base font"
	baseFontBySizes at: aNumber put: aFontAndSize ! !

!methodRemoval: FontFamily #atPointSize:put:!
FontFamily removeSelector: #atPointSize:put:!

!StrikeFontFamily reorganize!
('accessing' atPointSize:put:)
!

