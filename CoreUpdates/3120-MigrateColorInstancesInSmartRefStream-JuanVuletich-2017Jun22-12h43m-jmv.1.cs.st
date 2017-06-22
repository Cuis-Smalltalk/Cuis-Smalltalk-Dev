'From Cuis 5.0 of 7 November 2016 [latest update: #3119] on 22 June 2017 at 12:54:43 pm'!

!Color methodsFor: 'as yet unclassified' stamp: 'jmv 6/22/2017 12:54:10'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	"subclasses should implement if they wish to convert old instances to modern ones"
	self size = 0 ifTrue: [
		^ Color new copyFrom: (varDict at: 'floatRGB') ].
	^ self! !

