'From Cuis 4.2 of 25 July 2013 [latest update: #2058] on 29 July 2014 at 8:57:38.333843 am'!

!Color class methodsFor: 'color name lookup' stamp: 'KenD 7/25/2014 21:22'!
setColorNamesDict: aNameColorDictionary
	"Answer the current dictionary of  name->color associations."
	
	"Note: This is a non-standard name because this is a non-standard action!!
	Do this if you really, really know what you are doing."
	
	"Some Colors are required for proper system operation"
	Color defaultColorNamesDictionary keysAndValuesDo: [ :colorName :colorValue |
		aNameColorDictionary at: colorName ifAbsentPut: colorValue
	].

	ColorNamesDict := aNameColorDictionary! !

