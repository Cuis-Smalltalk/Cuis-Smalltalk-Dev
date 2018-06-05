'From Cuis 5.0 of 7 November 2016 [latest update: #3341] on 1 June 2018 at 8:56:07 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
FloatArray variableWordSubclass: #Color
	instanceVariableNames: ''
	classVariableNames: 'CachedColormaps ColorNamesDict FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'other' stamp: 'jmv 6/1/2018 20:55:02'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color.
	Return nil if named color support is not present"

	Color classPool 
		at: #ColorNamesDict
		ifPresent: [ :dict | ^dict keyAtValue: self ifAbsent: [nil]].
	^nil! !


!Color class methodsFor: 'cached state access' stamp: 'jmv 6/1/2018 20:55:34'!
releaseClassCachedState
	"
	Color releaseClassCachedState
	"
	CachedColormaps _ nil. 		"Maps to translate between color depths"
	ToGrayColormaps _ nil.			"Maps colors to 8bpp grays for various color depths"
	FromGrayColormaps _ nil.		"Maps from 8bpp grays to colors of various depths"
	MaskingMap _ nil. 				"Maps all colors except transparent to black for creating a mask"
	IndexedColors _ nil.
	GrayToIndexMap _ nil! !

!classDefinition: #Color category: #'Graphics-Primitives'!
FloatArray variableWordSubclass: #Color
	instanceVariableNames: ''
	classVariableNames: 'CachedColormaps FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
