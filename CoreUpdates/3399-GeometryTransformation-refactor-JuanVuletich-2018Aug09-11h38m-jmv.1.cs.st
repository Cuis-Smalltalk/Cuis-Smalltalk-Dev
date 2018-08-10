'From Cuis 5.0 of 7 November 2016 [latest update: #3398] on 9 August 2018 at 11:45:19 am'!
!classDefinition: #AffineTransformation category: #'Graphics-Primitives'!
GeometryTransformation variableWordSubclass: #AffineTransformation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classDefinition: #Homography category: #'Graphics-Primitives'!
GeometryTransformation variableWordSubclass: #Homography
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Homography commentStamp: '<historical>' prior: 0!
An homography (or perspective transformation) for resampling images (for example). Can apply or correct for the perspective resulting from taking a photograph to a 2D object that is not perpendicular to the camera axis (for example, taking an image of the ground ahead of the camera).

http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#warpperspective

To be of use, needs #map:to: in NumCuis/ImageProcessing.pck.st!

!classDefinition: #MorphicTranslation category: #'Graphics-Primitives'!
GeometryTransformation subclass: #MorphicTranslation
	instanceVariableNames: 'deltaX deltaY '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Morph commentStamp: '<historical>' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. 

Structure:
instance var 	Type 						Description 
owner 		Morph		 				My parent Morph, or nil for the top-level Morph, which is a
 				or nil						world, typically a PasteUpMorph.
submorphs 	Array 						My child Morphs. 
location 		MorphicTranslation 		Specifies position (and possibly, angle of rotation and scale change) inside owner
				or AffineTransformation	
extension 	MorphExtension 			Allows extra properties to be stored without adding a
				or nil  						storage burden to all morphs. 
!


!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 8/9/2018 11:25:47'!
is: aSymbol
	^aSymbol == #GeometryTransformation or: [ super is: aSymbol ]! !

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 8/9/2018 11:27:48'!
isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	^false! !


!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
a0
	^self at: 1! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
a1
	^self at: 2! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
a2
	^self at: 3! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
b0
	^self at: 4! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
b1
	^self at: 5! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
b2
	^self at: 6! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
c0
	^self at: 7! !

!Homography methodsFor: 'element access' stamp: 'jmv 8/9/2018 11:22:18'!
c1
	^self at: 8! !

!Homography methodsFor: 'private access' stamp: 'jmv 8/9/2018 11:22:18'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Homography methodsFor: 'private access' stamp: 'jmv 8/9/2018 11:22:18'!
at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!Homography methodsFor: 'converting coordinates' stamp: 'jmv 8/9/2018 11:22:18'!
map: aPoint
	| xx yy zz |
	xx _ (self a0 * aPoint x) + (self a1 * aPoint y) + self a2.
	yy _ (self b0 * aPoint x) + (self b1 * aPoint y) + self b2.
	zz _ (self c0 * aPoint x) + (self c1 * aPoint y) + 1.
	^(xx / zz) @ (yy / zz)! !


!Homography class methodsFor: 'instance creation' stamp: 'jmv 8/9/2018 11:22:18'!
new
	^(self basicNew: 8) initialize! !

!Homography class methodsFor: 'instance creation' stamp: 'jmv 8/9/2018 11:23:34'!
new: s
	self error: 'Please call Homography>>#new (without arguments).'.
	^self new! !


!MorphicTranslation methodsFor: 'comparing' stamp: 'jmv 8/9/2018 11:26:07'!
= aMorphicTranslation
	self == aMorphicTranslation ifTrue: [ ^ true ].
	(aMorphicTranslation is: #GeometryTransformation) ifFalse: [ ^false ].
	aMorphicTranslation isPureTranslation ifFalse: [ ^false ].
	^self translation = aMorphicTranslation translation! !

!methodRemoval: Homography class #map:to:!
Homography class removeSelector: #map:to:!
!classDefinition: #MorphicTranslation category: #'Graphics-Primitives'!
GeometryTransformation subclass: #MorphicTranslation
	instanceVariableNames: 'deltaX deltaY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classRemoval: #MorphicLocation!
Smalltalk removeClassNamed: #MorphicLocation!
