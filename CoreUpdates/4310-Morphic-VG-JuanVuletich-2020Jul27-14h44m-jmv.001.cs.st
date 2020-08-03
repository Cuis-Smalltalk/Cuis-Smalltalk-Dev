'From Cuis 5.0 [latest update: #4306] on 27 July 2020 at 2:48:09 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds '
	classVariableNames: 'ActiveSubclass '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 14:45:07'!
activeSubclass
	ActiveSubclass ifNil: [
		ActiveSubclass _ BitBltCanvas ].
	^ActiveSubclass! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 13:37:39'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 14:47:45'!
subclassToUse
	"If asked to a specific subclass, use that."

	^ self == MorphicCanvas
		ifFalse: [ self ]
		ifTrue: [ self activeSubclass ]! !


!Form methodsFor: 'accessing' stamp: 'jmv 7/27/2020 13:39:10'!
getCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^MorphicCanvas onForm: self! !


!Morph methodsFor: 'drawing' stamp: 'jmv 7/27/2020 13:39:07'!
imageForm: depth
	| canvas |
	canvas _ MorphicCanvas depth: depth over: (self morphPosition extent: self morphExtent).
	canvas fullDraw: self.
	^ canvas form divideByAlpha! !

!Morph methodsFor: 'drawing' stamp: 'jmv 7/27/2020 13:39:04'!
imageForm: extent depth: depth
	| canvas |
	canvas _ MorphicCanvas depth: depth over: (self morphPosition extent: (self morphExtent min: extent)).
	canvas fullDraw: self.
	^ canvas form divideByAlpha! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 7/27/2020 13:39:01'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= world morphExtent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (MorphicCanvas withExtent: world morphExtent depth: Display depth)]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 14:47:53'!
on: aForm over: aRectangle
	"Warning: aForm extent must equal aRectangle extent"

	^ self subclassToUse basicNew
		initializeWith: aForm origin: aRectangle topLeft negated! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 7/27/2020 14:47:56'!
onForm: aForm

	^ self subclassToUse basicNew
		initializeWith: aForm origin: `0@0`! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds'
	classVariableNames: 'ActiveSubclass'
	poolDictionaries: ''
	category: 'Morphic-Support'!
