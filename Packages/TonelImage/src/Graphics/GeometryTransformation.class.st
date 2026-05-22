"
Superclass of several Geometry Transformations used mainly to specify locations of Morphs.

Morphs specify a coordinate system in which they and their submorphs are expressed. A Morph's coordinate system is defined by a GeometryTransformation that is applied to points in inner space to convert them to points in outer space. Therefore #externalizePosition: is equivalent of #transform: and #internalizePosition: is equivalent to #inverseTransform:
"
Class {
	#name : 'GeometryTransformation',
	#superclass : 'Object',
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
GeometryTransformation class >> identity [
	^MorphicTranslation withTranslation: 0@0
]

{ #category : 'testing' }
GeometryTransformation >> doesMirror [
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	^false
]

{ #category : 'testing' }
GeometryTransformation >> doesNotRotate [
	"Return true if the receiver specifies no rotation."
	^false
]

{ #category : 'modifying' }
GeometryTransformation >> invertingYAxis: mustInvertYAxis [
	"Answer an instance (either the receiver or a new one) with the prescribed behavior on the Y axis:
	- If mustInvertYAxis, the Y axis in inner and outer space point in opposite directions.
	- If mustInvertYAxis is false, the Y axis in inner and outer space point in the same direction (either up or down).
	Senders should always use the returned object, but not assume it is a new one:
		it could also be the receiver itself."

	self doesMirror = mustInvertYAxis ifFalse: [
		^self withCurrentYAxisInverted ].
	^self
]

{ #category : 'testing' }
GeometryTransformation >> is: aSymbol [
	^aSymbol == #GeometryTransformation or: [ super is: aSymbol ]
]

{ #category : 'testing' }
GeometryTransformation >> isPureMirroring [
	"Return true if the receiver specifies no translation, rotation or scaling, and just mirrors the Y axis."

	^ false
]

{ #category : 'testing' }
GeometryTransformation >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."
	^false
]

{ #category : 'modifying' }
GeometryTransformation >> withCurrentYAxisInverted [
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one:
		it could also be the receiver itself."

	self subclassResponsibility
]
