'From Cuis 4.2 of 25 July 2013 [latest update: #2499] on 17 September 2015 at 9:48:25.759967 am'!

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 9/17/2015 09:36'!
formFromStream: aBinaryStream
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form  featureName |

	readerClass _ self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			aBinaryStream close.
			featureName _ 'Graphics-Files-Additional'.
			^(FeatureRequirement name: featureName) isAlreadySatisfied
				ifTrue: [ self error: 'Unsupported image file format.' ]
				ifFalse: [
					self error: 'Unsupported image format. Try "', featureName, '".']].
	reader _ readerClass onBinaryStream: aBinaryStream reset.
	form _ reader nextImage.
	reader close.
	^ form! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 9/17/2015 09:47'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| effectiveExtent width strips height grid allowedArea maxLevel |
	effectiveExtent _ self maximumUsableArea extent
					- (self scrollBarSetback @ self screenTopSetback).
	Preferences reverseWindowStagger ifTrue:
		["NOTE: following copied from strictlyStaggeredInitialFrameFor:"
		allowedArea _ self maximumUsableArea insetBy: (
			self scrollBarSetback @ self screenTopSetback extent: 0@0
		).
		"Number to be staggered at each corner (less on small screens)"
		maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
		"Amount by which to stagger (less on small screens)"
		grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
		^ (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: 52@40 * Preferences standardCodeFont height ].
	width _ (strips _ self windowColumnsDesired) > 1
		ifTrue:
			[effectiveExtent x // strips]
		ifFalse:
			[(3 * effectiveExtent x) // 4].
	height _ (strips _ self windowRowsDesired) > 1
		ifTrue:
			[effectiveExtent y // strips]
		ifFalse:
			[(3 * effectiveExtent y) //4].
	^ width @ height

"RealEstateAgent standardWindowExtent"! !

