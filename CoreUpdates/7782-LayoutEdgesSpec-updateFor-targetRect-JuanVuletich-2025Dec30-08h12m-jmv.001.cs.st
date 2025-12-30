'From Cuis7.7 [latest update: #7781] on 30 December 2025 at 8:26:03 am'!

!LayoutEdgesSpec methodsFor: 'updating' stamp: 'jmv 29/Dec/2025 15:44:32'!
updateFor: targetBounds in: availableSpace
	"Update ourselves so as to compute targetBounds if applied to availableSpace."

	self updateHorizontalWeightsAndOffsetsFor: targetBounds in: availableSpace.
	self updateVerticalWeightsAndOffsetsFor: targetBounds in: availableSpace.

	self updateContainingLayout.! !

!LayoutEdgesSpec methodsFor: 'updating' stamp: 'jmv 30/Dec/2025 08:25:30'!
updateHorizontalWeightsAndOffsetsFor: targetBounds in: availableSpace

	| newLeftOffset newLeftWeight newRightOffset newRightWeight |

	((#[0.0 0.5 1.0 ] includes: topLeftEdgesWeight x) or: [topLeftOffset x ~= 0])
		ifTrue: [				"Keep weight, adjust offset."
			newLeftWeight := topLeftEdgesWeight x.
			newLeftOffset := targetBounds left - (availableSpace left + (availableSpace width * newLeftWeight)) ]
		ifFalse: [				"Keep offset, adjust weight."
			newLeftOffset := topLeftOffset x.
			newLeftWeight := (targetBounds left - (availableSpace left + newLeftOffset)) / availableSpace width ].

	(bottomRightEdgesWeight x = topLeftEdgesWeight x)
		ifTrue: [				"Use left weight, adjust offset"
			newRightWeight := newLeftWeight.
			newRightOffset := targetBounds right - (availableSpace left + (availableSpace width * newRightWeight)) ]
		ifFalse: [
			((#[0.0 0.5 1.0 ] includes: bottomRightEdgesWeight x) or: [bottomRightOffset x ~= 0])
				ifTrue: [		"Keep weight, adjust offset"
					newRightWeight := bottomRightEdgesWeight x.
					newRightOffset := targetBounds right - (availableSpace left + (availableSpace width * newRightWeight)) ]
				ifFalse: [		"Keep offset, adjust weight."
					newRightOffset := bottomRightOffset x.
					newRightWeight := (targetBounds right - (availableSpace left + newRightOffset)) / availableSpace width ]].

	topLeftEdgesWeight := newLeftWeight @  topLeftEdgesWeight y.
	topLeftOffset := newLeftOffset @topLeftOffset y.
	bottomRightEdgesWeight := newRightWeight @bottomRightEdgesWeight y.
	bottomRightOffset := newRightOffset @ bottomRightOffset y.! !

!LayoutEdgesSpec methodsFor: 'updating' stamp: 'jmv 29/Dec/2025 16:32:50'!
updateVerticalWeightsAndOffsetsFor: targetBounds in: availableSpace

	| newTopOffset newTopWeight newBottomOffset newBottomWeight |

	((#[0.0 0.5 1.0 ] includes: topLeftEdgesWeight y) or: [topLeftOffset y ~= 0])
		ifTrue: [				"Keep weight, adjust offset."
			newTopWeight := topLeftEdgesWeight y.
			newTopOffset := targetBounds top - (availableSpace top + (availableSpace height * newTopWeight)) ]
		ifFalse: [				"Keep offset, adjust weight."
			newTopOffset := topLeftOffset y.
			newTopWeight := (targetBounds top - (availableSpace top + newTopOffset)) / availableSpace height ].

	(bottomRightEdgesWeight y = topLeftEdgesWeight y)
		ifTrue: [				"Use left weight, adjust offset"
			newBottomWeight := newTopWeight.
			newBottomOffset := targetBounds bottom - (availableSpace top + (availableSpace height * newBottomWeight)) ]
		ifFalse: [
			((#[0.0 0.5 1.0 ] includes: bottomRightEdgesWeight y) or: [bottomRightOffset y ~= 0])
				ifTrue: [		"Keep weight, adjust offset"
					newBottomWeight := bottomRightEdgesWeight y.
					newBottomOffset := targetBounds bottom - (availableSpace top + (availableSpace height * newBottomWeight)) ]
				ifFalse: [		"Keep offset, adjust weight."
					newBottomOffset := bottomRightOffset y.
					newBottomWeight := (targetBounds bottom - (availableSpace top + newBottomOffset)) / availableSpace height ]].

	topLeftEdgesWeight := topLeftEdgesWeight x @ newTopWeight.
	topLeftOffset := topLeftOffset x @ newTopOffset.
	bottomRightEdgesWeight := bottomRightEdgesWeight x @ newBottomWeight.
	bottomRightOffset := bottomRightOffset x @ newBottomOffset.! !


!LayoutEdgesSpec reorganize!
('accessing' bottomRightEdgesWeight bottomRightEdgesWeight: bottomRightOffset bottomRightOffset: topLeftEdgesWeight topLeftEdgesWeight: topLeftOffset topLeftOffset:)
('updating' updateFor:in: updateHorizontalWeightsAndOffsetsFor:in: updateVerticalWeightsAndOffsetsFor:in:)
('layout' minimumSpecExtent)
('testing' isLayoutEdgesSpec)
!

