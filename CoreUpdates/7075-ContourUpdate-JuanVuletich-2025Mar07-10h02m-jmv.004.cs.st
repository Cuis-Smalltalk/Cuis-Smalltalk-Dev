'From Cuis7.3 [latest update: #7066] on 7 March 2025 at 10:03:44 am'!

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/6/2025 17:42:24'!
morphContourAfterDrawInterectInto: aGlobalContour
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or answer a new array if not.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawInto:into:
	See #morphContourAfterDrawUpdate:oldTop:oldBottom:into:
	"

	| y0 y1 dilationHalfHeight strokeHalfSquared |
	y0 := morphBoundsTop max: 0.
	y1 := morphBoundsBottom min: targetHeight -1.
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	y0 to: y1 do: [ :y | | x0 x1 i |
		x0 := targetWidth.
		x1 := 0.
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y + dy between: y0 and: y1) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		i := y*2+1.
		aGlobalContour at: i put: ((aGlobalContour at: i) max: x0).
		aGlobalContour at: i+1 put: ((aGlobalContour at: i+1) min: x1) ].! !


!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/6/2025 17:39:14'!
morphContourAfterDrawInto: contourArrayOrNil into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Use argument if appropriate, or answer a new array if not.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawUpdate:oldTop:oldBottom:into:
	See #morphContourAfterDrawInterectInto:
	"

	| morphContour i requiredSize y0 y1 dilationHalfHeight strokeHalfSquared |
	y0 := morphBoundsTop max: 0.
	y1 := morphBoundsBottom min: targetHeight -1.
	requiredSize := (y1 - y0 + 1) *  2 max: 0.
	morphContour := contourArrayOrNil.
	(morphContour isNil or: [ requiredSize > morphContour size ]) ifTrue: [
		morphContour := IntegerArray new: requiredSize ].
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	i := 1.
	y0 to: y1 do: [ :y | | x0 x1 |
		x0 := targetWidth.
		x1 := 0.
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y + dy between: y0 and: y1) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		morphContour at: i put: x0.
		morphContour at: i+1 put: x1.
		i := i + 2 ].
	i to: morphContour size by: 2 do: [ :ii |
		morphContour at: ii put: targetWidth.
		morphContour at: ii+1 put: 0 ].
	aBlock value: morphContour value: y0 value: y1.! !

!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 3/6/2025 17:34:50'!
morphContourAfterDrawUpdate: oldContour oldTop: oldTop oldBottom: oldBottom into: aBlock
	"In targetForm coordinates.
	Doesn't account for any clipping at all.
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph.
	Dilates contour by the last used strokeWIdth. A future improvement must be,
	when building the contour in the engine, for each point recorded, also record
	the amout it must be dilated by, i.e. the current strokeWidth at that moment.
	See #morphContourAfterDrawInto:into:
	See #morphContourAfterDrawInterectInto:
	"

	| morphContour i ii requiredSize oldContourDelta y0 y1 dilationHalfHeight strokeHalfSquared mbb mbt |
	y0 := (morphBoundsTop min: oldTop) max: 0.
	y1 := (morphBoundsBottom max: oldBottom) min: targetHeight -1.
	requiredSize := (y1 - y0 + 1) *  2.
	morphContour := oldContour.
	oldContourDelta := 0.
	(morphContour size < requiredSize or: [y0 < oldTop] ) ifTrue: [
		oldContourDelta := (oldTop - y0) * 2.
		morphContour := IntegerArray new: requiredSize ].
	dilationHalfHeight := (strokeWidth / 2.0) rounded.
	strokeHalfSquared := (strokeWidth / 2.0) squared.
	i := 1.
	ii := 1-oldContourDelta.
	mbt := morphBoundsTop max: 0.
	mbb := morphBoundsBottom min: targetHeight -1.
	y0 to: y1 do: [ :y | | x0 x1 |
		(ii between: 1 and: oldContour size-1) ifTrue: [
			x0 := oldContour at: ii.
			x1 := oldContour at: ii+1 ]
		ifFalse: [
			x0 := targetWidth.
			x1 := 0 ].
		dilationHalfHeight negated to: dilationHalfHeight do: [ :dy | | dx |
			(y+dy between: mbt and: mbb) ifTrue: [
				dx := (strokeHalfSquared - dy squared max: 0) sqrt.
				x0 := x0 min: ((contour at: y+dy * 2 + 1) - dx) rounded.
				x1 := x1 max: ((contour at: y+dy * 2 + 2) + dx) rounded ]].
		morphContour at: i put: x0.
		morphContour at: i+1 put: x1.
		i := i + 2.
		ii := ii + 2 ].
	i to: morphContour size by: 2 do: [ :iii |
		morphContour at: iii put: targetWidth.
		morphContour at: iii+1 put: 0 ].
	aBlock value: morphContour value: y0 value: y1.! !

