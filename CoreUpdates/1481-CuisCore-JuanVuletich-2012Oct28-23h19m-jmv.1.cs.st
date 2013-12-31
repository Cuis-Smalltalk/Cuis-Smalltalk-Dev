'From Cuis 4.0 of 21 April 2012 [latest update: #1480] on 28 October 2012 at 11:49:58 pm'!

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 10/28/2012 23:48'!
externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12).
	y _ (aPoint x * self a21) + (aPoint y * self a22).
	^x @ y! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 10/28/2012 23:49'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x asFloat.
	y _ aPoint y asFloat.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^0@0 ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^(detX * det) @ (detY * det)! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/28/2012 23:49'!
externalizeDistance: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ location externalizeDelta: aPoint! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/28/2012 23:49'!
internalizeDistance: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizeDelta: aPoint! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/28/2012 23:37'!
externalizeDistanceToWorld: aPoint
	"aPoint is a distance in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!!"
	"Add scale factor!!"
	self flag: #jmvVer2.
	^aPoint! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 10/28/2012 23:27'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| h w |
	aWorld addMorph: self.
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: progress morphWidth) + 8.
	h _ labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10.
	self morphExtent: w@h.
	self morphPosition: aWorld morphExtent - extent // 2.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
	aWorld startSteppingSubmorphsOf: self.! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 10/28/2012 23:49'!
step

	"My dimensions are constrained live."
	self morphExtent: (self internalize: self world morphExtent) x @ 18.
	self morphPosition: 0@ (self world morphExtent y - (self externalizeDistance: extent ) y)! !

!methodRemoval: Morph #externalizeDist:!
Morph removeSelector: #externalizeDist:!
!methodRemoval: Morph #internalizeDist:!
Morph removeSelector: #internalizeDist:!
!methodRemoval: MatrixTransform2x3 #externalizeDistance:!
MatrixTransform2x3 removeSelector: #externalizeDistance:!
!methodRemoval: MatrixTransform2x3 #internalizeDistance:!
MatrixTransform2x3 removeSelector: #internalizeDistance:!
