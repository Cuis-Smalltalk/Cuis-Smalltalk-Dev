'From Cuis 5.0 of 7 November 2016 [latest update: #3227] on 4 December 2017 at 3:17:12 pm'!

!Float64Array commentStamp: '<historical>' prior: 0!
FloatArrays store 64bit IEEE floating point numbers, i.e. instances of the Float class.
Some support is included for subclasses in the style of Balloon3D-Math.

Uses the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits. See #floatAt: and #floatAt:put:!


!Workspace methodsFor: 'variable declarations' stamp: 'jmv 12/4/2017 15:15:31'!
nameForObject: object
	"Answer a name suitable for a Workspace variable"
	^ object class name asLowercase , object identityHash asString! !

