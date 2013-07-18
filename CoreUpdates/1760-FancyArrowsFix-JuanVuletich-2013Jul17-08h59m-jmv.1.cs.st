'From Cuis 4.1 of 12 December 2012 [latest update: #1759] on 17 July 2013 at 9:10:00.597427 am'!

!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 7/17/2013 09:07'!
buildArrowWith: insideForm borderForm: borderForm
	| extent translucentForm color insideSpec borderSpec border background alpha|
	"
	Display getCanvas
		image: (FormCanvas buildArrowWith: FormCanvas downInsideForm borderForm: FormCanvas downBorderForm)
		at: 20@20
	Display getCanvas
		image: (FormCanvas buildArrowWith:FormCanvas upInsideForm borderForm: FormCanvas upBorderForm)
		at: 40@20
	"
	extent _ insideForm extent - (1@2).
	translucentForm _ Form extent: insideForm extent depth: 32.
	0 to: extent x-1 do: [ :x |
		0 to: extent y-1 do: [ :y |
			insideSpec _ insideForm colorAt: x@(y+1).
			borderSpec _ borderForm colorAt: x@(y+1).
			insideSpec = (Color r: 0.0 g: 0.0 b: 1.0)
				ifTrue: [ color _ Color transparent ]
				ifFalse: [
					borderSpec = (Color r: 1.0 g: 0.0 b: 0.0)
						ifTrue: [ color _ insideSpec ]
						ifFalse: [
							border _ x < (extent x//2)
								ifTrue: [Color white]
								ifFalse: [Color black].
							background _ borderForm colorAt: extent x@(y+1).
							alpha _ borderSpec red asFloat - background red / (border red - background red).
							color _ border alpha: (alpha min: 1 max: 0) ]].
			translucentForm colorAt: x@y put: color ]].
	^translucentForm! !

