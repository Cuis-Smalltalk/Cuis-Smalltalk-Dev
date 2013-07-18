'From Cuis 4.1 of 12 December 2012 [latest update: #1636] on 20 March 2013 at 10:47:01 pm'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 3/20/2013 22:34'!
fakeSphereAt: aPoint diameter: diameter color: aColor
	"Create a form which contains a round black dot."
	| bb rect d p |
	bb _ GrafPort toForm: Display.
	bb fillColor: aColor.
	bb combinationRule: Form blend.
	rect _ aPoint extent: diameter.
	bb fillOval: rect.
	p _ (diameter*2//5) asPoint + aPoint..
	#(2 3 4 5 6) do: [ :i |  "simulate facade by circles of gray"
		bb fillColor: (Color white alpha: 0.3).
		d _ i*diameter//10.
		rect _ Rectangle center: p extent: d.
		bb fillOval: rect ].
"
	Form fakeSphereAt: 50@50 diameter: 30 color:  (Color red alpha: 0.5)
"! !


!Form class methodsFor: 'examples' stamp: 'jmv 3/20/2013 22:36'!
toothpaste: diam	
	"
	Display restoreAfter: [Form toothpaste: 30]
	"
	"Not completely unlike the ST-80 implementation :)
	Original comment:
	Draws wormlike lines by laying down images of spheres.
	See Ken Knowlton, Computer Graphics, vol. 15 no. 4 p352.
	Draw with mouse button down; terminate by option-click."
	| point queue color q colors colr colr2 |
	colors _ Color red wheel: 12.
	color _ 8.
	[ true ] whileTrue: [
		queue _ OrderedCollection new: 32.
		16 timesRepeat: [queue addLast: -40@-40].
		Sensor waitButton.
		Sensor isMouseButton2Pressed ifTrue: [^ self].
		point _ Sensor mousePoint.
		colr _ colors atWrap: (color _ color + 5).  "choose increment relatively prime to colors size"
		colr2 _ colr alpha: 0.3.
		[Sensor isMouseButton1Pressed or: [queue size > 0]] whileTrue: [
			point _ point * 4 + Sensor mousePoint // 5.
			point _ point.
			Form fakeSphereAt: point diameter: diam color: colr.
			(q _ queue removeFirst) ifNil: [^ self].	"exit"
			Form fakeSphereAt: q diameter: diam color: colr2.
			Sensor isMouseButton1Pressed ifTrue: [queue addLast: point]]]! !

