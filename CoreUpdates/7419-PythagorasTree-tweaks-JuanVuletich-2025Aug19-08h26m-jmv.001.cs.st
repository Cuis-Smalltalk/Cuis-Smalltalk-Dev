'From Cuis7.5 [latest update: #7419] on 19 August 2025 at 8:27:39 am'!

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 8/19/2025 08:27:27'!
drawTreeOn: aCanvas p1: p1 p2: p2 depth: depth

	| dx dy p3 p4 p5 color |
	
	depth = depthLimit ifTrue: [ ^ self].
	
	color := self color.
	colors ifNotNil: [
		color := colors at: (depth mod: colors size) + 1].
	
	dx := p2 x - (p1 x).
	dy := p1 y - (p2 y).
	
	p3 := (p2 x - dy) @ (p2 y - dx).
	p4 := (p1 x - dy) @ (p1 y - dx).
	p5 := (p4 x + (angle * (dx - dy))) @ (p4 y - (angle * (dx + dy))).
	
	aCanvas fillColor: color do: [
		aCanvas moveTo: p1;
			lineTo: p2;
			lineTo: p3;
			lineTo: p4;
			lineTo: p1].
	
	"Enable to also draw triangles. Disable for a 'true' Pythagoras tree."
	true ifTrue: [
		aCanvas fillColor: color do: [
			aCanvas moveTo:  p3;
				lineTo: p4;
				lineTo: p5;
				lineTo: p3]].
	
	self drawTreeOn: aCanvas p1: p4 p2: p5 depth: depth + 1.
	self drawTreeOn: aCanvas p1: p5 p2: p3 depth: depth + 1.! !

