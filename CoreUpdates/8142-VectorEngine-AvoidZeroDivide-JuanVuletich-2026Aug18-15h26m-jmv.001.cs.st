'From Cuis7.9 [latest update: #8143] on 18 August 2026 at 3:37:58 pm'!

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 18/Aug/2026, 6:37:07 pm (UTC)'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 f23 f4 x y t0 x0 y0 length correction |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl1@yControl1. txControl1 := p x. tyControl1 := p y.
	p := geometryTransformation externalizePosition: xControl2@yControl2. txControl2 := p x. tyControl2 := p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	"Case t = 0.0"
	x := txFrom.
	y := tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	dx := (txTo-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	
	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length := ((x-x0)@(y-y0)) r.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
			x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
			length := ((x-x0)@(y-y0)) r.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99]  whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 18/Aug/2026, 6:37:33 pm (UTC)'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	| p txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 x y t0 x0 y0 length correction |

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.

	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl@yControl. txControl := p x. tyControl := p y.

	dx := (txTo-txFrom) abs.
	dx2 := (txControl-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	dy2 := (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop := spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).

	"Case t = 0.0"
	x := txFrom.
	y := tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
		x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length := ((x-x0)@(y-y0)) r.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			length := ((x-x0)@(y-y0)) r.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99]  whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

