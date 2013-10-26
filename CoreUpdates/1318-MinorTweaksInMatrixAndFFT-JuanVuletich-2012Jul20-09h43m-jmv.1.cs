'From Cuis 4.0 of 21 April 2012 [latest update: #1317] on 20 July 2012 at 9:43:46 am'!

!FFT methodsFor: 'testing' stamp: 'jmv 7/19/2012 18:04'!
plot: samples in: rect
	"Throw-away code just to check out a couple of examples"
	| min max x dx pen y |
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	min _ 1.0e30.  max _ -1.0e30.
	samples do:
		[:v |
		min _ min min: v.
		max _ max max: v].
	pen _ Pen new.  pen up.
	x _ rect left.
	dx _ rect width asFloat / (samples size-1).
	samples do:
		[:v |
		y _ (max-v) / (max-min) * rect height asFloat.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !

!FFT methodsFor: 'testing' stamp: 'jmv 7/19/2012 18:04'!
plot: samples in: rect color: aColor min: min max: max
	"Throw-away code just to check out a couple of examples"
	| x dx pen y |
	pen _ Pen new.
	pen color: aColor.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / (samples size-1).
	samples do: [ :v |
		y _ (max-v) / (max-min) * rect height asFloat.
		y _ y min: 1000.
		y _ y max: 0.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !


!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 17:41'!
fillEndsWith1Derivative
	"Fill one row and one column at each end with such values that the derivative is that at the ends of the inner submatrix.
	If the size is less than 4 in any direction, meaning we don't have enough elements to compute the derivatives, do nothing.
	Note: We could make derivatives zero, meaning to copy the values."

	| m n third second first antepenultimate penultimate last |
	m _ self m. n _ self n.
	n >= 4 ifTrue: [
		2 to: m-1 do: [ :i |
			second _ self i: i j: 2. third _ self i: i j: 3.
			first _ second * 2-third.
			self i: i j: 1 put: first.
			antepenultimate _ self i: i j: n-2.
			penultimate _ self i: i j: n-1.
			last  _ penultimate * 2 - antepenultimate.
			self i: i j: n put: last ]].
	m >= 4 ifTrue: [
		1 to: n do: [ :j |
			second _ self i: 2 j: j. third _ self i: 3 j: j.
			first _ second * 2-third.
			self i: 1 j: j put: first.
			antepenultimate _ self i: m-2 j: j.
			penultimate _ self i: m-1 j: j.
			last  _ penultimate * 2 - antepenultimate.
			self i: m j: j put: last ]]! !

!Matrix methodsFor: 'accessing' stamp: 'jmv 7/19/2012 17:42'!
fillEndsWith2Derivative2
	"Fill two rows and two columns at each end with such values that the second derivative is that at the ends of the inner submatrix.
	If the size is less than 7 in any direction, meaning we don't have enough elements to compute the derivatives, do nothing.
	Note: We could make derivatives zero, meaning to copy the values."

	| m n third fourth fifth second first fourthToEnd thirdToEnd antepenultimate penultimate last |
	m _ self m. n _ self n.
	n >= 7 ifTrue: [
		3 to: m-2 do: [ :i |
			third _ self i: i j: 3. fourth _ self i: i j: 4. fifth _ self i: i j: 5.
			second _ (third-fourth) * 3.0 + fifth.
			first _ (second-third) * 3.0 + fourth.
			self i: i j: 2 put: second.
			self i: i j: 1 put: first.
			fourthToEnd _ self i: i j: n-4.
			thirdToEnd _ self i: i j: n-3.
			antepenultimate _ self i: i j: n-2.
			penultimate _ (antepenultimate - thirdToEnd) * 3.0 + fourthToEnd.
			last _ (penultimate -antepenultimate) * 3.0 + thirdToEnd.
			self i: i j: n-1 put: penultimate.
			self i: i j: n put: last ]].
	m >= 7 ifTrue: [
		1 to: n do: [ :j |
			third _ self i: 3 j: j. fourth _ self i: 4 j: j. fifth _ self i: 5 j: j.
			second _ (third-fourth) * 3.0 + fifth.
			first _ (second-third) * 3.0 + fourth.
			self i: 2 j: j put: second.
			self i: 1 j: j put: first.
			fourthToEnd _ self i: m-4 j: j.
			thirdToEnd _ self i: m-3 j: j.
			antepenultimate _ self i: m-2 j: j.
			penultimate _ (antepenultimate - thirdToEnd) * 3.0 + fourthToEnd.
			last _ (penultimate -antepenultimate) * 3.0 + thirdToEnd.
			self i: m-1 j: j put: penultimate.
			self i: m j: j put: last ]]! !

