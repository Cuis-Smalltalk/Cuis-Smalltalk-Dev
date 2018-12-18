'From Cuis 5.0 of 7 November 2016 [latest update: #3528] on 18 December 2018 at 12:54:11 pm'!
'Description A collection of general numerical algorithms for continuous problems.'!
!provides: 'NumericalMethods' 1 9!
SystemOrganization addCategory: #NumericalMethods!


!classDefinition: #NelderMeadMethod category: #NumericalMethods!
Object subclass: #NelderMeadMethod
	instanceVariableNames: 'objectiveFunction testPoints epsilon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'NumericalMethods'!
!classDefinition: 'NelderMeadMethod class' category: #NumericalMethods!
NelderMeadMethod class
	instanceVariableNames: ''!


!NelderMeadMethod commentStamp: 'jmv 10/13/2017 12:32:23' prior: 0!
The Nelder-Mead method or downhill simplex method or amoeba method is a commonly applied numerical method used to find the minimum or maximum of an objective function in a multidimensional space. It is applied to nonlinear optimization problems for which derivatives may not be known.

See https://en.wikipedia.org/wiki/Nelder-Mead_method

	- objectiveFunction is the (continuous, unimodal) N-dimensional function to minimize. It takes N variables,
		actually a kind of FloatArray or Float64Array of N elements.
	- testPoints a collection of N+1 points, needed to walk an N-dimensional space
	
At each step, the worst of the testPoints is detected, and replaced with an improvement (a reflection towards the centroid of the testPoints). Upon convergence, all testPoints are very close, essentially at the solution.!

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 7/11/2017 16:26:17'!
epsilon: aNumberOrArray
	"Convergence criteria.
	Stop iterating when error in the answer (i.e. the test points) is believed to be smaller than epsilon.
	epsilon might be a FloatArray if different values are desired for each element of the solution."

	epsilon _ aNumberOrArray! !

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 12/1/2016 12:54:00'!
initialPoint: aPoint distanceForOthers: aNumber
	"aPoint is a kind of FloatArray or Float64Array of size N.
	N is the dimension of the function input, i.e. the number of variables.
	aNumber is a reasonable distance to aPoint for the rest of the initial testPoints."

	| n |
	n _ aPoint size.
	testPoints _ Array new: n+1.
	1 to: n do: [ :i | | p |
		p _ aPoint copy.
		p at: i put: (p at: i) + aNumber.
		testPoints at: i put: p ].
	testPoints at: n+1 put: aPoint! !

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 12/1/2016 12:51:16'!
initialPoints: initialPoints
	"initialPoints is size N+1. Each element is a kind of FloatArray or Float64Array of size N.
	N is the dimension of the function input, i.e. the number of variables.
	Call this method when you do have a set of initial points. Otherwise it is ok to call #initialPoint:distanceForOthers:"

	testPoints _ initialPoints! !

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 10/13/2017 12:32:09'!
objectiveFunction: aBlock
	"Set the function to minimize.
	See class comment"

	objectiveFunction _ aBlock! !

!NelderMeadMethod methodsFor: 'computing' stamp: 'jmv 12/18/2018 11:04:37'!
solve
	"Closely follows implementation at
		https://en.wikipedia.org/wiki/Nelder-Mead_method
	"
	| alpha centroidX0 worstPointXnPlus1 worstValueXnPlus1 bestPointX1 bestValueX1 reflectedPointXr reflectedValueXr sortedIndexes values expandedPointXe expandedValueXe gamma secondWorstValueXn contractedPointXc contractedValueXc rho sigma iters |
	iters _ 0.
	alpha _ 1.
	gamma _ 2.
	rho _ 0.5.
	sigma _ 0.5.
	sortedIndexes _ (1 to: testPoints size) asArray.
	values _ testPoints collect: [ :p | objectiveFunction value: p ].
	[
		"Order"
		sortedIndexes sort: [ :i1 :i2 | (values at: i1) < (values at: i2) ].
		bestPointX1 _ testPoints at: sortedIndexes first.
		bestValueX1 _ values at: sortedIndexes first.
		secondWorstValueXn _ values at: sortedIndexes penultimate.
		worstPointXnPlus1 _ testPoints at: sortedIndexes last.
		worstValueXnPlus1 _ values at: sortedIndexes last.

		"Stopping condition:
			- Reached requested epsilon on answer (i.e. points). Epsilon can be number or array. Each component of answer is compared.
			- or function values are almost the same. The amount of ulps to allow is rather arbitrary... Maybe it can safely be made a lot smaller.
				(this depends on the numerical convergence of objectiveFunction)"
		(((worstPointXnPlus1 - bestPointX1) abs - epsilon allSatisfy: [ :v | v < 0.0 ])
			or: [ bestValueX1 % worstValueXnPlus1 < 1.0e-12])
	] whileFalse: [
	
		"Centroid"
		centroidX0 _ testPoints sum - worstPointXnPlus1 / (testPoints size - 1).

		reflectedPointXr _ centroidX0 + (alpha * (centroidX0 - worstPointXnPlus1)).
		"self assert: (reflectedPointXr+worstPointXnp1/2 - centroidX0) length < 0.0001."
		reflectedValueXr _ objectiveFunction value: reflectedPointXr.
		(bestValueX1 <= reflectedValueXr and: [ reflectedValueXr < secondWorstValueXn ])
			ifTrue: [
				"Reflection"
				testPoints at: sortedIndexes last put: reflectedPointXr.
				values at: sortedIndexes last put: reflectedValueXr ]
			ifFalse: [
				reflectedValueXr < bestValueX1
					ifTrue: [
						"Expansion"
						expandedPointXe _ centroidX0 + (gamma * (reflectedPointXr - centroidX0)).
						expandedValueXe _ objectiveFunction value: expandedPointXe.
						expandedValueXe < reflectedValueXr
							ifTrue: [
								testPoints at: sortedIndexes last put: expandedPointXe.
								values at: sortedIndexes last put: expandedValueXe ]
							ifFalse: [
								testPoints at: sortedIndexes last put: reflectedPointXr.
								values at: sortedIndexes last put: reflectedValueXr ]]
					ifFalse: [ "reflectedValueXr >= secondWorstValueXn"
						"self assert: reflectedValueXr >= secondWorstValueXn."
						contractedPointXc _ centroidX0 + (rho * (worstPointXnPlus1 -centroidX0)).
						contractedValueXc _ objectiveFunction value: contractedPointXc.
						contractedValueXc < worstValueXnPlus1
							ifTrue: [
								"Contraction"
								testPoints at: sortedIndexes last put: contractedPointXc.
								values at: sortedIndexes last put: contractedValueXc ]
							ifFalse: [
								"Shrink"
								2 to: testPoints size do: [ :i | | xi ii |
									ii _ sortedIndexes at: i.
									xi _ testPoints at: ii.
									xi _ bestPointX1 + (sigma * (xi - bestPointX1)).
									testPoints at: ii put: xi.
									values at: ii put: (objectiveFunction value: xi) ]]]
			].
		iters _ iters + 1.
	].
	"Evaluate once more with the best found point.
	This is useful when the objectiveFunction has side effects, such as setting some state according to the best solution found."
	objectiveFunction value: bestPointX1.
	^bestPointX1! !

!NelderMeadMethod class methodsFor: 'examples' stamp: 'jmv 10/13/2017 12:44:44'!
example01
	"
	NelderMeadMethod example01
	See the results of the #print in the Transcript: Once the optimal solution is found, the objectiveFunction is evaluated once more:
	last evaluation is best evaluation, so error variable is correct.
	"
	| algorithm f error answer |
	algorithm _ NelderMeadMethod new.
	f _ [ :aFloatArray |
		error _ (aFloatArray first-1) squared + (aFloatArray second-3) squared + (aFloatArray third-2) abs.
		error print.
		error ].
	algorithm objectiveFunction: f.
	algorithm initialPoint: #[10.0 10.0 10.0] distanceForOthers: 0.5.
	algorithm epsilon: 0.0001.
	answer _ algorithm solve.
	{'error: '. error} print.
	^ answer! !
