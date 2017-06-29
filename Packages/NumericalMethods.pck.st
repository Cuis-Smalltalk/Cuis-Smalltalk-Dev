'From Cuis 5.0 of 7 November 2016 [latest update: #3120] on 23 June 2017 at 3:29:09 pm'!
'Description A collection of general numerical algorithms for continuous problems.'!
!provides: 'NumericalMethods' 1 5!
!classDefinition: #NelderMeadMethod category: #NumericalMethods!
Object subclass: #NelderMeadMethod
	instanceVariableNames: 'f testPoints epsilon'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'NumericalMethods'!
!classDefinition: 'NelderMeadMethod class' category: #NumericalMethods!
NelderMeadMethod class
	instanceVariableNames: ''!


!NelderMeadMethod commentStamp: 'jmv 12/1/2016 12:38:12' prior: 0!
The Nelder-Mead method or downhill simplex method or amoeba method is a commonly applied numerical method used to find the minimum or maximum of an objective function in a multidimensional space. It is applied to nonlinear optimization problems for which derivatives may not be known.

See https://en.wikipedia.org/wiki/Nelder-Mead_method

	- f, the objectiveFunction is the (continuous, unimodal) N-dimensional function to minimize. It takes N variables,
		actually a kind of FloatArray or Float64Array of N elements.
	- testPoints a collection of N+1 points, needed to walk an N-dimensional space
	
At each step, the worst of the testPoints is detected, and replaced with an improvement (a reflection towards the centroid of the testPoints). Upon convergence, all testPoints are very close, essentially at the solution.!

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 12/1/2016 16:12:36'!
epsilon: aNumber
	"Convergence criteria. Stop iterating when error is believed to be smaller than epsilon."

	epsilon _ aNumber! !

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

!NelderMeadMethod methodsFor: 'initialization' stamp: 'jmv 12/1/2016 12:40:09'!
objectiveFunction: aBlock
	"Set the function to minimize.
	See class comment"

	f _ aBlock! !

!NelderMeadMethod methodsFor: 'computing' stamp: 'jmv 6/23/2017 15:02:54'!
solve
	"Closely follows implementation at
		https://en.wikipedia.org/wiki/Nelder-Mead_method
	"
	| alpha centroidX0 worstPointXnPlus1 worstValueXnPlus1 bestPointX1 bestValueX1 reflectedPointXr reflectedValueXr sortedIndexes values expandedPointXe gamma secondWorstValueXn contractedPointXc rho sigma iteration errorEstimation bestErrorEstimation lastImprovement |
	alpha _ 1.
	gamma _ 2.
	rho _ 0.5.
	sigma _ 0.5.
	iteration _ 1.
	bestErrorEstimation _ nil.

	[
		"Order"
		values _ testPoints collect: [ :p | f value: p ].
		sortedIndexes _ (1 to: testPoints size) asArray sort: [ :i1 :i2 | (values at: i1) < (values at: i2) ].
		bestPointX1 _ testPoints at: sortedIndexes first.
		bestValueX1 _ values at: sortedIndexes first.
		secondWorstValueXn _ values at: sortedIndexes penultimate.
		worstPointXnPlus1 _ testPoints at: sortedIndexes last.
		worstValueXnPlus1 _ values at: sortedIndexes last.

		"Stopping condition"
		errorEstimation _ worstValueXnPlus1 - bestValueX1.
		(bestErrorEstimation isNil or: [errorEstimation < bestErrorEstimation]) ifTrue: [
			bestErrorEstimation _ errorEstimation.
			lastImprovement _ iteration ].
		(errorEstimation < epsilon or: [
			"This is just an heuristic and there is risk of stopping too early.
			But if we don't do something like this there is risk that setting a very small epsilon,
			we never reach it due to rounding..."
			iteration - lastImprovement > (testPoints size*4) ])
	] whileFalse: [
	
		"Centroid"
		centroidX0 _ testPoints sum - worstPointXnPlus1 / (testPoints size - 1).

		reflectedPointXr _ centroidX0 + (alpha * (centroidX0 - worstPointXnPlus1)).
		"self assert: (reflectedPointXr+worstPointXnp1/2 - centroidX0) length < 0.0001."
		reflectedValueXr _ f value: reflectedPointXr.
		(bestValueX1 <= reflectedValueXr and: [ reflectedValueXr < secondWorstValueXn ])
			ifTrue: [
				"Reflection"
				testPoints at: sortedIndexes last put: reflectedPointXr ]
			ifFalse: [
				reflectedValueXr < bestValueX1
					ifTrue: [
						"Expansion"
						expandedPointXe _ centroidX0 + (gamma * (reflectedPointXr - centroidX0)).
						(f value: expandedPointXe) < reflectedValueXr
							ifTrue: [
								testPoints at: sortedIndexes last put: expandedPointXe ]
							ifFalse: [
								testPoints at: sortedIndexes last put: reflectedPointXr ]]
					ifFalse: [ "reflectedValueXr >= secondWorstValueXn"
						"self assert: reflectedValueXr >= secondWorstValueXn."
						contractedPointXc _ centroidX0 + (rho * (worstPointXnPlus1 -centroidX0)).
						(f value: contractedPointXc) < worstValueXnPlus1
							ifTrue: [
								"Contraction"
								testPoints at: sortedIndexes last put: contractedPointXc ]
							ifFalse: [
								"Shrink"
								2 to: testPoints size do: [ :i | | xi |
									xi _ testPoints at: i.
									xi _ bestPointX1 + (sigma * (xi - bestPointX1)).
									testPoints at: i put: xi ]]]
			].
		iteration _ iteration + 1.
	].
	^bestPointX1! !

!NelderMeadMethod class methodsFor: 'examples' stamp: 'jmv 12/1/2016 16:52:37'!
example01
	"
	NelderMeadMethod example01
	"
	| algorithm f |
	algorithm _ NelderMeadMethod new.
	f _ [ :aFloatArray | (aFloatArray first-1) squared + (aFloatArray second-3) squared + (aFloatArray third-2) abs ].
	algorithm objectiveFunction: f.
	algorithm initialPoint: #(10.0 10.0 10.0) distanceForOthers: 0.5.
	algorithm epsilon: 0.00001.
	^algorithm solve! !
