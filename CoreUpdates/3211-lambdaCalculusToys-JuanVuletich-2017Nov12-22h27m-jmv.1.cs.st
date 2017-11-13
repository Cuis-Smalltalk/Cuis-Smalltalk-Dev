'From Cuis 5.0 of 7 November 2016 [latest update: #3210] on 12 November 2017 at 10:28:26 pm'!

!BlockClosure methodsFor: 'lambda calculus' stamp: 'jmv 11/12/2017 22:27:40'!
curried
	"
	https://en.wikipedia.org/wiki/Currying
	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] curried value: 1 :: value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ :arg1 | [ self value: arg1 ]]].
		[ 2] -> [[ :arg1 | [ :arg2 | self value: arg1 value: arg2 ]]].
		[ 3] -> [[ :arg1 | [ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]]].
		[ 4] -> [[ :arg1 | [ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]]] }
	otherwise: [ self halt ]! !

!BlockClosure methodsFor: 'lambda calculus' stamp: 'jmv 11/12/2017 22:27:57'!
withFirstArg: arg1
	"
	https://en.wikipedia.org/wiki/Partial_application

	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] withFirstArg: 1
	([ :a :b | a + b ] withFirstArg: 1) value: 2
	([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2
	(([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2) value

	([ :a :b | a - b ] withFirstArg: 1) value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ self value: arg1 ]].
		[ 2] -> [[ :arg2 | self value: arg1 value: arg2 ]].
		[ 3] -> [[ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]].
		[ 4] -> [[ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]] }
	otherwise: [ self halt ]! !

